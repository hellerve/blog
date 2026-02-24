"""
One-time migration: convert lepiter/*.lepiter files to posts/*.md with YAML frontmatter.

Skips any lepiter post that already has a matching posts/*.md file (by title slug).
"""

import json
import os
import glob
import re


POSTS_DIR = "posts"

LANG_MAP = {
    "pythonSnippet": "python",
    "elixirSnippet": "elixir",
    "pharoSnippet": "smalltalk",
    "javascriptSnippet": "javascript",
    "codeSnippet": "",
}


def clean_code(code):
    """Lepiter uses \r as line separator in code snippets."""
    return code.replace("\r\n", "\n").replace("\r", "\n")


def snippet_to_md(item):
    t = item.get("__type", "")

    if t == "textSnippet":
        text = item.get("string", "")
        # Lepiter uses \r as line separator within a text snippet
        lines = text.replace("\r\n", "\r").replace("\n", "\r").split("\r")
        # Ensure a blank line before any run of list items that immediately
        # follows non-list text (otherwise pandoc folds them into a <p>)
        result = []
        LIST_RE = re.compile(r"^\s*[-*+] |^\s*\d+[.)]\s")
        for i, line in enumerate(lines):
            if i > 0 and LIST_RE.match(line) and result and result[-1].strip() and not LIST_RE.match(result[-1]):
                result.append("")  # blank line before first list item
            result.append(line)
        return "\n".join(result)

    if t in LANG_MAP:
        lang = LANG_MAP[t]
        code = clean_code(item.get("code", ""))
        return f"```{lang}\n{code}\n```"

    if t == "pictureSnippet":
        url = item.get("url", "")
        caption = item.get("caption", "")
        if caption:
            return f"![{caption}]({url})\n*{caption}*"
        return f"![]({url})"

    if t == "youtubeSnippet":
        url = item.get("youtubeUrl", "")
        return f"[Watch on YouTube]({url})"

    # Unknown snippet type: skip silently
    return ""


def title_to_slug(title):
    """Convert a page title to a filename slug (same convention as blog posts)."""
    # Replace special chars with underscore, preserve colons-with-space as colon
    slug = title.replace(": ", ":_").replace(" ", "_")
    return slug


def existing_post_slugs():
    slugs = set()
    for f in os.listdir(POSTS_DIR):
        if f.endswith(".md"):
            slugs.add(f[:-3].lower())
    return slugs


def convert_lepiter(path, existing):
    with open(path) as f:
        data = json.load(f)

    title = data.get("pageType", {}).get("title", "")
    if not title:
        print(f"  Skipping {path}: no title")
        return

    slug = title_to_slug(title)
    if slug.lower() in existing:
        print(f"  Skipping '{title}': already exists as posts/{slug}.md")
        return

    date_str = data.get("createTime", {}).get("time", {}).get("dateAndTimeString", "")
    date = date_str[:10] if date_str else ""

    items = data.get("children", {}).get("items", [])
    parts = [snippet_to_md(item) for item in items]
    # Filter out empty parts and join with double newlines
    body = "\n\n".join(p for p in parts if p.strip())

    # Escape double quotes in title for YAML
    title_yaml = title.replace('"', '\\"')
    frontmatter = f'---\ntitle: "{title_yaml}"\ndate: {date}\n---\n'

    out_path = os.path.join(POSTS_DIR, slug + ".md")
    with open(out_path, "w") as f:
        f.write(frontmatter + "\n" + body + "\n")

    print(f"  Written: {out_path}")


def main():
    existing = existing_post_slugs()
    lepiter_files = sorted(glob.glob("lepiter/*.lepiter"))
    print(f"Found {len(lepiter_files)} lepiter files, {len(existing)} existing posts\n")
    for path in lepiter_files:
        convert_lepiter(path, existing)


if __name__ == "__main__":
    main()
