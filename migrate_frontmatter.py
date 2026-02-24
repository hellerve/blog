"""
One-time migration: add YAML frontmatter to all posts/*.md that don't already have it.

Date is taken from git log (first commit date for the file), falling back to mtime.
Title is derived from the filename.
"""

import os
import re
import subprocess
from datetime import datetime


POSTS_DIR = "posts"
FM_RE = re.compile(r"\A---\s*\n", re.DOTALL)


def has_frontmatter(contents):
    return bool(FM_RE.match(contents))


def filename_to_title(filename):
    """posts/Some_Post_Title.md → 'Some Post Title'"""
    stem = filename[:-3]  # strip .md
    return stem.replace("_", " ")


def get_all_git_dates():
    """
    Run one git log pass to get the first-add date for every posts/*.md file.
    Returns dict: filename (basename) → YYYY-MM-DD
    """
    result = subprocess.run(
        ["git", "log", "--diff-filter=A", "--format=DATE:%ai", "--name-only", "--", "posts/*.md"],
        capture_output=True, text=True
    )
    dates = {}
    current_date = None
    for line in result.stdout.splitlines():
        if line.startswith("DATE:"):
            current_date = line[5:].split()[0]  # take YYYY-MM-DD part
        elif line.startswith("posts/") and current_date:
            basename = os.path.basename(line.strip())
            # Keep iterating; git log is newest-first, so last seen = oldest/first commit
            dates[basename] = current_date
    return dates


def mtime_date(filepath):
    ts = os.stat(filepath).st_mtime
    return datetime.fromtimestamp(ts).strftime("%Y-%m-%d")


def add_frontmatter(filepath, git_dates):
    with open(filepath) as f:
        contents = f.read()

    if has_frontmatter(contents):
        return  # already done (e.g. lepiter-migrated posts)

    filename = os.path.basename(filepath)
    title = filename_to_title(filename)
    title_yaml = title.replace('"', '\\"')

    date = git_dates.get(filename) or mtime_date(filepath)

    frontmatter = f'---\ntitle: "{title_yaml}"\ndate: {date}\n---\n\n'
    with open(filepath, "w") as f:
        f.write(frontmatter + contents)

    print(f"  {filename}: {date}")


def main():
    print("Collecting git history (one pass)...")
    git_dates = get_all_git_dates()
    print(f"  Found git dates for {len(git_dates)} files\n")

    posts = sorted(os.listdir(POSTS_DIR))
    md_posts = [p for p in posts if p.endswith(".md")]
    print(f"Processing {len(md_posts)} posts...\n")
    for filename in md_posts:
        add_frontmatter(os.path.join(POSTS_DIR, filename), git_dates)
    print(f"\nDone.")


if __name__ == "__main__":
    main()
