"""
Blog publisher: reads all posts/*.md (with YAML frontmatter), renders HTML for each
post, regenerates index.html, and regenerates feed.rss.

Usage:
    python publish.py            # build everything
    python publish.py <post.md>  # build a single post only (no index/feed update)

Each post must have a YAML frontmatter block with at least:
    ---
    title: "Post Title"
    date: YYYY-MM-DD
    ---
"""

import json
import os
import re
import sys

from email import utils
from datetime import datetime, date, timezone
from xml.sax.saxutils import escape as xml_escape

import pystache
import pypandoc
import yaml


POSTS_DIR = "posts"
OUT_DIR = "out"
SITE_URL = "https://blog.veitheller.de"
EXCERPT_LEN = 155
FM_RE = re.compile(r"\A---\s*\n(.*?)\n---\s*\n", re.DOTALL)


# ---------------------------------------------------------------------------
# Frontmatter parsing
# ---------------------------------------------------------------------------

def split_frontmatter(md):
    m = FM_RE.match(md)
    if not m:
        return {}, md
    meta = yaml.safe_load(m.group(1)) or {}
    return meta, md[m.end():]


# ---------------------------------------------------------------------------
# Post helpers
# ---------------------------------------------------------------------------

def stem(filename):
    """'Some_Post.md' → 'Some_Post'"""
    return os.path.basename(filename)[:-3]


def out_file(filename):
    return stem(filename) + ".html"


def to_date(value):
    """Coerce a frontmatter date value to a Python date."""
    if isinstance(value, date):
        return value
    if isinstance(value, str):
        return date.fromisoformat(value)
    raise ValueError(f"Cannot parse date: {value!r}")


def rfc2822(d):
    """Format a date as RFC 2822 for RSS pubDate."""
    dt = datetime(d.year, d.month, d.day, tzinfo=timezone.utc)
    return utils.format_datetime(dt)


def make_excerpt(body, limit=EXCERPT_LEN):
    """Plain-text excerpt of a markdown body for meta descriptions."""
    text = pypandoc.convert_text(body, "plain", format="md")
    text = re.sub(r"\s+", " ", text).strip()
    if len(text) <= limit:
        return text
    cut = text[:limit].rsplit(" ", 1)[0].rstrip(" .,;:—-")
    return cut + "…"


def build_jsonld(title, description, url, post_date):
    """schema.org BlogPosting as a script-tag-safe JSON string."""
    data = {
        "@context": "https://schema.org",
        "@type": "BlogPosting",
        "headline": title,
        "description": description,
        "url": url,
        "mainEntityOfPage": url,
        "datePublished": post_date.isoformat(),
        "author": {"@type": "Person", "name": "Veit Heller", "url": "https://veitheller.de"},
        "publisher": {"@type": "Person", "name": "Veit Heller", "url": "https://veitheller.de"},
    }
    # Escape "<" so the payload can never break out of the <script> element.
    return json.dumps(data, ensure_ascii=False).replace("<", "\\u003c")


def get_post(filename):
    path = os.path.join(POSTS_DIR, filename)
    with open(path) as f:
        contents = f.read()

    meta, body = split_frontmatter(contents)

    post_date = to_date(meta["date"]) if "date" in meta else date.fromtimestamp(os.stat(path).st_mtime)
    title = meta.get("title") or stem(filename).replace("_", " ")
    description = meta.get("description") or make_excerpt(body)
    url = f"{SITE_URL}/{out_file(filename)}"

    return {
        "filename": filename,
        "out_file": out_file(filename),
        "title": title,
        "description": description,       # meta description / OG / RSS
        "url": url,                       # canonical URL
        "jsonld": build_jsonld(title, description, url, post_date),
        "date": str(post_date),           # YYYY-MM-DD, used in post template
        "date_and_time": rfc2822(post_date),  # RFC 2822, used in RSS
        "post": pypandoc.convert_text(body, "html", format="md"),
        "_date_obj": post_date,
    }


def get_posts():
    today = date.today()
    filenames = [f for f in os.listdir(POSTS_DIR) if f.endswith(".md")]
    posts = [get_post(f) for f in filenames]
    posts = [p for p in posts if p["_date_obj"] <= today]
    posts.sort(key=lambda p: p["_date_obj"], reverse=True)
    return posts


# ---------------------------------------------------------------------------
# Rendering
# ---------------------------------------------------------------------------

def render_post(tpl, post):
    html = pystache.render(tpl, post)
    with open(os.path.join(OUT_DIR, post["out_file"]), "w+") as f:
        f.write(html)


def render_index(tpl, posts):
    html = pystache.render(tpl, {"posts": posts})
    with open(os.path.join(OUT_DIR, "index.html"), "w+") as f:
        f.write(html)


def render_feed(tpl, posts):
    build_date = rfc2822(date.today())
    xml = pystache.render(tpl, {"date": build_date, "posts": posts})
    with open(os.path.join(OUT_DIR, "feed.rss"), "w+") as f:
        f.write(xml)


def render_sitemap(posts):
    entries = [(f"{SITE_URL}/", date.today())]
    entries += [(p["url"], p["_date_obj"]) for p in posts]
    urls = "\n".join(
        f"  <url>\n    <loc>{xml_escape(loc)}</loc>\n"
        f"    <lastmod>{lastmod.isoformat()}</lastmod>\n  </url>"
        for loc, lastmod in entries
    )
    xml = (
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n'
        f"{urls}\n</urlset>\n"
    )
    with open(os.path.join(OUT_DIR, "sitemap.xml"), "w+") as f:
        f.write(xml)


def render_robots():
    txt = f"User-agent: *\nAllow: /\n\nSitemap: {SITE_URL}/sitemap.xml\n"
    with open(os.path.join(OUT_DIR, "robots.txt"), "w+") as f:
        f.write(txt)


# ---------------------------------------------------------------------------
# Entry points
# ---------------------------------------------------------------------------

def build_all():
    os.makedirs(OUT_DIR, exist_ok=True)
    posts = get_posts()
    print(f"Building {len(posts)} posts into {OUT_DIR}/...")

    with open("layout.html") as f:
        post_tpl = f.read()
    for post in posts:
        render_post(post_tpl, post)

    with open("index_layout.html") as f:
        index_tpl = f.read()
    render_index(index_tpl, posts)
    print("  index.html written")

    with open("feed_tpl.rss") as f:
        feed_tpl = f.read()
    render_feed(feed_tpl, posts)
    print("  feed.rss written")

    render_sitemap(posts)
    print("  sitemap.xml written")

    render_robots()
    print("  robots.txt written")


def build_one(target):
    """Render a single post file (path or bare filename inside posts/)."""
    os.makedirs(OUT_DIR, exist_ok=True)
    filename = os.path.basename(target)
    if not filename.endswith(".md"):
        filename += ".md"
    post = get_post(filename)

    with open("layout.html") as f:
        tpl = f.read()
    render_post(tpl, post)
    print(f"  {OUT_DIR}/{post['out_file']} written")


if __name__ == "__main__":
    if len(sys.argv) == 1:
        build_all()
    elif len(sys.argv) == 2:
        build_one(sys.argv[1])
    else:
        print(__doc__)
        sys.exit(1)
