#!/usr/bin/env python3
"""Honest word/code analysis of the published blog, straight from source.

Reads posts/*.md (same set publish.py builds: frontmatter date <= today) and,
for each post, strips frontmatter, fenced + inline code, images and HTML tags
before counting words. So "prose" is what a human actually reads, and "code"
is tracked separately rather than silently inflating the total.

    python word_count.py            # full report
    python word_count.py --csv      # per-post table as CSV on stdout

No third-party dependencies.
"""
import csv
import os
import re
import statistics
import sys
from datetime import date

POSTS_DIR = "posts"

FM_RE    = re.compile(r"\A---\s*\n(.*?)\n---\s*\n", re.DOTALL)
DATE_RE  = re.compile(r"^date:\s*(\d{4}-\d{2}-\d{2})", re.MULTILINE)
TITLE_RE = re.compile(r'^title:\s*"?(.*?)"?\s*$', re.MULTILINE)
FENCE_RE = re.compile(r"^\s*(`{3,}|~{3,})")
WORD_RE  = re.compile(r"[0-9A-Za-zÀ-ɏ]+(?:[-'’][0-9A-Za-zÀ-ɏ]+)*")
INLINE_CODE_RE = re.compile(r"`[^`\n]+`")
MD_IMG_RE  = re.compile(r"!\[[^\]]*\]\([^)]*\)")
MD_LINK_RE = re.compile(r"\[([^\]]*)\]\([^)]*\)")
HTML_TAG_RE = re.compile(r"<[^>]+>")


def split_frontmatter(md):
    m = FM_RE.match(md)
    return (md[:m.end()], md[m.end():]) if m else ("", md)


def analyze(path):
    with open(path, encoding="utf-8") as f:
        raw = f.read()
    fm, body = split_frontmatter(raw)
    dm, tm = DATE_RE.search(fm), TITLE_RE.search(fm)
    d = date.fromisoformat(dm.group(1)) if dm else None
    title = tm.group(1) if tm else os.path.basename(path)[:-3].replace("_", " ")

    # Separate fenced code blocks from prose, line by line.
    prose_lines, code_lines = [], []
    in_fence, fence_char = False, ""
    for line in body.splitlines():
        fence = FENCE_RE.match(line)
        if not in_fence and fence:
            in_fence, fence_char = True, fence.group(1)[0]
        elif in_fence and fence and fence.group(1)[0] == fence_char:
            in_fence = False
        elif in_fence:
            code_lines.append(line)
        else:
            prose_lines.append(line)

    prose = "\n".join(prose_lines)

    # Inline code counts as code, not prose.
    inline_spans = INLINE_CODE_RE.findall(prose)
    inline_code_words = sum(len(WORD_RE.findall(s)) for s in inline_spans)
    prose = INLINE_CODE_RE.sub(" ", prose)

    # Drop images, keep link text (drop URLs), strip remaining HTML.
    prose = MD_IMG_RE.sub(" ", prose)
    prose = MD_LINK_RE.sub(r"\1", prose)
    prose = HTML_TAG_RE.sub(" ", prose)

    return {
        "title": title,
        "date": d,
        "file": os.path.basename(path),
        "prose_words": len(WORD_RE.findall(prose)),
        "code_lines": len(code_lines),
        "code_words": sum(len(WORD_RE.findall(l)) for l in code_lines),
        "inline_code_words": inline_code_words,
    }


def load_posts(today):
    posts = []
    for fn in os.listdir(POSTS_DIR):
        if not fn.endswith(".md"):
            continue
        a = analyze(os.path.join(POSTS_DIR, fn))
        if a["date"] is None or a["date"] <= today:
            posts.append(a)
    posts.sort(key=lambda p: (p["date"] or date.min), reverse=True)
    return posts


def emit_csv(posts):
    w = csv.writer(sys.stdout)
    w.writerow(["date", "title", "prose_words", "code_lines", "code_words", "inline_code_words", "file"])
    for p in posts:
        w.writerow([p["date"], p["title"], p["prose_words"], p["code_lines"],
                    p["code_words"], p["inline_code_words"], p["file"]])


def report(posts):
    n = len(posts)
    prose = [p["prose_words"] for p in posts]
    total_prose = sum(prose)
    total_code_lines = sum(p["code_lines"] for p in posts)
    total_code_words = sum(p["code_words"] for p in posts)
    total_inline = sum(p["inline_code_words"] for p in posts)
    with_code = [p for p in posts if p["code_lines"] > 0]
    dates = [p["date"] for p in posts if p["date"]]

    print(f"Published posts:            {n}")
    print(f"Date range:                 {min(dates)}  ->  {max(dates)}")
    print()
    print("== PROSE (code / images / html / frontmatter excluded) ==")
    print(f"Total prose words:          {total_prose:,}")
    print(f"Average per post:           {statistics.mean(prose):,.0f}")
    print(f"Median per post:            {statistics.median(prose):,.0f}")
    print(f"Std dev:                    {statistics.pstdev(prose):,.0f}")
    print(f"Shortest / Longest:         {min(prose):,} / {max(prose):,}")
    print(f"Est. reading time @200wpm:  {total_prose/200/60:.1f} hours total")
    print()
    print("== CODE ==")
    print(f"Posts containing code:      {len(with_code)} of {n} ({len(with_code)/n*100:.0f}%)")
    print(f"Total fenced code lines:    {total_code_lines:,}")
    print(f"Total code words (fenced):  {total_code_words:,}")
    print(f"Inline code words:          {total_inline:,}")
    if with_code:
        print(f"Avg code lines / post:      {total_code_lines/n:.1f} (all)   "
              f"{total_code_lines/len(with_code):.1f} (code posts only)")
    print()
    print("== TOP 15 LONGEST (prose words) ==")
    for p in sorted(posts, key=lambda p: p["prose_words"], reverse=True)[:15]:
        print(f"{p['prose_words']:>6}  {p['date']}  {p['title']}")
    print()
    print("== 10 SHORTEST ==")
    for p in sorted(posts, key=lambda p: p["prose_words"])[:10]:
        print(f"{p['prose_words']:>6}  {p['date']}  {p['title']}")
    print()
    print("== TOP 15 MOST CODE (fenced lines) ==")
    for p in sorted(posts, key=lambda p: p["code_lines"], reverse=True)[:15]:
        print(f"{p['code_lines']:>6} lines  {p['prose_words']:>5} words  {p['title']}")
    print()
    buckets = [(0, 300, "micro (<300)"), (300, 700, "short (300-700)"),
               (700, 1500, "medium (700-1500)"), (1500, 3000, "long (1500-3000)"),
               (3000, 10**9, "epic (3000+)")]
    print("== LENGTH DISTRIBUTION ==")
    for lo, hi, name in buckets:
        c = sum(1 for w in prose if lo <= w < hi)
        print(f"{name:<20} {c:>4}  {'#'*c}")
    print()
    print("== BY YEAR ==")
    years = {}
    for p in posts:
        if p["date"]:
            years.setdefault(p["date"].year, []).append(p)
    for y in sorted(years):
        ps = years[y]
        w = sum(x["prose_words"] for x in ps)
        cl = sum(x["code_lines"] for x in ps)
        print(f"{y}: {len(ps):>3} posts  {w:>7,} words  {w//len(ps):>5} avg  {cl:>5} code lines")


def main():
    today = date.today()
    posts = load_posts(today)
    if "--csv" in sys.argv[1:]:
        emit_csv(posts)
    else:
        report(posts)


if __name__ == "__main__":
    main()
