#!/usr/bin/env python3
import argparse, csv, datetime as dt, hashlib, json, os, re, sys, time, urllib.parse
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests
from bs4 import BeautifulSoup
import feedparser

try:
    from dateutil import parser as dateparse
except Exception:
    dateparse = None

# ---------------- HTTP + cache ----------------
def _cache_path(cache_dir, url):
    h = hashlib.sha256(url.encode("utf-8")).hexdigest()[:16]
    u = urllib.parse.urlparse(url)
    name = (u.path.strip("/").replace("/", "_") or "index") + ("_" + h)
    return os.path.join(cache_dir, name + ".html")

def fetch(url, cache_dir=None, timeout=20):
    headers = {"User-Agent": "blog-scraper/strict-date/1.0"}
    if cache_dir:
        os.makedirs(cache_dir, exist_ok=True)
        cp = _cache_path(cache_dir, url)
        if os.path.exists(cp):
            with open(cp, "rb") as f:
                return f.read()
    r = requests.get(url, headers=headers, timeout=timeout)
    r.raise_for_status()
    content = r.content
    if cache_dir:
        with open(cp, "wb") as f:
            f.write(content)
    return content

# ---------------- helpers ----------------
def norm_host(url):
    u = urllib.parse.urlparse(url)
    host = u.netloc.lower()
    if host.startswith("www."): host = host[4:]
    return host

def is_internal(href, base_host):
    try:
        u = urllib.parse.urlparse(href)
        return (not u.netloc) or (u.netloc.lower().lstrip("www.") == base_host)
    except Exception:
        return False

def slug_from_url(url):
    u = urllib.parse.urlparse(url)
    path = u.path.rstrip("/")
    if not path or path == "/": return "index"
    base = os.path.basename(path)
    if base.endswith(".html"): base = base[:-5]
    s = base.replace(" ", "-")
    s = re.sub(r"[_]+", "-", s)
    s = re.sub(r"[^a-zA-Z0-9\-]+", "-", s).strip("-")
    return s.lower() or "post"

# ---------------- extraction ----------------
REMOVE_SELECTORS = [
    "nav", "header", "footer", "aside", "form", ".nav", ".menu", ".pagination", ".breadcrumbs",
    ".sidebar", ".site-header", ".site-footer", ".post-meta", ".tags", ".share", ".comments"
]

def parse_date_strict_from_body_date(html):
    """Find body .date and parse its *text* into a datetime (no other sources)."""
    soup = BeautifulSoup(html, "lxml") if "lxml" in sys.modules else BeautifulSoup(html, "html.parser")
    body = soup.body or soup
    el = body.select_one(".date")
    if not el:
        return None
    s = el.get_text(" ", strip=True)
    if not s:
        return None
    # single parsing attempt: dateutil if available, else fromisoformat or simple YYYY-MM-DD
    if dateparse:
        try: return dateparse.parse(s)
        except Exception: return None
    try: return dt.datetime.fromisoformat(s)
    except Exception:
        m = re.search(r"\b(20\d{2})[-/](\d{1,2})[-/](\d{1,2})\b", s)
        if m:
            y, mo, d = map(int, m.groups())
            try: return dt.datetime(y, mo, d)
            except Exception: pass
        return None

def extract_content(html, base_url=None):
    soup = BeautifulSoup(html, "lxml") if "lxml" in sys.modules else BeautifulSoup(html, "html.parser")
    for sel in REMOVE_SELECTORS:
        for n in soup.select(sel):
            n.decompose()
    node = soup.find("article") or soup.find("main") or soup.body or soup
    title_tag = node.find("h1") or soup.find("h1") or soup.title
    title = title_tag.get_text(strip=True) if title_tag else (soup.title.get_text(strip=True) if soup.title else slug_from_url(base_url or ""))

    # count code and remove before word count
    code_blocks = 0; code_lines = 0
    for pre in node.find_all("pre"):
        code = pre.find("code") or pre
        cls = " ".join(code.get("class", [])).lower()
        code_blocks += 1
        code_text = code.get_text()
        code_lines += len([line for line in code_text.splitlines() if line.strip()])
        pre.decompose()

    links_int, links_ext = [], []
    base_host = norm_host(base_url) if base_url else ""
    for a in node.find_all("a", href=True):
        href = a["href"].strip()
        if href.startswith("#") or href.startswith("mailto:"): continue
        full = urllib.parse.urljoin(base_url, href) if base_url else href
        if is_internal(full, base_host): links_int.append(full)
        else: links_ext.append(full)

    text = node.get_text(" ", strip=True)
    text = re.sub(r"`[^`]+`", " ", text)
    words = re.findall(r"[A-Za-z0-9äöüÄÖÜß\-]+", text)

    return {
        "title": title,
        "words": len(words),
        "code_blocks": code_blocks,
        "code_lines": code_lines,
        "internal_links": links_int,
        "external_links": links_ext,
    }

# ---------------- URL discovery ----------------
FEED_TYPES = {"application/rss+xml", "application/atom+xml", "application/xml", "text/xml"}

def discover_feed(base_url):
    common = ["feed.xml", "rss.xml", "atom.xml", "index.xml", "feed/", "rss/"]
    for path in common:
        u = urllib.parse.urljoin(base_url.rstrip("/") + "/", path)
        try:
            r = requests.head(u, allow_redirects=True, timeout=10)
            if r.ok and int(r.headers.get("content-length", "1")) > 0:
                return r.url
        except Exception:
            pass
    try:
        html = fetch(base_url)
        soup = BeautifulSoup(html, "html.parser")
        for link in soup.find_all("link", rel=lambda v: v and "alternate" in v):
            type_ = (link.get("type") or "").lower()
            if type_ in FEED_TYPES:
                href = link.get("href")
                if href:
                    return urllib.parse.urljoin(base_url, href)
    except Exception:
        pass
    return None

def parse_feed_urls(feed_url):
    fp = feedparser.parse(feed_url)
    return [e.get("link") for e in fp.entries if e.get("link")]

def crawl_more(base_url, limit=0):
    seen, q = set(), [base_url]; out = set()
    host = norm_host(base_url)
    while q:
        url = q.pop(0)
        if url in seen: continue
        seen.add(url)
        try:
            html = fetch(url)
        except Exception:
            continue
        soup = BeautifulSoup(html, "html.parser")
        for a in soup.find_all("a", href=True):
            href = urllib.parse.urljoin(url, a["href"])
            u = urllib.parse.urlparse(href)
            if norm_host(href) != host: continue
            if u.fragment: href = href.split("#",1)[0]
            if href in seen: continue
            if href.endswith(".html") and not re.search(r"(tag|tags|category|categories|page|archive|index)\.html$", href, re.I):
                out.add(href)
            if (u.path.endswith("/") or u.path.endswith(".html")) and len(q) < 500:
                q.append(href)
        if limit and len(out) >= limit: break
    return sorted(out)

# ---------------- main ----------------
def word_bucket(n, step=500): return (n // step) * step
def iso(d): return d.isoformat() if isinstance(d, dt.datetime) else ""

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="https://blog.veitheller.de")
    ap.add_argument("--feed", default=None)
    ap.add_argument("--out", default="stats")
    ap.add_argument("--crawl", action="store_true")
    ap.add_argument("--crawl-limit", type=int, default=0)
    ap.add_argument("--cache", default=None)
    ap.add_argument("--concurrency", type=int, default=8)
    ap.add_argument("--sleep", type=float, default=0.25)
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)
    base_host = norm_host(args.base)

    # Discover URLs (feed for URLs only; we do NOT use feed dates)
    urls = []
    feed_url = args.feed or discover_feed(args.base)
    if feed_url:
        print(f"Using feed for URL discovery: {feed_url}")
        urls += parse_feed_urls(feed_url)
    if args.crawl:
        print("Crawling for additional posts…")
        try:
            extra = [u for u in crawl_more(args.base, args.crawl_limit) if u not in urls]
            print(f"Discovered {len(extra)} pages.")
            urls += extra
        except Exception as e:
            print(f"Crawl failed: {e}", file=sys.stderr)
    urls = sorted(set(urls))
    print(f"Total unique posts to fetch: {len(urls)}")

    rows = []
    langs_all = Counter()
    edges = Counter()
    yearly_words = defaultdict(int)
    yearly_posts = defaultdict(int)
    yearly_code_lines = defaultdict(int)

    def process(url):
        try:
            html = fetch(url, cache_dir=args.cache)
            time.sleep(args.sleep)
            pub = parse_date_strict_from_body_date(html)
            if not pub:
                print(f"No date (including anyway): {url}", file=sys.stderr)

            c = extract_content(html, base_url=url)
            slug = slug_from_url(url)

            # internal link edges → slugs
            for href in c["internal_links"]:
                tgt = urllib.parse.urljoin(url, href)
                if norm_host(tgt) != base_host: continue
                tgt_slug = slug_from_url(tgt)
                if tgt_slug and tgt_slug != slug:
                    edges[(slug, tgt_slug)] += 1

            # Only count in yearly stats if we have a date
            y = pub.year if pub else ""
            if pub:
                yearly_posts[y] += 1
                yearly_words[y] += c["words"]
                yearly_code_lines[y] += c["code_lines"]


            return {
                "url": url,
                "slug": slug,
                "title": c["title"],
                "series": "",
                "published": iso(pub),
                "updated": "",  # not collected in strict mode
                "year": y,
                "words_text": c["words"],
                "words_total": c["words"],
                "internal_links": len(c["internal_links"]),
                "external_links": len(c["external_links"]),
                "code_blocks": c["code_blocks"],
                "code_lines": c["code_lines"],
                "updated_after_publish": 0,
                "days_to_update": ""
            }
        except Exception as e:
            print(f"Failed {url}: {e}", file=sys.stderr)
            return None

    with ThreadPoolExecutor(max_workers=args.concurrency) as ex:
        for fut in as_completed([ex.submit(process, u) for u in urls]):
            r = fut.result()
            if r: rows.append(r)

    # Write posts.csv
    if rows:
        # Sort by date (empty dates last), then by title
        rows.sort(key=lambda r: (r["published"] or "9999", r["title"]))
        with open(os.path.join(args.out, "posts.csv"), "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
            w.writeheader(); w.writerows(rows)

    # yearly.csv
    with open(os.path.join(args.out, "yearly.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(["year","posts","words","Lines of Code"])
        years = sorted(yearly_posts.keys())
        for y in range(min(years), max(years) + 1):
            w.writerow([y, yearly_posts.get(y, 0), yearly_words.get(y, 0), yearly_code_lines.get(y, 0)])

    # length_hist.csv
    hist = Counter(word_bucket(r["words_text"]) for r in rows)
    with open(os.path.join(args.out, "length_hist.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(["Word Buckets","count"])
        for b in sorted(hist): w.writerow([b, hist[b]])

    # links.csv
    with open(os.path.join(args.out, "links.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(["source","target","weight"])
        for (s,t), wgt in sorted(edges.items(), key=lambda x:(-x[1], x[0][0], x[0][1])): w.writerow([s, t, wgt])

    # link_graph.gexf (optional)
    try:
        from xml.sax.saxutils import escape as xml_escape
        nodes = sorted({r["slug"] for r in rows})
        idx = {n:i for i,n in enumerate(nodes)}
        gexf = ['<?xml version="1.0" encoding="UTF-8"?>',
                '<gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">',
                '<graph mode="static" defaultedgetype="directed">','<nodes>']
        for r in rows:
            gexf.append(f'<node id="{idx[r["slug"]]}" label="{xml_escape(r["title"])}"/>')
        gexf.append('</nodes><edges>')
        eid = 0
        for (s,t), wgt in edges.items():
            gexf.append(f'<edge id="{eid}" source="{idx.get(s,0)}" target="{idx.get(t,0)}" weight="{wgt}"/>'); eid += 1
        gexf.append('</edges></graph></gexf>')
        with open(os.path.join(args.out, "link_graph.gexf"), "w", encoding="utf-8") as f:
            f.write("\n".join(gexf))
    except Exception:
        pass

    # metrics.json
    n = len(rows)
    ws = sorted(r["words_text"] for r in rows)
    gaps = []
    dates = [r["published"] for r in rows if r["published"]]
    dates.sort()
    for i in range(1, len(dates)):
        d0 = dt.datetime.fromisoformat(dates[i-1]); d1 = dt.datetime.fromisoformat(dates[i])
        gaps.append((d1 - d0).days)
    sorted_gaps = sorted(gaps) if gaps else []
    metrics = {
        "posts": n,
        "words_total": sum(ws),
        "median_words": (ws[n//2] if n else 0),
        "p90_words": (ws[int(0.9*(n-1))] if n else 0),
        "max_days_between_posts": (max(gaps) if gaps else None),
        "median_days_between_posts": (sorted_gaps[len(gaps)//2] if gaps else None),
        "mean_days_between_posts": (sum(gaps) // len(gaps) if gaps else None),
        "first_post": (dates[0] if dates else None),
        "last_post": (dates[-1] if dates else None),
    }
    with open(os.path.join(args.out, "metrics.json"), "w", encoding="utf-8") as f:
        json.dump(metrics, f, ensure_ascii=False, indent=2)

    print(f"Wrote stats to {args.out}/")

if __name__ == "__main__":
    main()
