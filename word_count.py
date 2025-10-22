#!/usr/bin/env python3
# pip install requests beautifulsoup4
import re
import time
from urllib.parse import urljoin, urlparse
import requests
from bs4 import BeautifulSoup

START_URL = "https://blog.veitheller.de/"   # index page
DOMAIN = urlparse(START_URL).netloc
PAUSE_S = 0.6                                # be polite to the server
MAX_INDEX_PAGES = 200                        # safety cap

# Treat words as sequences of letters/digits possibly joined by ' or -
WORD_RE = re.compile(r"[0-9A-Za-zÀ-ÖØ-öø-ÿ]+(?:[-'][0-9A-Za-zÀ-ÖØ-öø-ÿ]+)*", re.UNICODE)

def get(url):
    r = requests.get(url, headers={"User-Agent": "word-count/1.0 (+https://example.org)"}, timeout=20)
    r.raise_for_status()
    return r.text

def find_next_index_page(soup, base_url):
    # Prefer rel="next" (many themes use "next" for "older posts")
    link = soup.find("a", rel=lambda v: v and "next" in v.lower())
    if link and link.get("href"):
        return urljoin(base_url, link["href"])

    # Fallback: anchor with "older" in text (cover en/de variations if needed)
    for a in soup.find_all("a"):
        if a.get("href") and ("older" in a.get_text(strip=True).lower() or "ältere" in a.get_text(strip=True).lower()):
            return urljoin(base_url, a["href"])
    return None

def extract_index_post_links(soup, base_url):
    links = set()

    # Best guess: article headers often hold the canonical post link
    for a in soup.select("article h1 a[href], article h2 a[href]"):
        links.add(urljoin(base_url, a["href"]))

    # Fallback: any link under <article>
    for a in soup.select("article a[href]"):
        links.add(urljoin(base_url, a["href"]))

    # Last resort: any on-domain .html link that isn't obviously a non-post page
    for a in soup.select("a[href]"):
        href = a["href"]
        url = urljoin(base_url, href)
        if urlparse(url).netloc != DOMAIN:
            continue
        if not url.endswith(".html"):
            continue
        low = url.lower()
        if any(bad in low for bad in ("/index.html", "/archives", "/archive", "/tags", "/tag/", "/category", "/categories", "/about")):
            continue
        links.add(url)

    return sorted(links)

def extract_article_text_words(html):
    soup = BeautifulSoup(html, "html.parser")

    # Prefer the main article; otherwise fall back to <main>, then body
    main = soup.select_one("article") or soup.select_one("main") or soup.body
    if main is None:
        main = soup

    # Strip code + obvious non-article chrome
    for sel in ["pre", "code", ".highlight", "[class*=code]", "script", "style", "nav", "header", "footer", "aside"]:
        for el in main.select(sel):
            el.decompose()

    text = " ".join(main.stripped_strings)
    return WORD_RE.findall(text)

def collect_all_post_urls():
    urls = set()
    seen_index_pages = set()

    index_url = START_URL
    for _ in range(MAX_INDEX_PAGES):
        if not index_url or index_url in seen_index_pages:
            break
        seen_index_pages.add(index_url)

        html = get(index_url)
        soup = BeautifulSoup(html, "html.parser")
        for u in extract_index_post_links(soup, index_url):
            urls.add(u)

        next_page = find_next_index_page(soup, index_url)
        index_url = next_page
        time.sleep(PAUSE_S)

    return sorted(urls)

def main():
    print(f"Gathering post URLs from index starting at {START_URL} …")
    posts = collect_all_post_urls()
    print(f"Found {len(posts)} candidate posts.")

    total = 0
    per_post = []

    for i, url in enumerate(posts, 1):
        try:
            html = get(url)
            words = extract_article_text_words(html)
            count = len(words)
            total += count
            per_post.append((url, count))
            print(f"[{i:>3}/{len(posts)}] {count:>6} words — {url}")
            time.sleep(PAUSE_S)
        except Exception as e:
            print(f"[ERR] {url}: {e}")

    print("\n==== Summary ====")
    print(f"Posts counted: {len(per_post)}")
    print(f"Total words (excluding code): {total}")
    if per_post:
        top = sorted(per_post, key=lambda x: x[1], reverse=True)[:10]
        print("\nTop 10 by word count:")
        for url, cnt in top:
            print(f"{cnt:>6}  {url}")

if __name__ == "__main__":
    main()
