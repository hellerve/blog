import os

from datetime  import datetime
from xml.etree import ElementTree as E
from xml.dom   import minidom

import config


def read_index():
    tree = E.parse(config.index)

    for post in tree.findall(".//li[@class='post']"):
        yield post[0].text, post[0].attrib["href"][1:] # trim leading /


def post_elem(name, stats, url, idx):
    elem = E.Element("item")

    subelems = {
        "title": name,
        "description": name,
        "link": "{}/{}".format(config.blog, url),
        "pubDate": str(datetime.fromtimestamp(stats.st_mtime)),
        "guid": str(idx),
    }

    for name, val in subelems.items():
        subelem = E.SubElement(elem, name)
        subelem.text = val

    return elem


def renumerate(l):
    """
    Reverse index enumeration.
    We have to force it to be a list, though, bummer.
    """
    l   = list(l)
    idx = len(l) - 1

    for elem in l:
        yield idx, elem
        idx -= 1


def build(posts):
    tree = E.parse(config.feed_tpl)
    channel = tree.find("channel")
    channel.append(E.Element('lastBuildDate', text=str(datetime.now())))

    for idx, (name, url) in renumerate(posts):
        stats = os.stat(url)
        channel.append(post_elem(name, stats, url, idx))

    return tree


def prettyprint(tree):
    string = E.tostring(tree.getroot(), 'utf-8')
    parsed = minidom.parseString(string)

    with open(config.feed, "w+") as f:
        f.write(parsed.toprettyxml(indent="\t"))


def main():
    posts = read_index()

    tree = build(posts)

    prettyprint(tree)


if __name__ == "__main__":
    main()
