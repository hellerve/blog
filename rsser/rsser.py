import os

from datetime  import datetime
from xml.etree import ElementTree as E
from xml.dom   import minidom

import config


def read_index():
    tree = E.parse(config.index)

    for post in tree.findall(".//li[@id='post']"):
        yield post[0].text, post[0].attrib["href"][1:] # trim leading /

# ToDo: That's dumb
def post_elem(name, stats, url, idx):

    elem = E.Element("item")

    title = E.Element("title")
    title.text = name
    elem.append(title)

    description = E.Element("description")
    description.text = name
    elem.append(description)

    link = E.Element("link")
    link.text = "{}{}".format(config.blog, url)
    elem.append(link)

    pub_date = E.Element("pubDate")
    pub_date.text = str(datetime.fromtimestamp(stats.st_mtime))
    elem.append(pub_date)

    guid = E.Element("guid")
    guid.text = str(idx)
    elem.append(guid)

    return elem


def build(posts):
    tree = E.parse(config.feed_tpl)
    channel = tree.find("channel")
    channel.append(E.Element('lastBuildDate', text=str(datetime.now())))

    for idx, (name, url) in enumerate(posts):
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
