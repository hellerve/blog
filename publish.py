import os
import sys

from datetime import date

import pystache
import pypandoc


def from_path(f):
    return os.path.basename(f)[:-3]


def out_file(f):
    return "{}.html".format(from_path(f))


def title(f):
    return from_path(f).replace("_", " ")


def render(tpl, target):
    with open(target) as f:
        contents = f.read()

    args = {
        "post": pypandoc.convert_text(contents, "html", format="md"),
        "title": title(target),
        "date": date.today().strftime("%m/%d/%Y"),
    }

    with open(out_file(target), "w+") as f:
        f.write(pystache.render(tpl, args))


if __name__ == '__main__':
    with open('layout.html') as f:
        tpl = f.read()

    if len(sys.argv) != 2:
        print("Please provide a target to render")
    else:
        render(tpl, sys.argv[1])
