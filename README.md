# blog

The articles on my personal blog and the tools that power them.

This repository includes a [dead-simple static site generator](https://github.com/hellerve/blog/blob/master/publish.py)
written in Python, a [distraction-free WYSIWYG Markdown editor](https://github.com/hellerve/blog/tree/master/blargl)
written in Elm, a [RSS-feed generator (from `index.html`)](https://github.com/hellerve/blog/tree/master/rsser), and
a system that mostly supercedes all of that written in Pharo Smalltalk
for [Glamorous Toolkit](https://gtoolkit.com).
The generators are tailored to my specific needs, but the Markdown
editor could be useful for more people. [See it in action here](http://blog.veitheller.de/blargl).

The software is unlicensed and thus you’re free to do whatever you want with
it. The posts themselves are not subject to this license, and you are not
free to reuse them in any context unless given my explicit permission.
## Installation```Metacello new	repository: 'github://hellerve/blog:master/src';	baseline: 'VhBlog';	load.
#BaselineOfVhBlog asClass loadLepiter```