As some of you might have noticed, I corrupted my RSS and Twitter feed last
week. It’s an embarassing and cautionary tale, so it’s worth telling.

First, a word about my setup. I wrote a simple static site generator that
takes my markdown files and compiles them to HTML, inlining them into a
layout file. That’s all that’s on the server, no database or anything like
that. All of the software on my blog can be found
[here](https://github.com/hellerve/blog).

So how do I get to an RSS feed and a Twitter bot from there? Whenever I
publish a post, I run [this
script](https://github.com/hellerve/blog/blob/master/rsser/rsser.py). It
looks at the date when the file was last modified and uses that as the
RSS date. This isn't the best solution, of course, but it’s so simple and
foolproof, and good enough for my setup where I write the posts once and
don’t touch them again after publishing. Or at least that’s what I
thought.

Of course nothing is ever as easy as you think it will be.

The Twitter feed is generated in a similar way. It checks which posts are
younger than a certain number of hours, and posts a notification on Twitter
for my dearly beloved followers.

As detailed in [this PSA](//blog.veitheller.de/PSA_VI.html), I decided to
roll out HTTPS after a few friends of mine asked me to. I wrote the PSA
and then realized that I link to the HTTP version of this blog everywhere.
“I know how to fix this”, I thought to myself, not thinking of the RSS or
Twitter feed generator, and wrote a sed expression to change each occurrence
of “http://blog.veitheller.de” in my blog posts to “//blog.veitheller.de”.
A quick fix, noone will know.

Then I decided to write a quick blog post advertising the change, and posting
it the same day; in less than an hour, I had it written, edited, and published.
I ran through the script, which took unusually long. I suspected that the
Twitter API took a little to respond, that there was a connection error of some
sort. Finally it was done. I looked at Twitter to verify that it had indeed been
published; I saw nearly 70 new tweets.

I knew what had happenend immediately. It’s weird how I hadn’t thought about it
before, but once I saw what had happened, I knew what was wrong in the blink of
an eye: `sed` had touched every file, and my scripts thought they were all new.

I spent a few minutes cleaning up the mess on Twitter. My RSS feed is still not
fixed. Sorry if it affected you; you now know who to blame.
