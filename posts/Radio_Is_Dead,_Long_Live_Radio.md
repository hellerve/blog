This post will be fairly quick, just a shout-out to a project I’ve been
fond of for quite some time.

Some of you might know [Radio4000](http://radio4000.com/), a website for
Youtube-based radio players. I’ve had a fairly active radio there for
quite some time now and really enjoy the site. They now [introduced](https://blog.radio4000.com/post/166686068942/radio4000-player-embed-with-iframe)
an embeddable version of the radio player. Here’s mine:

<iframe src="https://api.radio4000.com/embed?slug=artisanal-cheeseburgers" width="100%" height="500" frameborder="0"></iframe>
<div class="figure-label">Fig. 1: The best radio in the world, curated by yours truly.</div>

It’s really easy to embed. The nice folks at [internet4000](https://internet4000.com/)
built [a tool](https://github.com/internet4000/radio4000-player) that lets you
create an embeddable player pretty easily. They really did a great job on that
front, but that shouldn’t surprise anyone who is familiar with their work.
And of course it is open source! This is all the code you need:

```
<iframe src="https://api.radio4000.com/embed?slug=<name>"
        width="320"
        height="500"
        frameborder="0">
</iframe>

<!-- alternatively -->

<script async src="https://unpkg.com/radio4000-player"></script>
<radio4000-player channel-slug="<name>"></radio4000-player>
```
<div class="figure-label">
  Fig. 2: All of the scaffolding we need to embed our truly great playlists.
</div>

I’m continually astounded by the great UI/UX work of theirs, and quite
enjoy the products they build. Definitely a crew to look out for.

I hope you have a good time looking through all of the great channels
on Radio4000! Have a great day!
