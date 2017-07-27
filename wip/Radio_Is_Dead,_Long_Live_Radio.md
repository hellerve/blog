This post will be fairly quick, just a shout-out to a project I’ve been
fond of for quite some time.

Some of you might know [Radio4000](http://radio4000.com/), a website for
Youtube-based radio players. I’ve had a fairly active radio there for
quite some time now and really enjoy the site. They now introduced an
embeddable version of the radio player. Here’s mine:

<script async src="https://unpkg.com/radio4000-player"></script>
<radio4000-player channel-slug="artisanal-cheeseburgers"></radio4000-player>
<div class="figure-label">Fig. 1: The best radio in the world, curated by yours truly.</div>

It’s really easy to embed. The nice folks at [internet4000](https://internet4000.com/)
built [a tool](https://github.com/internet4000/radio4000-player) that lets you
create an embeddable player pretty easily. They really did a great job on that
front, but that shouldn’t surprise anyone who is familiar with their work.
This is all the code you need:

```
<script async src="https://unpkg.com/radio4000-player">
</script>
<radio4000-player channel-slug="{your channel here}">
</radio4000-player>
```
<div class="figure-label">
  Fig. 2: All of the scaffolding we need to embed our truly great playlists.
</div>

I’m continually astounded by the great UI/UX work of theirs, and quite
enjoy the products they build. Definitely a crew to look out for.

I hope you have a good time looking through all of the great channels
on Radio4000! Have a great day!
