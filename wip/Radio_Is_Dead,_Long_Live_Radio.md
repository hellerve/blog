Some of you might know [Radio4000](http://radio4000.com/), a website for
Youtube-based radio players. I've had a mostly abandoned radio there for
quite some time now and quite enjoy the site. They now introduced an
[embeddable version of the radio player](https://github.com/Internet4000/radio4000-player-vue).
Here's mine:

<script async src="https://rawgit.com/Internet4000/radio4000-player-vue/master/dist/radio4000-player.min.js"></script>
<div id="radio4000-player" slug="artisanal-cheeseburgers"></div>

It's really easy to embed. Here is all the code you need:

```
<script src="<path to js>/radio4000-player.min.js">
</script>
<div id="radio4000-player" slug="<radio slug>"></div>
```

Of course you can also use [rawgit](). In that case the URL would be
`https://rawgit.com/Internet4000/radio4000-player-vue/master/dist/radio4000-player.min.js`.
Have fun!
