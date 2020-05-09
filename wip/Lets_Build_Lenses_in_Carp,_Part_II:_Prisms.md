In a continuation of a video from earlier this week on [lenses](/Lets_Build_Lenses_in_Carp.html)
we’re going to look at how to extend our library to work with prisms, and find
out what prisms are in the process.

If you didn’t watch the first session yet, I highly encourage you to do so
before diving into this video, because we’re going to build on the library we
started there and extend it.

I left my references on the bottom of the page if you want to dig a little
deeper—it’s the same resources I linked to the last time.

<video controls><source src="https://veitheller.de/static/prisms.mp4" type="video/mp4"></video>

[Download link](https://veitheller.de/static/prisms.mp4)

#### References

1. [Profunctor Optics: Modular Data Accessors](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf)
   by **Matthew Pickering et al.**: this is great if you want to learn the real
   theory behind lenses and prisms, and optics in general. It is not great if
   you don’t want to read 50 pages about that topic.
2. [Lenses embody Products, Prisms embody Sums](https://blog.jle.im/entry/lenses-products-prisms-sums.html)
   by **Justin Le**: this blog post is a very detailed and deep dive into
   lenses and prisms, using Haskell as the vehicle. It explains why our
   implementation is theoretically unsound and how to rectify that. It is also
   more advanced than my screencast.
