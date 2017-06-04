I had an epiphany a while back. It was one of those observations that seems
obvious in hindsight, one I think I knew on a subconscious level. Pinning it
down and formulating it in actual words helped make it more of an actionable
piece of my philosophy, though, and I hope that sharing it will further solidify
my stance on an important issue: abstractions.

So, here it is: `Abstractions are ideas, not implementations`. Seems like a
platitude, right? But the longer I think about it, the more convinced I am that
there is more to it, an important lesson to be learned.

## You cannot kill an idea

Ideas are much more powerful than implementations. While implementations can
serve as an important illustration for an idea—and thus can influence the
adoption of an idea, no matter how beautiful or misguided it might be—, ideas
survive implementation cycles. This is not always true: if an idea is tightly
coupled with an implementation, they might stand or fall together. But,
generally speaking, ideas are more timeless.

More importantly, you can apply ideas in different environments; implementations
on the other hand are always tied to their environment. This can be seen time
and time again in software development: framework X falls out of favor because
of its flaws, but this frees the underlying idea from the burden of being only
seen through the lens of X. We can now objectively decide whether the flaw was
merely in the implementation of its underlying idea, or whether the idea itself
falls short in certain respects. Then we can reimplement the idea—or not.

Ideas are also much easier to reason through. Without the burden of reality, we
can think about the properties that really make up a system, without having to
wade through all of the implicit decisions that have been made while forming an
artifact from an idea. “Is this a good piece of software?” becomes “is this idea
elegant?”. Then, of course, we will have to worry about whether the idea even
translates to reality, but for now I leave this as an exercise to the reader.

## Abstractions as ideas

Thus, when we think of abstractions as ideas, rather than design decisions that
make our implementations more manageable, we gain all of these nice properties.
If we make implicit decisions explicit, we gain the power to reason about them.

In the context of abstractions this means that we have to think of our
abstractions as maps that simplify the territory rather than glue that holds our
components together. As soon as we do that, the question of what a good
abstraction provides becomes clear: it's a good map for us to navigate. It
provides exactly the information we need. If it provides too much information,
we have to manually strip the information we don't need away—or ignore it, if we
can. If it provides too little information, we have to either rewrite the map,
or juxtapose the information with another map, which is as tedious as it sounds.

## Make it actionable

I am aware that this has been fairly hand-wavy, so let me tell you what concrete
take-away I've gotten from thinking about this. You are free to disagree and shout
at me on the internet, of course.

When I create my next set of abstractions—which really is the only thing I do as
a software developer—I will think of it as a map over a territory. I will think
about whether all of these valleys and peaks are needed or whether they are just
embellishment. I will think about whether I should really hide that river on the
map, lest my users fall into it. I will think about who my map is for, what
my map represents, and communicate that as clearly as possible.

I will tell my readers why I hid what I hid, and why I've shown what is there to
see; and maybe they will see that the map is good, or write their own. Because
if it is just glue, all you can do is stick things together, and if it doesn't
work, they are left to their own devices, and will have to come up with their
own glue. If it's a map, you can see what you cannot see and you know where its
boundaries are.
