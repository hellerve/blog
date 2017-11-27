First, some news: I’m currently at the Recurse Center again. Well, kind of. RC
launched a little experiment called “popups” a short while ago in which they
organize little—i.e. two weeks long—thematically grouped RC-like retreats. I’m
part of the “generative art” popup in Berlin, and today I want to talk about my
first project: my wedding ring.

My fiancée wished for a generatively designed ring, based either on lichen or
branches as seen in [space colonization
algorithms](http://algorithmicbotany.org/papers/colonization.egwnp2007.large.pdf).
I decided to first make a proof of concept, or rather a sketch, using
Processing, before eventually thinking about how to create a model that I can
3D print or have 3D printed in metal. This is a tale of half a day spent on
something that resembles a ring but probably won’t look very much like the
finished product. You can check out the—very, very rough—code
[here](https://github.com/hellerve/ring)—caveat lector!

## Space Colonization

Cool word, huh? Space colonization is a simple algorithm that models branch
growth based on the notion of branches and leaves only. Leaves are not what you
might expect, though. In this algorithm, leaves are points in space that
parameterize growth, in that branches always grow towards the nearest leaf. My
personal intuition is based on “food sources”, where the branch grows towards
certain environmental points with favorable conditions.

![](/assets/ring.gif)
<div class="figure-label">Fig. 1: A ~~donut~~ ring.</div>

So far, so boring. Don’t get me wrong, the algorithm itself is definitely
exciting, but it has been done a lot of times before: my version is based on an
old [Coding Train episode](https://www.youtube.com/watch?v=kKT0v3qhIQY), in
which Dan Schiffman explores how to program this algorithm in Processing.

My personal spin is then mostly comprised of a simple hack to distribute the
leaves in a torus shape—which is not an ideal shape for a ring, but I don’t
want to make it entirely flat, so it’s fair enough for now—, and linking any
stray branches to make the edges less uncomfortable. Let’s talk about that!

```
for (int i = 0; i < NUM_CLUSTERS; i++) {
  for (int p = 0; p < NUM_POINTS_PER_CLUSTER; p++) {
    PVector pos = PVector.random3D();
    pos.mult(SIZE_OF_CLUSTER);
    pos.add(sin(i)*SCALING, cos(i)*SCALING);
    leaves.add(new Leaf(pos));
  }
}
```
<div class="figure-label">Fig. 2: Making a torus of random points.</div>

Alright, that’s all of the code we need. Let’s squint at it and try to
understand what’s going on there. The basic idea of this algorithm is that
a bunch of circular point clouds offset to form a ring resemble a torus closely
enough for us to work with. That means that per iteration of the outer loop,
we create a cluster of points, with each point being assigned a random position
inside the ring. Then we offset the point: a scaled `sin` value based on the
current cluster number will be added to the X coordinate, and a scaled `cos`
value to the Y coordinate. Changing X and Y values with a combination of `sin`
and `cos` creates a circular motion, very handy for certain types of animation.

We then run the space colonization algorithm on that shape, and lo and behold,
the shape of the resulting “tree” is that of a torus. It also looks quite
similar to the ring showcased in Figure 1, except for an important ergonomic
problem: the branches are pokey. Branches stop growing when they can’t find any
“food” nearby, leading to a ring that’s probably a bit uncomfortable to wear.
Enter branch fusing, done once after all the food that can be reached has been
reached:

```
ArrayList<Branch> nonparents = new ArrayList<>();

// go through all branches and check whether they are
// not the parent of any other branch
for (Branch b : branches) {
  boolean isnonparent = true;
  for (Branch ib : branches) {
    if (ib.parent.equals(b)) { isnonparent = false; break; }
  }
  if (isnonparent) nonparents.add(b);
}

// then go through all of those, find the closest pair,
// and link the two
for (Branch b : nonparents) {
  Branch closest;
  float closestDist = -1;

  for (Branch ib : branches) {
    // don’t link branch to itself
    if (ib.equals(b)) continue;

    // get the distance
    PVector dir = PVector.sub(ib.pos, b.pos);
    float d = dir.mag();

    // if we don’t have any closest branches or
    // it’s the closest, set it!
    if (closest == null || d < record) {
      closest = ib;
      record = d;
    }
  }
  // make a branch that links the two
  if (closest != null) {
    // make the old branch the parent
    Branch n = new Branch(b);
    // make the other points end position the end position
    n.pos = closest.pos.copy();
    branches.add(n);
  }
}
```
<div class="figure-label">Fig. 3: Linking branches. Sorry it’s so long.</div>

This is probably the single longest snippet of code on my blog. That’s
unsurprising, considering it’s Java 7 and there’s a fair amount of ritual
involved in programming this algorithm.

The basic idea is simple, though. We filter all branches for the ones that are
not the parent of any other branch, meaning they point into nowhere and are
pokey. We then go through those and get the branch that’s also not a parent and
is closest, and link them. That’s it!

This leads to possible duplicate links, but also ensures that all of them are
linked at least once. There’s probably better ways of doing this, but I’m not
aware of any similar algorithms and, as such, was operating in an
algorithm-theoretic vaccuum and wanted to get it done by tonight. This is a
perfect solution for that.

## Fin

I only had half a day to sketch out a first idea for a ring, and I think this
is okay for what it is. I expect to go through my ideas and iteratively adapt
and refine them as I go, so stay tuned for updates! In the meantime, you can
always look at [the repository on Github](github.com/hellerve/ring).
