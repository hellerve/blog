A dark, mostly open secret about my trade is that I always come in when things
are on fire. I’m a consultant, a glorified mercenary, and whenever a company
calls me, they experience some sort of problem they believe they can’t fix on
their on. I’m a gun for hire, if you will.

There is nothing embarassing about asking for help, even as a company. I would
argue that the companies that hire external people to fix things have not
“failed”, even if it might seem that way. They merely acknowledge that they
have a problem and try their best to fix it before it’s too late, and often
they get a bad rep for it. Noone wants to know they’re not doing the right
thing<sup><a href="#1">1</a></sup>.

I almost exclusively get called in because of all kinds of technical
problems<sup><a href="#2">2</a></sup>. Most of these can be boiled down to one
simple buzzword: technical debt. Legacy systems are often the problem: code
that noone wants to touch, whose maintainers have moved on and that are left to
do their magic without anyone skilled in their incantations supervising them.

As a result, I’ve done a fair share of blind refactoring, and today I want to
share some of my insights and experiences. Sandy Metz has a [great
talk](https://www.youtube.com/watch?v=8bZh5LMaSmE) on refactoring which
characterizes a lot of my work. She does a better job at exploring this space
than I could ever aspire to, but I will try my best to add my own spin to it,
just for the heck of it.

## A Tale of Two Blocks

In her talk, Sandy Metz says she does the “squint test”. I’ve heard this term
before, and it’s very appropriate, but even that is a very idealistic way to
see the world. Her main concern in the talk is pulling apart things and
rearranging them, and big chunks of procedural, nested code are a good example
to showcase the squint test on. But I often find myself in a situation where
other blind minds have concluded that the code needs to be refactored, long
before my cold grubby hands got ahold of it.

But often deadlines came in the way of the “recreational” art of refactoring,
and features were deemed more important—or something along those lines—, and
they only had time to pull the method apart. So I’m looking at a partially
refactored code base, parts of the logic are everywhere, and nothing makes
sense.

The worst artifact of a partial refactor for me is two interdependent blocks
of code—mostly methods—that have been pulled apart, but still need each other
to function. This unholy symbioses can really ruin my day.

```
class FizzyBuzzer():

  def fizz_up(obj):
    if obj.fizzed():
      return obj
    elif obj.buzzed():
      return obj.fizz()
    else:
      return self.buzz_up(obj.fizz())

  def buzz_up(obj):
    if obj.buzzed():
      return obj
    elif obj.fizzed():
      return self.buzz()
    else:
      return self.fizz_up(obj.buzz())
```
<div class="figure-label">Fig. 1: We are two, we are one, we are many.</div>

In Figure 1, I prepared a silly example that is an encapsulation of an artifact
I encountered in a client’s code base. How does such silly code come to be?

To begin with, there was one giant method. The person looking at it realized
they could pull it apart, and separated the fizzing from the buzzing. They
knew that the input to the method could either be fizzed, buzzed, or neither.
But either they didn’t know which method would be called from the top level,
or the next person realized they had made a mistake and added a bunch of checks,
and now both methods duplicate all of the checks to make sure that the object
doesn’t end up in an inconsistent state. They are effectively useless without
each other to the point where we could probably make them one big method again
and end up with less confusing code.

This is obviously silly, but it happens all the time. Behaviour gets added over
time, the preconditions change, and we add a bunch of checks to fix up our code
without re-evaluating it. If we do have time to refactor some of the things, we
often don’t go down the road to the end and end up with worse code. That’s what
happened in the talk I linked to above as well: the intermediate steps that we
take until we arrive at a good solution are often more complex than what we
started out with.

So I guess the first take-away from this is: if you are refactoring, start with
the assumption that it will be worse before it can be better. It’s
counter-intuitive, but incredibly liberating. If you have a goal in mind, don’t
doubt it when you’re halfway there because your code starts to look worse. If
you’re truly hopeful that the end result will be good, try to get to
it<sup><a href="#3">3</a></sup>.

## It’s simple: we add a check for it

The next thing I want to talk about has to do with the way in which functions
and their scope grow. Often we realize that initial assumptions around which we
architect our code are wrong, and we have to fix a tiny bit of behavior. When
that happens, the first idea that is thrown around infallibly is “let’s add a
check here to test for it and tweak it a little”. It’s non-intrusive, and it’s
cheap—in the beginning. As those checks pile up, the functions become ever
harder to grasp and get out of hand.

Often we realize this when it is too late, and the function has already become
unmanageable. The first piece of advive is thus “don’t let it come to that”.
Rewind and think about how the assumptions have changed and how to best express
this.

But you’ll inevitably find yourself in a situation where you have to untangle
a piece of code where this kind of organic growth has already happened. It
happens to me all the time. What I’d adivse you to do in this situation is to
write down the assumptions and the problem we’re trying to solve. Often you’ll
see that by shifting our perspective we can avoid half of the complexity that
makes our functions unmanageable.

```
def do_the_math(type, quantity, operation):
  if type == 'flowers' and quantity < 12:
    return operation(quantity*1.3+4)

  if type == 'flowers':
    return operation(quantity*1.24)
  elif type == 'vertebrae':
    return quantity
  elif type == 'null':
    return fizzbuzz(15)
  else:
    return operation(12+type)
```
<div class="figure-label">Fig. 2: The thing from outer space.</div>

In Figure 2, you can see my mind running rampant, trying to recreate the
weirdest, most incongruent looking code I’ve seen at a
client<sup><a href="#4">4</a></sup>. When you see a piece of code like that,
rather than pulling it apart and refactoring using more “classic” approaches,
ask yourself—and the product people who can tell you what your code wants to
achieve—what the assumptions are, and whether all of that cruft is still
needed.

## We haven’t touched that in years

My last piece of advice ties in to the second one, but extends it a little bit.
Remember those product people who know what actually happens in the real world?
Try pulling them aside when they have time, and go over the tests that test
more arcane parts of the behavior with them. They probably can’t read them, so
summarize what the test does, and then ask them whether this behavior is still
needed. Mark all of the tests where they say no or are unsure, and revisit
those with one of the senior engineers to make sure that it isn’t actually a
bit of internal business logic that the customer might not know about, but that
makes your product work<sup><a href="#5">5</a></sup>.

Getting rid of those tests on their own isn’t very helpful. If you have a code
coverage tool in place, remove the tests, see where the coverage goes to zero,
and see if you can remove those lines (otherwise write new tests). If you don’t
and are in for a dangerous, unprofessional, and exciting adventures, play
around with them and see what breaks, and remove those things. I strongly
advise you to take the first path, but I can’t say I’ve never gone down the
latter, so who am I to judge?

## Don’t trust me

Finally, I’d like to tell you to exercise great caution with everything I told
you in this blog post, even if it made sense. None of it is particularly
quantifiable, and I have the bad programmer habit of extrapolating from
anecdotal advice. I’d like to say I have empiric evidence about the
effectivenes of my advice, but I don’t. YMMV, I guess.

See you around!

#### Footnotes

<span id="1">1.</span> My first instinct was to write that noone likes to be
                       told that they’re wrong, but that’s too harsh. Often
                       people are doing the right thing in the wrong way, which
                       is even more heinous, because it’s more subtle.

<span id="2">2.</span> Sometimes there are structural problems that I can’t fix
                       on my own; I’m mostly only hired as a technical add-on
                       to the team. If I encounter problems outside of my realm
                       I communicate them, but I’ll not mess with things just
                       because I think they are wrong, especially because I’m
                       always an outsider and probably lacking context.

<span id="3">3.</span> One of the reasons why my blog posts always get so
                       rambly is that I cannot let blanket statements like this
                       one sit there without a caveat: if you lose hope during
                       your work and can quantify why, it’s worth at least
                       investigating whether you might be wasting your time, of
                       course. Don’t just trust your gut instinct blindly.

<span id="4">4.</span> One of my clients employed a very productive, smart, and
                       eccentric programmer. He’d sometimes microdose on
                       psychedelics and bang out large amounts of code that
                       worked, but was a nightmare to look at. It all made
                       sense, I think, but I got rid of a lot of it, anyway.

<span id="5">5.</span> Those pieces of weird semi-internal pieces of behaviour
                       tend to grow at about the same rate as the rest of the
                       code base. Revisit them and try to get rid of them if
                       you can, because noone can remember them and they are
                       everyone’s biggest source of bugs.
