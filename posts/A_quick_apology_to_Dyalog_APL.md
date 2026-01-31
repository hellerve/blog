Today I get to do something that’s exceedingly fun for me: “well, actually” my old self.
In this case, I get to post a correction to my recent Dyalog APL Sudoku solver.

A few days ago, [Adám Brudzewsky](https://apl.wiki/adam) from the Dyalog APL team reached
out with detailed notes and a cleaned-up rewrite. He had some very kind words about my
solution, but pointed out some things to improve, which is just incredible.

He also graciously granted me permission to quote and attribute, so here we go.

### Prelude: Golf isn’t “APL-like”

In the original post I leaned into golfing to make the code feel “APL-like”. That
aesthetic is definitely common online, but Adám reminded me that it doesn’t have to
be golfed to look APL-like, and that production APL tends to be much less dense. That’s a
fair point. I got carried away a bit when shrinking my solution, and truth be told, it was
quite fun! But I feel like it’s important to point out that that’s optional.

The more useful lesson is: APL rewards good representations. If you pick shapes that
align with array operations, the code often becomes short without trying. I tried anyway,
but I should disclose that it isn’t a requirement for idiomatic APL.

### Concrete improvements

Adám also came to me with some concrete improvements that I want to dig into to make
sure I understand them correctly.

Adám suggested a tighter `S` that removes redundant parentheses (in line with [his
style guide](https://abrudz.github.io/style/)), replaces my “find minimum” with a
sort-based pick, simplifies how `@` is applied, and uses a more tacit step for
picking the first solution:

```apl
S←{
    b←P ⍵ ⋄ 0=≢b:⍬ ⋄ ~0∊b:b
    i←⍸0=b ⋄ cs←↑b∘C¨i ⋄ l←+/cs ⋄ k←⊃⍋l ⋄ idx←i[k]
    sols←⊂∘∇¨⊣@idx∘b¨cs[k;]/D
    ⊃(×∘≢∘⊃¨⍛/sols),⊂⍬
}
```

I also got a nicer candidate function `C`: it avoids explicit box-index arithmetic by
extracting the 3x3 box via drop/take (`↓`/`↑`), which reads very “APL” once you know the
idiom (which, uh, I didn’t really):

```apl
C←{
    m←9 9⍴⍺ ⋄ r c←9 9⊤⍵
    ~D∊0~⍨∪m[r;],m[;c],,3 3↑(3×⌊r c÷3)↓m
}
```

If you compare this to my original `C`, the big change is that the box is obtained by:

* computing the box origin (`3×⌊r c÷3`)
* dropping that many rows/cols (`↓m`)
* taking a 3x3 (`3 3↑…`)

Much cooler.

### One note on boxing and unboxing

My original solver boxed `sols` just to unbox it again. This was a defensive step against
shooting myself in the foot with my own limited understanding. Dyalog may sometimes mix
same-shaped results into a plain array because it can—and I don’t understand the rules
it follows completely—which breaks a pipeline that filters non-empty solutions and then
takes the first remaining one.

The rewrite above makes the selection step cleaner by doing away with the need for explicit
unboxing. It feels less brittle and is more readable to boot!

### Thanks, and where to find Adám

Thanks again to Adám for the review. He mentioned he’s reachable in the [APL
Orchard](https://apl.chat/) (which I understand he started!). He was exceedingly kind and
helpful, so I encourage anyone who’s even remotely interested in APL to hop in and say hi!
