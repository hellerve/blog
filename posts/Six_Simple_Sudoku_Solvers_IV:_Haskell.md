---
title: "Six Simple Sudoku Solvers IV: Haskell"
date: 2025-10-13
---

It is time for Sudoku, once more (we are more than half-way through)! In case you are new here, this is a series about building six different Sudoku solvers in six different programming languages. [Check the first part](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) for more information on the algorithm as well as a reference implementation.

Today we’re going to look at Haskell, including types, monads, the whole thing. We’re going to look at monadic states, internal mutation, and laziness as a backtracking mechanism.

As with Common Lisp and Prolog, it won’t necessarily be a good primer for the language, and I suggest following all the rabbit holes you can find if you want to dig deeper; it’s a rewarding if a little exhausting exercise.

## Why Haskell?

Haskell is mostly known for being a language with an advanced type system and for the scary word “monads”. In my opinion, that’s a pity. Haskell is a deeply elegant weapon, designed for sharp problems of all shapes and sizes, and while it doesn’t necessarily produce aesthetically pleasing solutions, it makes damn sure it doesn’t get into your way if you want to follow your stylistic intuitions.

Apart from that, however, it also has more substantive qualities that lend themselves well to algorithms like Sudoku solvers:

- **Laziness**. Haskell expressions only get evaluated when they’re actually used. This is cool, powerful, and sometimes a nightmare to debug. In this case, we will use it to short-circuit our backtracking once we’ve found a solution, and I just think that’s awesome.
- **Algebraic combinators**. Haskell is combinator-heavy. This one is somewhat hard to explain, but it’s abundance of algebraic combinators make it easy to express things like searches and constraints, and we’ll make lots of use of it.
- **Applicatives, Monads, Category Theory**. We’ll use both Applicatives and Monads today—for control flow and state, respectively—, and that’s only the tip of the iceberg! These concepts are scary and the names don’t help, but they are definitely worth a look, and I’ll try my best to demystify the small bits we’ll be using today.

I hope those things are enough to make a compelling case for why Haskell is worth a look. And, once again, our solver will be less than 100 lines of code (we could golf it down more, but that would defeat the purpose).

I do want to warn you that there would be a much simpler way to solve this in Haskell, but I simply could not resist the urge to show how internal mutation is still a possibility in a language that has the reputation of being the most pure used in industry.

## The solver

As always, we’ll start with the main solver. As with most of our solutions except for Prolog, we’ll then look at propagation, candidates, and MRV search.

Before we do so, however, we’ll define a type alias to feel comfortable when looking at types.

```
type Board = [[Int]]
```

This is, of course, not strictly necessary, but it makes things a bit more intuitive to look at.

### The main entrypoint

The main `solve` function is deceptively short, but quite involved.

```
solve :: Board -> Maybe Board
solve b0 = do
  b <- propagate b0
  if solved b
  then pure b
  else
    let ((i,j), cs) = findMRV b
    in foldr (<|>) empty [ solve (set b i j v) | v <- cs ]
```

The type declaration of the function here is above the function definition. It tells us that we give the function a `Board`, and we get a `Maybe Board`—equivalent to `Option(Board)` in other languages—back. `Maybe` is a type with multiple constructors—in Haskell, we refer to these as sum types—, with the constructors `Just value` or `Nothing`. The former transports a value, the latter shows the absence of a value. It’s an explicit way of demarking return value optionality.

First we’ll call our usual propagation (by now you should have an idea what that means), and check if we’ve solved the puzzle. If yes, we’ll return the solution. If no, we’ll get all the candidates and use `foldr`—algebra speak for `reduce` or `inject:into:`—to iterate over them.

Now, this is our first bit of magic. How do we iterate over them? How do we short-circuit? What is this whole fold line?

I already teased laziness in the beginning, and this is where it get’s crucial. For every candidate, we insert it (`set b i j v`) and plug it into `solve`. This is a list comprehension akin to what you might find, for instance, in Python. We then take the resulting list and plug it into `foldr`, using `<|>`  as a fold function, and `empty` (which is implemented by `Maybe` as `Nothing`) as a first accumulator.

What does `<|>` do? It is the choice operator, and it comes from the `Alternative` type class, which is “a monoid on applicative functors”—in our case, simply a thing we can combine with other instances of that thing. You can think of it like this:

```
empty      = Nothing
Nothing <|> r = r
Just x  <|> _ = Just x
```

So, we’re just combining things and take the first `Just` value we find and stick with it. And because of laziness and the fact that the unused computations will never be performed, we end up with something akin to:

```
recur []     = Nothing
recur (v:vs) =
  case solve (set b i j v) of
    Just s  -> Just s
    Nothing -> recur vs
```

But a bit slicker and generic over all instances of the `Alternative` type class (think of it as an interface). You could also try swapping the fold for `asum` from `Data.Foldable` as an exercise, if you were so inclined.

### Propagation

Next, we’ll try to run a fill-in pass.

```
propagate :: Board -> Maybe Board
propagate b0 = runST $ do
  a <- thawBoard b0
  ok <- fixpoint a
  if not ok then pure Nothing else Just <$> freezeBoard a
```

Again with the magic. This time we do it for performance reasons, however. We get internal mutation inside a monad called `ST` (State Threads) that we let execute with runST, so the outside world stays pure. I will, for the purposes of this post, ignore `thawBoard` and `freezeBoard`. They simple transform our nice little board into a dirty, mutable array encapsulated in said monad and back, so we can do our unclean mutations. If you do want to check it out, you can do so [by looking at the repo version](https://github.com/hellerve/sudoku/blob/main/sudoku.hs) or by going to the documentation of [`STUArray`](https://hackage.haskell.org/package/array-0.5.8.0/docs/Data-Array-Base.html#t:STUArray) directly. Our actual work happens in `fixpoint`, which will, as is tradition, mutate the board in place and return a marker whether the board is solvable.

```
fixpoint :: STUArray s (Int,Int) Int -> ST s Bool
fixpoint a = go True
  where
    go ch = if ch then do
               (ch', ok) <- sweep a
               if not ok then pure False else go ch'
             else pure True

sweep :: STUArray s (Int,Int) Int -> ST s (Bool, Bool)
sweep a = do
  changed <- newSTRef False
  ok <- newSTRef True
  forM_ [0..8] $ \i ->
    forM_ [0..8] $ \j -> do
      v <- readArray a (i,j)
      when (v == 0) $ do
        cs <- candidatesST a i j
        case cs of
          []   -> writeSTRef ok False
          [x]  -> writeArray a (i,j) x >> writeSTRef changed True
          _    -> pure ()
  (,) <$> readSTRef changed <*> readSTRef ok
```

Have you ever seen code so pretty? I kid, but let’s try it anyway.

The function `fixpoint` is our helper that will run passes over the board until it doesn’t change anymore. `sweep` is the function that actually performs that pass. Taking away all the state-fu, we are left with a function that has a marker for whether the board has been mutated this pass (`changed`) and a marker for whether it’s still solvable (`ok`). We then go into the nested loop, reading the value from the array, and checking for candidates for those that are still `0`. If we don’t find any, the board is unsolvable and we set `ok` to false. If we find exactly one candidate, we set the value and mark the pass as having changed something. Otherwise we do nothing.

So why is this so clumsy? This is what it looks like to try and write imperative code in a pure functional language. It is still safe and encapsulated, and the state remains local. But it’s a pain to deal with and to look at, as all state should be—I’m only partially joking.

#### Finding candidates

Next up, we’ll look at finding the individual candidates, which is done using `candidatesST` (the same `ST` as above, so you know it’ll be good).

```
candidatesST :: STUArray s (Int,Int) Int -> Int -> Int -> ST s [Int]
candidatesST a i j = do
  usedR <- mapM (\c -> readArray a (i,c)) [0..8]
  usedC <- mapM (\r -> readArray a (r,j)) [0..8]
  let r0 = (i `div` 3) * 3
  let c0 = (j `div` 3) * 3
  usedB <- sequence [ readArray a (r,c)
                    | r <- [r0..r0+2], c <- [c0..c0+2] ]
  let used = filter (/=0) (usedR ++ usedC ++ usedB)
  pure (digits \\ used)
```

Where is the eye bleach?! Let’s try and figure it out.

`usedR` and `usedC` are the rows and columns, respectively. `usedB` is the block, and functions essentially the same way as in the other algorithms. We then add them together, filter out any `0`, and return the difference of `digits` (which is bound to `[1..9]`), so all the values we haven’t found.

Once again, bending Haskell to our will has worked, but left us with code that looks less than elegant. Let’s press on to resolve that issue!

### Computing the MRV

Finally, the candidates for our depth first search await!

```
row i b = b !! i
col j b = map (!! j) b
box i j b =
  [ b !! r !! c | r <- [r0..r0+2], c <- [c0..c0+2] ]
  where r0 = (i `div` 3) * 3; c0 = (j `div` 3) * 3

cands b i j =
  let v = b !! i !! j
  in if v /= 0
     then [v]
     else digits \\ filter (/=0) (row i b ++ col j b ++ box i j b)

findMRV :: Board -> ((Int,Int), [Int])
findMRV b = minimumBy (comparing (length . snd))
  [ ((i,j), cands b i j) | i <- [0..8], j <- [0..8], b !! i !! j == 0 ]
```

That’s more like it! Here, unencumbered by low-level array accesses, we can make Haskell’s combinators truly shine.

The heuristics are the same as before: we compute the candidates (that’s the list expression at the bottom there together with the functions at the top), and then get the result with the shortest list of candidates (`minimumBy . length` with extra steps).

It might seem awkward to basically redo all the candidate search we already did in the state monad, but here this was a deliberate choice to show you how mutable, efficient code looks like next to pure, slightly less efficient (though not by as much as you might think) code.

But, as is tradition by now, we can now pat ourselves on the back: we’ve written a Sudoku solver!

## Fin

Once again, we’ve tried to use a language to its fullest by showing off all the things it can do and is good for in a humble Sudoku solver, in under 100 lines of code. I might be slightly biased, but I think that’s kind of neat!

Now, we might have dipped our toes in the waters of mutability and efficiency with this one, but in the next one we’ll take it up a notch by taking Rust through its paces, lettin’ ’er rip! Stay tuned!
