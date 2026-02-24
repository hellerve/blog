---
title: "Six Simple Sudoku Solvers VI: Elixir"
date: 2025-11-03
---

Welcome to our sixth and latest installment of Six Simple Sudoku Solvers! In case you are new here, this is a series about building six different Sudoku solvers in six different programming languages. [Check the first part](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) for more information on the algorithm as well as a reference implementation.

For this last post, we’re going to focus on Elixir. Elixir is a language that runs on the BEAM, the virtual machine that powers Erlang, a language for massively parallel workloads originally built for telephony systems. Elixir is the newer, hipper cousin of that language, sporting a Ruby-like syntax.

My goal is for us to build a parallel solver without even noticing; we’ll build the same algorithm we’ve built five times before now, and then just wave our hands and let Elixir take the wheel to make it parallel.

## Why Elixir?

Elixir is a pretty language to look at. It’s both approachable and powerful, and it can be (and is) used to solve real-world problems quickly and well. But since that doesn’t tell us very much yet, let’s look at some concrete language features that make Elixir interesting:

- **Effortless concurrency**. We can use the BEAM and its abstraction to spawn low-overhead threads and use messages passing to communicate between them (and is network-transparent!).
- **The Open Telecom Platform (OTP)**. The OTP is, strictly speaking, the whole Erlang distribution, but nowadays more often than not refers to the extensive library of abstractions for parallel applications that Erlang (and thus Elixir) ships with. Servers, state management, monitoring, supervisors, and others are all readily available as components, and have proven quite robust over decades of use in anger.
- **Excellent tooling and libraries**. Package management and build tooling through hex and mix, even documentation generation are all pleasurable experiences. And with modern frameworks such as Phoenix (a full-service web framework in the lineage of Ruby on Rails) working with Elixir feels not just productive but extremely ergonomic.

Truth be told, we won’t be needing much of that today. What we will use is a convenient base language with effortless concurrency. And I think that’s more than enough!

(Oh, and by the way, a Glamorous Toolkit package for it exists and I was able to write this blog post using real Elixir snippets!)

## The implementation

You know the drill: outside in, main solver first, then specifics.

### The main solver

This code should by now be familiar to you.

```elixir
def solve(board) do
  case propagate(board) do
    {:error, :contradiction} -> {:error, :unsat}
    {:ok, b} ->
      if solved?(b) do
        {:ok, b}
      else
        {{i, j}, cs} = find_mrv(b)

        branch(b, i, j, cs)
      end
  end
end
```

The only thing we might be looking at a little weirdly if this is our first time reading Elixir is the pattern matching, but even that’s pretty tame: `propagate` either gives us an error back and we in turn pass it to our caller, or we check whether things are solved, then find our best candidate and branch out. But how do we do that?

```elixir
defp branch(b, i, j, cs) do
  cs
  |> Task.async_stream(
       fn v -> solve(put_cell(b, i, j, v)) end,
       timeout: :infinity,
       ordered: false
     )
  |> Enum.find_value(fn
       {:ok, {:ok, s}} -> {:ok, s}
       _ -> nil
     end)
  || {:error, :unsat}
end
```

That’s more like it, what are we doing here? We take our candidates, make a task that tries to solve all of them, and then either select the first one that returned a solution or return an error (`|>` and `||` are combinators here to help us chain streams and collections together).

So already we got to the meat of this post: the concurrency. And it was incredibly simple, almost boring: we just called `Task.async_stream()` and it handled the rest for us. By specifying `ordered: false` we also ensure that we work on results as they come in and do not force an order, since we really don’t care which results come in first.

## Propagation

Next up is propagation, the solver of boards.

```elixir
defp propagate(board) do
  {b2, changed, ok?} =
    Enum.reduce(0..8, {board, false, true}, fn i, acc ->
      Enum.reduce(0..8, acc, fn j, {b, ch, ok} ->
        v = b |> Enum.at(i) |> Enum.at(j)

        cond do
          not ok -> {b, ch, false}
          v != 0 -> {b, ch, ok}
          true ->
            cs = candidates(b, i, j)
            cond do
              cs == [] -> {b, ch, false}
              length(cs) == 1 ->
                {put_cell(b, i, j, hd(cs)), true, ok}
              true -> {b, ch, ok}
            end
        end
      end)
    end)

  cond do
    not ok? -> {:error, :contradiction}
    changed -> propagate(b2)
    true    -> {:ok, b2}
  end
end
```

A little longer than the other propagation algorithms perhaps, but very simple. We formulate our nested sweep over the board as reductions, returning a new board and two flags as a result. The flags should be familiar by now, signifying change of the board in this pass as well as whether the board is still solvable.

Inside the loop we access the value and go over our check: if we already have an inconsistent board or the value is already set, we just continue to the next cell. Otherwise, we get the candidates. If we don’t have any valid candidates, we mark the board as inconsistent. If there’s only one, we fill the board and mark it as changed. Otherwise we just continue to the next cell.

After the loop we return an error if we are inconsistent, propagate again (recursion, yay!) if we had a change, or return.

### Finding candidates

Alright, let’s find the candidates for propagation.

```elixir
defp row(b, i), do: Enum.at(b, i)
defp col(b, j), do: Enum.map(b, &Enum.at(&1, j))

defp box(b, i, j) do
  r0 = div(i, 3) * 3
  c0 = div(j, 3) * 3
  for r <- r0..(r0 + 2), c <- c0..(c0 + 2), do: b |> Enum.at(r) |> Enum.at(c)
end

defp candidates(b, i, j) do
  used =
    MapSet.new(row(b, i) ++ col(b, j) ++ box(b, i, j))
    |> MapSet.delete(0)

  MapSet.to_list(MapSet.difference(@digits, used))
end
```

Honestly, thanks to pretty good abstractions this code is pretty boring! We make a set of all the row, column, and box values, delete 0, and get the difference from all possible digits.

The box calculation is the only one that looks a bit funky for uninitiated eyes, presumably, but it is just a `for` list comprehension (with two generators) that collects all values by going through the rows and columns (think of it as a list comprehension that does a Cartesian product). The syntax might be a bit daunting at first, but I encourage you to split it up by commas and see whether you can decipher the individual expressions and go from there!

## MRV selection

The last piece of our puzzle: selecting candidates for our search.

```elixir
defp find_mrv(b) do
  opts =
    for i <- 0..8, j <- 0..8, Enum.at(Enum.at(b, i), j) == 0 do
      cs = candidates(b, i, j)
      {{i, j}, cs}
    end

  Enum.min_by(opts, fn {_pos, cs} -> length(cs) end)
end
```

Since we can just re-use our candidates code right above, all we have to do is collect all candidates and get the minimum, and we’re done with both MRV and the solver at large!

## Fin

Even if you didn’t notice, we wrote a parallelized version of our solver in Elixir today! If it felt like revisiting Part I, this is no accident! I wanted to show you how with the right tools, parallelization and concurrency don’t have to be a pain. To be fair, our workload today was about as trivial as things can be, but let me assure you that Elixir can handle most difficult cases gracefully.

More than that, though, this brings our series to a close. We laughed, we cried, we puzzled, and you’ve been the best audience I could hope for! See you around, maybe for another season (I already have many more languages in mind!).
