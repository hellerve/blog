Elixir introduced a very controversial change in version 1.4. They deprecated “barewords”. The mechanism of barewords is basically a fundamental part of Elixir's behaviour where, when a function is called without parameters, you can omit the parentheses. You can read about the change [here](https://github.com/elixir-lang/elixir/issues/3268). But let me give you a little motivating example. I'll try to make it very simple, so even those of you who have never programmed in Elixir will understand what this means:

```elixir
# return an empty list
def data do
  []
end

# old version
# assign the return value of a function
# to a variable of the same name. yuck!
data = data 
something_else_with_data(data)

# alternative: call the function directly
# but: is it a variable or a function?
# we don't know.
something_else_with_data(data)

# new version
data = data()
something_else_with_data(data)

# alternative: much cleaner like that
something_else_with_data(data())
```

Note that while the old version now throws a warning, it will only warn the user, not throw an error.

I, for one, am in favor of that change. I do understand it will “break” a lot of code—keep in mind it's only warnings, though—, but it reduces the cognitive load on the developer and removes ambiguity, which is a seriously good thing in my book. For people coming from Ruby or people who hate typing unnecessary characters even more than I do it might not actually be an improvement, but I think that this is effortlessly trumped by removing a whole class of bugs. Then again I'm not a professional Elixir developer and have only ever worked on one serious project using the language. Regardless, I think I do have a relatively firm grasp on its concepts and its internals—I even scanned through the kernel module, which defines a lot of the syntactic abstractions of Elixir as macros.

Elixir always struck me as a beautifully designed language; it brings almost everything I want from a language to the table, and I think this is another step in the right direction. Let's hope that the core developers keep up their quality and hard work, because Elixir is one of our few hopes for a beautiful, bug-free future.
