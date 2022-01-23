A while back I had the opportunity to work with Common Lisp professionally.
As has happened to many before and after me, a lot of the powerful features of
Common Lisp and its implementations made me a little drunk on power for a
while, but I quickly recovered.

Some things stuck with me, however. Among them was [`setf](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm#setf),
which feels similar yet different to another concept that I quite adore: [Lenses](https://blog.veitheller.de/Lets_Build_Lenses_in_Carp.html).

As with lenses and many other concepts before them, I decided to try to
understand `setf` more deeply by implementing it in Carp. [The final pull
request](https://github.com/carp-lang/Carp/pull/1192) to the Carp standard
library got rejected—by myself, no less!—, but [the library lives
on](https://github.com/carpentry-org/setf/)!

In this blog post, we’re going to look at how to implement `setf` together.
It’s going to be an interesting riff on what we did when we implemented
[`derive`](https://blog.veitheller.de/Carp_and_derive_II:_This_Time_Its_Personal.html).

Sit tight, and grab a drink! It’s follow-along time!

## What is `setf`, anyway?

To know how to implement it, maybe we should get acquainted with `setf` first.

To understand `setf`, we should first talk about “places”. Places are the
name for the getter functions that we use as descriptors to tell `setf` what to
update. For instance, we could do:

```
(def x [1 2 3])
(setf (nth &x 1) 4) ; => [1 4 3]
```

The place here would be `nth`. The magic here is that, by convention, the
setter is named after an existing getter function, so it feels as if `setf`
magically knows how to transform a getter into a setter.

You can also register your own places. For Carp, I chose to implement three
such functions:

```
; register-place takes a name and a function that,
; given the arguments, knows how to transform them
; into a call to the setter
(register-place 'nth
                (fn [args] `(Array.aset! %@args)))

; register-simple-place is a simple abstraction
; that takes a name and the function that it
; should be rewritten to
(register-simple-place 'nth 'Array.aset!)

; register-struct-places takes a type and creates
; places for all its members
(register-struct-places 'Vector2)

(let-do [x (Vector2.init 1 2)]
  (setf (Vector2.x &x) 10)
  x) ; => (Vector2 10 2)
```

Given these parameters, implementing `setf` should be easy! Let’s do this!

## Implementing `setf`

First, we need a place—no pun intended—to store our places. Since it’s going to
be a mapping from names to functions, a hashmap seems like a good data
structure. Let’s call it `places`.

```
(defdynamic places {})
```

Next up, let’s define the registration functions.

```
(defndynamic register-place [name builder]
  (set! places (Map.put places name builder)))

(defndynamic register-simple-place [name setter]
  (register-place name
                  (fn [args] (cons setter args))))
```

`register-place` will simply update the `places` map.
`register-simple-place` will build upon this by building a function that just
puts the setter in front of all the arguments received.

As might be expected, `register-struct-places` is a little more involved.

```
(defndynamic register-struct-places [t]
  (map
    (fn [member]
      (let [name (car member)
            getter (Symbol.prefix t name)
            setter (Symbol.prefix t
                                  (Symbol.concat [
                                    'set- name '!
                                  ]))]
        (register-simple-place getter setter)))
    (members t)))
```

In the end, we just iterate over the members of the type and use
`register-simple-place` to tie together the getters and setters that the
compiler autogenerates for us. For a `Vector` type with an `x` coordinate,
for instance, the appropriate pair will be `Vector.x` and `Vector.set-x`.

For convenience, we define a function to get a place from the map:

```
(defndynamic get-place [n]
    (Map.get places n))
```

This is really just a simple wrapper around map retrieval.

Now that we have all the plumbing for registration in place, we can finally
implement the main event, i.e. the `setf` macro. It is the most intricate
part of the puzzle, but most of its complexity comes from error handling.
As such, let’s look at a naïve implementation first:

```
(defmacro setf [place val]
   (let [setter (get-place (car place))]
      (setter (cons-last val (cdr place)))))
```

We get the setter function, and then apply it to the rest of the form, with
the new value appended. This means that `(setf (nth &x 1) 10)` will be given
to the setter registered under `nth` as `(&x 1 10)`. This happens to match
the signature of `Array.aset!` perfectly, so we can register it as a
pass-through, i.e. a `simple-place`.

Now, to enable setting variables, we use a trick: we transform it into a list
or `(sym <variable>)` and register `set!` as a `simple-place`, meaning that
`(setf x 10)` will first be transformed to `(setf (sym x) 10)`, and then to
`(set! x 10)`. To enable that behavior, we have to change `setf` a little:

```
(defmacro setf [place val]
  (let [place (if (symbol? place) `(sym %place) place)
        setter (get-place (car place))]
    (setter (cons-last val (cdr place)))))
```

Alright, all that’s left is handling errors gracefully. First, let’s make sure
we give a good error message when a place isn’t known:

```
(defmacro setf [place val]
  (let [place (if (symbol? place) `(sym %place) place)
        key (car place)
        setter? (get-place key)]
    (if (= nil setter?)
        (macro-error (list "I didn’t find a `setf` place for " key ". Is it defined?"))
        (setter? (cons-last val (cdr place))))))
```

Since `Map.get` will return `nil` when the key doesn’t exist, we can just check
for that and move on. Now there is only one error case left that we have to
deal with: garbage input.

```
(defdynamic malformed (gensym-with 'place-malformed))

(defmacro setf [place val]
  (let [place (if (symbol? place) `(sym %place) place)
        key (if (and (list? place) (not (empty? place)) (symbol? (car place)))
               (car place)
               malformed)
        setter? (get-place key)]
    (cond
      (= key malformed)
        (macro-error (list "The `setf` place " place " is malformed. A list or symbol was expected."))
      (= nil setter?)
        (macro-error (list "I didn’t find a `setf` place for " key ". Is it defined?"))
        (setter? (cons-last val (cdr place))))))
```

We introduce a special symbol to signal that the place that was put in was not
a non-empty list that starts with a symbol. Any non-empty list that has a
symbol as its first element could potentially be a valid place, anything else
is invalid.

And that’s all we need to do to define `setf`!

## Fin

If you followed my [`derive`](https://blog.veitheller.de/Carp_and_derive_II:_This_Time_Its_Personal.html)
journey, a lot of the ground we covered today should be familiar territory. If you
didn’t—and also if you did!—, I hope you enjoyed our little journey, maybe
learned a thing or two, and got inspired to play around with the concepts a bit
on your own time.

See you soon!
