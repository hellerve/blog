```scheme
; correct load semantics
(load random) ; refers rand-elems, for example
(require io)
(require strings s)

; define with def, keyword arguments by default
; (defined in mythical language)
(def (print-rand-string len alphabet := [\a \b \c])
  (|> (take len (rand-elems alphabet))
      s:join
      io/print))

; simple data structures
; (constructors defined in mythical language)
(def array [1 2 3]) ; internally a hash map with integer hashes
(def hash  {0 1, 1 2, 2 3}) ; the same hash map, explicitly

; succinct, hygienic, pattern matching macros
(defsyn cp (=>) "cp documentation"
  ((_ x => y) (define y x))
  ((_ x => y more ...)
    (define y x)
    (x => more)))

; first-class typing support, optional
:(typed-first : [a] -> a)
(def (typed-first l) (head l))
```
