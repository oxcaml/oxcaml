[coalesce_arity] merges the parameters of a function whose body is itself a function.
This is what metaquot uses it for: [[%expr fun x -> [%e e]]] should have the arity of
[e] plus one, rather than arity one.

  $ ./test.exe
  nested functions:
    before: fun x -> fun y -> x + y
    after:  fun x y -> x + y
  under newtypes:
    before: fun (type a) -> fun x -> fun y -> x + y
    after:  fun (type a) x y -> x + y
  function cases body:
    before: fun x -> (function | y -> x + y)
    after:  fun x -> function | y -> x + y
  constrained result:
    before: fun x  : (int -> int) -> fun y -> x + y
    after:  fun x  : (int -> int) -> fun y -> x + y
  attributes on the body:
    before: fun x -> ((fun y -> x + y)[@inline ])
    after:  fun x -> ((fun y -> x + y)[@inline ])
