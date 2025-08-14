# The `let mutable` extension

The `let mutable` extension provides a mechanism for creating mutable variables.
This codifies a pre-existing optimization, where the compiler attempts to
eliminate allocating a box for a `ref` when it can see the `ref` is only used
locally in a given scope, instead simply storing the value in a register.

```ocaml
let triangle n =
  let mutable total = 0 in
  for i = 1 to n do
    total <- total + i
  done;
  total
```

Mutable variables must not escape their scope. For example, a closure can't
capture a mutable variable.

Local data can be stored in a mutable variable. For example:

```ocaml
let rec sum (to_sum @ local) =
  match to_sum with
  | [] -> 0
  | hd :: tl -> hd + sum tl

let triangle_list n =
  let mutable to_sum = [] in
  for i = 1 to n do exclave_
    to_sum <- stack_ (i :: to_sum)
  done;
  sum to_sum [@nontail]
```


## Restrictions

Mutable `let` declarations may not be recursive, and they may not be used at the
structure level or in class definitions. The pattern of a mutable `let`
statement must be a single variable, possibly with a type annotation, e.g. `let
mutable x, y = ..` and `let mutable add x y = ..` are not allowed. Mutable `let`
statements must also not use `and`s.

Because closures may not capture mutable variables, some uses that are
apparently safe from a scope perspective are not possible. For example, the
following program is rejected:

```ocaml
let sum xs =
  let mutable total = 0 in
  List.iter xs ~f:(fun x -> total <- total + x);
  total
```
