---
layout: documentation-page
collectionName: Kinds
title: Implicit Kind Declarations
---

# Implicit Kind Declarations

In signatures, type variable names can be declared to have *implicit kinds*.
A type variable with a name that has an implicit kind will be instantiated
with that kind. Here's an example:

```ocaml
[@@@implicit_kind: ('elt : word)]

type 'elt collection

val singleton : 'elt -> 'elt collection
val lenght : 'elt collection -> int
```

This signature is equivalent to:

```ocaml
type ('elt : word) collection

val singleton : ('elt : word) . 'elt -> 'elt collection
val lenght : ('elt : word) . 'elt collection -> int
```

You can declare implicit kinds for multiple variable names at once:

```ocaml
[@@@implicit_kind: ('a : immediate) * ('b : immediate)]

val swap : 'a * 'b -> 'b * 'a
```

Implicit kinds can't be overridden -- a variable declared with
an implicit kind must always have that kind. Attempts to narrow or change it will fail:

```ocaml
module type S = sig
  [@@@implicit_kind: ('a : value_or_null)]
  val i : ('a : value mod external_) -> 'a
  val j : ('a : bits64) -> 'a
end

[%%expect{|
Line 3, characters 10-36:
3 |   val i : ('a : value mod external_) -> 'a
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value_or_null
       But it was inferred to have kind value mod external_
         because of the annotation on the type variable 'a.
|}]
```

Implicit kinds are inherited by signatures:

```ocaml
module type Outer = sig
  [@@@implicit_kind: ('t : bits64)]

  val outer : 't -> 't

  module Inner : sig
    (* Also [bits64] *)
    val inner : 't -> 't
  end
end
```

Trying to re-declare an implicit kind will fail too:

```ocaml
module type Outer = sig
  [@@@implicit_kind: ('t : bits64)]

  val outer : 't -> 't

  module Inner : sig
    [@@@implicit_kind: ('t : immediate)]

    val inner : 't -> 't
  end
end

[%%expect{|
Line 7, characters 29-38:
7 |     [@@@implicit_kind: ('t : immediate)]
                                 ^^^^^^^^^
Error: The implicit kind for "t" is already defined at Line 2, characters 27-33.
|}]
```


Implicit kinds can't be declared in structures, though we plan to support that.

Implicit kinds are syntactically limited to the signature
they are declared in and won't be `include`d:

```ocaml
module type S = sig
  [@@@implicit_kind: ('a : value_or_null) * ('b : immediate)]

  val fst : 'a * 'b -> 'a
end

module type T = sig
  include S

  (* ['a] and ['b] are defaulted to [value] here: *)
  val snd : 'a * 'b -> 'b
end
```

Implicit kinds affect `constraint`s and are, for now, the only way to set
`constraint`s to certain kind values:

```ocaml
module Constrained : sig
  [@@@implicit_kind ('a : value_or_null) * ('b : value_or_null)]

  type 'c t constraint 'c = 'a * 'b
                       (* the only way to get [value_or_null] here *)
end
```
