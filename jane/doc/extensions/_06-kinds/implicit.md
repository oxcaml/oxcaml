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

Implicit kinds can't be overridden -- a variable declared with
an implicit kind must always have that kind. Attempts to narrow it will fail:

```ocaml
module type S = sig
  [@@@implicit_kind: ('a : value_or_null)]
  val i : ('a : value mod external_) -> 'a
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

Trying to re-declare an implicit kind will fail too:

```ocaml
module type Oiter = sig
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
