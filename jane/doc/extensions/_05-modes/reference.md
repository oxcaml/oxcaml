---
layout: documentation-page
collectionName: Modes
title: Reference
---

The goal of this document is to be a reasonably complete reference to the mode system in
OxCaml.

<!-- CR zqian: For a gentler introduction, see [the introduction](../intro). -->

The mode system in the compiler tracks various properties of values, so that certain
performance-enhancing operations can be performed safely. For example:
- Locality tracks escaping. See [the local allocations
  reference](../../stack-allocation/reference)
- Uniqueness and linearity tracks aliasing. See [the uniqueness reference](../../uniqueness/reference)
- Portability and contention tracks inter-thread sharing.
    <!-- CR zqian: reference for portability and contention -->

# Lazy
`lazy e` contains a thunk that evaluates `e`, as well as a mutable cell to store the
result of `e`. Upon construction, the mode of `lazy e` cannot be stronger than `e`. For
example, if `e` is `nonportable`, then `lazy e` cannot be `portable`. Upon destruction
(forcing a lazy value), the result cannot be stronger than the mode of lazy value. For
example, forcing a `nonportable` lazy value cannot give a `portable` result. Additionally,
forcing a lazy value involves accessing the mutable cell and thus requires the lazy value
to be `uncontended`.

Currently, the above rules don't apply to the locality axis, because both the result and
the lazy value are heap-allocated, so they are always `global`.

Additionally, upon construction, the comonadic fragment of `lazy e` cannot be stronger
than the thunk. The thunk is checked as `fun () -> e`, potentially closing over variables,
which weakens its comonadic fragment. This rule doesn't apply to several axes:
- The thunk is always heap-allocated so always `global`.
- Since the thunk is only evaluated if the lazy value is `uncontended`, one can construct
a lazy value at `portable` even if the thunk is `nonportable` (e.g., closing over
`uncontended` or `nonportable` values). For example, the following is allowed:
```ocaml
let r = ref 0 in
let l @ portable = lazy (r := 42) in
```
- Since the thunk runs at most once even if the lazy value is forced multiple times, one
can construct the lazy value at `many` even if the thunk is `once` (e.g., closing over
`unique` or `once` values). For example, the following is allowed:
```ocaml
let r = { x = 0 } in
let l @ many = lazy (overwrite_ r with { x = 42 })
```

# Exceptions

The exception type `exn` crosses portability and contention.

For backwards compatibility with OCaml, we don't require exception constructor argument
types to cross portability and contention themselves. Instead, we treat each instance
of an exception constructor as belonging to the capsule it originally was defined in.

When the constructor is instantiated outside the original capsule
(i.e. in a `portable` function), its arguments are required to cross contention
and be portable. This parallels how `Capsule.Data.inject` requires its argument
to cross contention and be portable to insert it into another capsule.
Likewise, when pattern-matched on outside the original capsule, the constructor's arguments
must cross portability and are marked as contended, similar to `Capsule.Data.project`.

```ocaml
exception Foo of (unit -> unit)
exception Bar of int ref


let (foo @ portable) f =
  raise (Foo f) (* Here, [f] is required to be portable and must cross contention. *)

let (bar @ portable) g =
  try g () with
  | Bar x -> ... (* And here [x] is marked as contended and must cross portaibility. *)
```

Rebinding exception constructors "resets" its originating capsule.
It's permitted only if all its argument types cross portability and contention:

```ocaml
exception Crossing of int list
exception Noncrossing of (string -> unit)

let (cross @ portable) () =
    let module M = struct
        exception Crossing' = Crossing
    end in
    raise (Crossing [3; 4; 5])

let noncross () = (* can't be portable *)
  let module N = struct
      exception Noncrossing' = Noncrossing
  end in
  let r = ref "" in
  raise (Noncrossing ((:=) r))
```

WARNING: currently, first-class modules do not account for portability and contention
of extension constructors defined inside them. This leads to a soundness problem:

```ocaml
module type S = sig
    exception Exn of string ref
end

let make_s : (unit -> (module S)) Modes.Portable.t =
    let module M = struct
        exception Exn of string ref
    end
    in
    { portable = fun () -> (module M : S) }

let (foo @ portable) () =
    let module M = (val make_s.portable ()) in
    raise (M.Exn (ref "foo"))

let (bar @ portable) f =
    let module M = (val make_s.portable ()) in
    try f () with
    | M.Exn r -> print_endline !r (* [r] is uncontended despite crossing capsules *)

let () = bar foo (* prints "foo" *)
```

Exceptions also cross statefulness and visibility with identical restrictions.

# Modalities
Modalities, as described in the [syntax](./syntax) section, can be though of as functions
from mode to mode. For example, let's imagine one defines a record type with some modality
`m`:

```ocaml
type 'a t = { field : 'a @@ m }
```

Then, if we have a value `(t : _ t @ n)` then what's the mode of `t.field`? The answer:
apply the `m`. For future axes, the modality acts as a `min` between the record mode and
the written modality. For example:

```ocaml
type 'a t = { field : 'a @@ shareable }

let f : 'a t @ nonportable -> 'a @ shareable = fun t -> t.field  (* shareable < nonportable *)
let g : 'a t @ shareable -> 'a @ shareable = fun t -> t.field    (* shareable = shareable *)
let h : 'a t @ portable -> 'a @ portable = fun t -> t.field      (* portable < shareable *)
```

For past axes, the modality acts as a `max`. For example:

```ocaml
type 'a t = { field : 'a @@ shared }

let f : 'a t @ uncontended -> 'a @ shared = fun t -> t.field   (* uncontended < shared *)
let g : 'a t @ shared -> 'a @ shared = fun t -> t.field        (* shared = shared *)
let h : 'a t @ contended -> 'a @ contended = fun t -> t.field  (* shared < contended *)
```

However, things are more complex for diamond-shaped axes, such as visibility and
statefulness. In these cases, applying modalities to future modes results
in the _greatest common submode_, while applying modalities to past modes
results in the _least common supermode_. Mathematically, this corresponds to the meet
and join of the two modes, respectively.

For example, the least common supermode of `read` and `write` is `immutable`:

```ocaml
type 'a t = { field : 'a @@ write }

let f : 'a t @ read -> 'a @ immutable = fun t -> t.field
```

On the other hand, the greatest common submode of `observing` and `observable` is
`stateless`:

```ocaml
type 'a t = { field : 'a @@ observing }

let f : 'a t @ observable -> 'a @ stateless shareable = fun t -> t.field
```

The addition of `shareable` here is potentially surprising, but in fact necessary. Recall
from the [syntax](./syntax) section that `observing` implies `shareable`, but `observable`
implies `nonportable`, because we have no analogous notion of "function that only writes
mutable fields" on the portability axis. Thus, while applying the `observing` modality to
`observable` yields `stateless`, applying the `shareable` modality to `nonportable` only
yields `shareable` and not `portable`.

# Mode crossing
In the [intro](./intro) to modes, we saw the idea of "mode crossing", in which values of
types with particular properties can cross from some supermode to some submode for free.
For example, immutable data crosses most modes: a `string @ immutable` can always be
treated as a `string @ read_write` (because there are no mutable fields to read or write),
and a `string @ stateful` can always be treated as a `string @ stateless` (because it
contains no functions closing over mutable data).

But beyond concrete types, we have a generic facility for capturing mode crossing, using
`mod` syntax. This allows us to express behavior such as the following:

```ocaml
let cross_contended : type (a : value mod contended). a @ contended -> a @ uncontended =
  fun x -> x
;;

let cross_shared : type (a : value mod shared). a @ shared -> a @ uncontended =
  fun x -> x
;;

let cross_portable : type (a : value mod portable). a @ nonportable -> a @ portable =
  fun x -> x
;;

let cross_shareable : type (a : value mod shareable). a @ nonportable -> a @ shareable =
  fun x -> x
;;
```

Like modalities, diamond-shape modal axes allow for more interesting kinds of mode
crossing. In particular, because visibility allows us to cross `read` separately from
`write`, and statefulness allows us to cross `observing` separately from `observable`, we
can strengthen `read`-crossing values from `immutable` to `write` (and vice-versa), and
strengthen `observing`-crossing values from `observable` to `stateless` (and vice-versa):

```ocaml
let f : type (a : value mod read). a @ immutable shared -> a @ write =
  fun x -> x
;;

let f : type (a : value mod write). a @ immutable shared -> a @ read =
  fun x -> x
;;

let f : type (a : value mod observing). a @ observable -> a @ stateless shareable =
  fun x -> x
;;

let f : type (a : value mod observable). a @ observing -> a @ stateless shareable =
  fun x -> x
;;
```

And again, like modalities, there is a tricky interaction here between statefulness,
visibility, portability, and contention. Because the latter two axes are not themselves
diamonds, only `read` and `observing` carry any information with regard to portability
and contention. Thus, we cannot talk about a value's ability to cross from `contended` to
`shared` but not `uncontended`, nor a value's ability to cross to `portable` from
`shareable` but not `nonportable`.
