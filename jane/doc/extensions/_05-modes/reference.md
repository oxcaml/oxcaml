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
