---
layout: documentation-page
collectionName: Modes
title: Reference
---

<!-- CR zqian: For a gentler introduction, see [the introduction](../intro). -->

The mode system in the compiler tracks various properties of values, so that certain
performance-enhancing operations can be performed safely. For example:
- Locality tracks escaping. See [the local allocations reference](../../stack/reference)
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

# Advanced mode crossing

We say that, for example, types without functions in them mode-cross the portability
axis: values of these types do not care what their portability is.

However, when we have an axis with more than two modes in it, the situation is
somewhat more subtle.

Let's start with the statefulness axis: `stateless < observing < stateful`. This
axis is a future axis, supporting the `stateless` and `observing` modalities. That
is, we can declare these types:

```ocaml
module Stateless = struct
  type 'a t = { stateless : 'a @@ stateless } [@@unboxed]
end

module Observing = struct
  type 'a t = { observing : 'a @@ observing } [@@unboxed]
end
```

We can thus store a `(int -> int) Stateless.t` and remember that our function is
surely stateless. In addition, if we have a `f : (int -> int) Stateless.t @ stateful`,
we can mode-cross `f` to become `stateless`.

What about a `(int -> int) Observing.t`? That function is not quite stateless: it might
still read mutable state. So if we have a `g : (int -> int) Observing.t @ stateful`,
we can treat it as `observing`, but definitely not `stateless`. This is *partial*
mode crossing, where we can cross from `stateful` to `observing`, but not all the
way to `stateless`.

The situation is slightly different with past axes, like visibility: `read_write < read < immutable`.
Here, the modality types are as follows:

```ocaml
module Immutable = struct
  type 'a t = { immutable : 'a @@ immutable } [@@unboxed]
end

module Read = struct
  type 'a t = { read : 'a @@ read } [@@unboxed]
end
```

To make this interesting, we'll need a partially mutable type:

```ocaml
type person = { name : string; mutable age : int }
```

Now, if we have a `person Immutable.t`, we'll be able to access the `name` field
but not the `age` field. Furthermore, if we have a function `f : person Immutable.t @ read_write -> ...`,
we can pass it any `p : person Immutable.t`: the `Immutable.t` wrapper prevents accessing
`age`, even if `f` treats its argument as `read_write`. Effectively, it is as if we really
had `f : person Immutable.t @ immutable -> ...` (even though normally we cannot arbitrarily
raise the mode of a function argument).

What about a `person Read.t`? Here, we can read the `age` field but cannot mutate
it. A function `g : person Read.t @ read_write -> ...` can safely accept `p1 : person Read.t @ read`:
we know that `g` cannot actually write to the `age` field, and so our `p1` is indeed only
read from, never written to. However, what about a `p2 : person Read.t @ immutable`? Here,
the mode `immutable` says that we cannot access `p2`'s `age` field at all. If we pass
`p2` to `g`, `g` might read that field, violating our requirement. Thus we cannot call
`g p2`. It is as if we really had `g : person Read.t @ read -> ...` (even though normally we cannot
arbitrarily raise the mode of a function argument). This is *partial* mode crossing again:
we can cross the expectation of `read_write` to an expectation of `read`, but not all the
way to an expectation of `immutable`.

The challenging aspect here is that mode crossing on past axes is on *expectations*, not
on values. Contrast with statefulness: a `h1 : (int -> int) Observing.t @ stateful` can
cross to `observing`, but not all the way down to `stateless`; and a `h2 : (int -> int) Observing.t @ observing` cannot mode cross to anything better.
On the other hand, if we have a `q1 : person Read.t @ immutable`, we *cannot* cross to 
to `read`: that would newly allow reading! If we have a `q2 : person Read.t @ read`, we *can*
cross this to `read_write` safely.

Accordingly, and admittedly somewhat confusingly, mode crossing just works differently
on future axes than past axes. One way to think of this is to say that, for a future axis, a modality
puts an upper bound on the mode of values (when we have a `@@ observing`, a value at any mode
greater than `observing` can just cross down to `observing`), while for a past axis, 
a modality puts a lower bound on the mode of expectations (when we have a `@@ read`, a function
argument at any mode less than `read` can just cross up to `read`).
