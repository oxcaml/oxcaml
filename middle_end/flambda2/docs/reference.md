# Encyclopædia of Optimizations

This page is a reference of the optimizations and transformations performed by
the flambda2 middle-end. **It does not cover classic mode (`-Oclassic`)**.

Note that, while we intentionally try to describe the optimizations performed
by flambda2 as more-or-less atomic transformations on OCaml source code
(because we believe that is the form in which they are the most useful to OCaml
programmers), they in practice happen all at the same time in a lower-level
intermediate representation. This can sometime (hopefully rarely!) cause
discrepancies between what would be expected based on this document and what
actually happens when running the compiler -- if you notice such discrepancies,
this is still a bug (either of the code or the documentation); please report
them.

Most of the following optimizations are enabled by default in `-O2` mode; some
require `-O3` or specific flags which are documented below.

**Note:** This is currently a work-in-progress; not all optimizations are
covered yet.

## CPS conversion

The flambda2 optimizer does not operate directly on the surface OCaml syntax
but on an intermediate representation using continuations and
continuation-passing style (CPS). The first "optimization" performed by the
flambda2 optimizer is to convert code into CPS, and all other optimizations
can only be fully understood in the context of CPS.

The CPS conversion makes control flow explicit by replacing all control flow
joins with continuations, and all disjunctions with a `match`. Continuations
are annotated with the `[@local]` attribute (this is valid syntax recognized
and enforced by the compiler).

```ocaml
(* Before CPS conversion *)
let f x y =
  let z = if x then y else y + 1 in
  z + 2

(* After CPS conversion *)
let f x y ~ret ~exn:_ =
  let[@local] k1 z =
    let z' = z + 2 in
    ret z'
  in
  let[@local] k2 #() =
    let y' = y + 1 in
    k1 y'
  match x with
  | true -> k1 y
  | false -> k2 #()
```

Note that the full CPS conversion (as is actually performed by the flambda2
optimizer) makes code overly verbose and hard to read for humans. This
reference instead uses a lighter style, leaving code in direct (non CPS) style
when possible and only using CPS when required to show a specific optimization.

## Canonicalization

Canonicalization is a pervasive optimization that identifies all the names of a
given expression, and replaces all their usages with the first such name that
was defined (or a constant, if applicable).

This is the most basic of the optimizations performed by flambda2, and is
generally critical to cleaning up the code after other optimizations.

```ocaml
(* Before Canonicalization *)
let f x =
  let y = x in
  let z = (y, y) in
  let w, _ = z in
  Some w

(* After Canonicalization *)
let f x =
  let y = x in
  let z = (x, x) in
  let w = x in
  Some x
```

Canonicalization also takes into account equalities with constants learned in a
specific branch of a `match`.

```ocaml
(* Before Canonicalization *)
let f x =
  match x with
  | 0 -> x + 1
  | _ -> x

(* After Canonicalization *)
let f x =
  match x with
  | 0 ->
    (* x = 0 is known *)
    1
  | _ -> x
```

## Unboxing

Unboxing is an optimization that triggers when a continuation has a parameter
with a fixed shape (e.g. a tuple, or a boxed float), and at least one of the
calls to that continuation is an allocation (of the corresponding shape).

In this situation, Unboxing adds new arguments to the continuation for each of
the fields of the allocation, potentially introducing projections at the other
call sites. In conjunction with Canonicalization and Dead Code Elimination,
this often allows eliminating the allocation.

**Note:** [Variant unboxing](#variant-unboxing) is a stronger version of
unboxing that works when different call sites can have different shapes (e.g.
different tags, or sometimes being an immediate). We distinguish the two
because "regular" unboxing is conceptually simpler, although they are both
enabled by default.

```ocaml
(* Before Unboxing *)
let f x pair =
  let (a, b) =
    if x
    then (1, 2)
    else pair
  in
  a + b

(* After CPS conversion *)
let f x =
  let[@local] k (a, b) =
    a + b
  in
  if x
  then k (1, 2)
  else k pair

(* After CPS conversion, Unboxing, and Canonicalization *)
let f x =
  let[@local] k _ a b =
    a + b
  in
  if x
  then k (1, 2) 1 2
  else
    let (f, s) = pair in
    k pair f s

(* After CPS conversion, Unboxing, Canonicalization and Dead Code Elimination
 *)
let f x =
  let[@local] k a b =
    a + b
  in
  if x
  then k 1 2
  else
    let (f, s) = pair in
    k f s
```

## Variant Unboxing

Variant unboxing is an optimization that triggers when an optimization has a
parameter with multiple possible representations (e.g. an option), at least one
of the calls to that continuation is a constructor, and the shape of the
argument is known at all the call sites.

In this situation, Variant Unboxing adds a new argument to the continuation to
represent the _tag_ of the parameter, and new arguments for each of the fields
of the constructor.

Note that Variant Unboxing is highly dependent on the runtime representation of
values. It uses a special `__DUMMY__` value (which could be arbitrary) to
represent cases that are known to be impossible. Like Unboxing, its main use is
to eliminate allocations.

```ocaml
type t =
  | A (* runtime representation: 0 *)
  | B (* runtime representation: 1 *)
  | C of int (* runtime representation: block with tag 0 *)
  | D of string (* runtime representation: block with tag 1 *)

(* Before Variant Unboxing *)
let f b x y z =
  let[@local] hash v =
    match v with
    | A -> 7
    | B -> 13
    | C n -> 3 * Hashtbl.hash n
    | D s -> 11 * Hashtbl.hash s
  in
  if b
  then
    if y
    then hash (C x)
    else hash A
  else
    let t =
      if y
      then D ""
      else D z
    (* t is not an allocation, but we know it is a block with tag 1 *)
    in
    hash t

(* After Variant Unboxing, Canonicalization and Dead Code Elimination *)

type immediate_constructor = A_ | B_

type block_constructor = C_ | D_

let f b x y z =
  let __DUMMY__ : 'a. 'a = Obj.magic 0 in
  let[@local] hash is_int int_value tag c_field_0 d_field_0 =
    if is_int
    then
      match int_value with
      | A_ -> 7
      | B_ -> 13
    else
      match tag with
      | C_ -> 3 * Hashtbl.hash c_field_0
      | D_ -> 11 * Hashtbl.hash d_field_0
  in
  if b
  then
    if y
    then hash false __DUMMY__ C_ x __DUMMY__
    else hash true A_ __DUMMY__ __DUMMY__ __DUMMY__
  else
    let t =
      if y
      then D ""
      else D z
    in
    let D t_field_0 = t in
    hash false __DUMMY__ D_ __DUMMY__ t_field_0
```

## Match optimizations

The following optimizations are applied to `match` and `match`-like constructs.
The examples below are always given using `match`, but also apply to `if`.


### Match Elimination

Match Elimination is an optimization that triggers when the scrutinee of a
match is a constructor (or literal), and replaces the entire match with the
corresponding branch.

Match Elimination is a *standard* optimisation, it is always enabled.

When Match Elimination happens on a constructor with arguments, Canonicalization
simplifications allow to eliminate the allocation.

```ocaml
(* Before Match Elimination *)
let main ~is_none ~is_some a =
  let some_a = Some a in
  match some_a with
  | None -> is_none ()
  | Some x -> is_some x

(* After Match Elimination *)
let main ~is_none ~is_some a =
  let some_a = Some a in
  let Some x = some_a in (* irrefutable *)
  is_some x

(* After Match Elimination and Canonicalization *)
let main ~is_none ~is_some a =
  let some_a = Some a in
  is_some a
```

```ocaml
(* Before Match Elimination *)
let return x = Some x

let bind f x =
  match x with
  | None -> None
  | Some y -> f y

let main f v =
  bind f (return v)

(* After inlining, Match Elimination, and Canonicalization *)
let main f v =
  f v
```

In the following situation, Match Elimination does **not** trigger: the
scrutinee of the `match` is the result of an `if` statement, not a constructor.
To optimize this code, more powerful optimisations (Match Simplification or
Match Forwarding) are necessary.

```ocaml
(* No Match Elimination: always_some is not a constructor *)
let main ~is_none ~is_some b x y =
  let always_some = if b then Some x else Some y in
  match always_some with
  | None -> is_none ()
  | Some z -> is_some z
```

### Match Simplification

Match Simplification is an optimization that triggers when some branches of a
match are unreachable, and removes the unreachable branches. If there is only
one branch remaining, the entire match is removed and replaced with that branch.

Match Simplification provides fairly small runtime benefits compared to Match
Elimination, as the more complex control flow involved typically prevents
additional simplifications from being performed. Its primary use is for dead
code elimination.

```ocaml
(* Before Match Simplification *)
let main b =
  let c = if b then 0 else 1 in
  match c with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | _ -> failwith "does not fit on four bits"

(* After Match Simplification *)
let main b =
  let c = if b then 0 else 1 in
  match c with
  | 0 -> "zero"
  | 1 -> "one"
```
