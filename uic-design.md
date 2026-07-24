# Unique Implies Uncontended

This note describes the desired `unique_implies_uncontended` feature, without
choosing a particular approximation strategy. It then records the implementation
attempt on this branch and the solver problem we ran into.

## Goal

The desired feature is:

```text
for types that have UIC:
  a value at mode unique may be used at mode uncontended
```

The intended reasoning is ordinary semantic reasoning about references:

```text
if there is only one reference to a value, then there are no cross-thread
references to that value
```

This is a type-specific mode crossing property. It should not apply to every
type. In particular, it should not apply when the only reason a value appears to
cross uniqueness is an artificial uniqueness crossing introduced by a modality
such as `@@ aliased`.

## Axes

There are three concepts that must stay separate:

```text
uniqueness:
  whether the value is unique or aliased

contention:
  whether the value is uncontended or contended

UIC:
  whether uniqueness is valid evidence for uncontendedness for this type
```

UIC is not the same thing as uniqueness. UIC says whether this implication is a
valid crossing step:

```text
unique => uncontended
```

UIC is also not the same thing as contention crossing. Full contention crossing
already makes UIC unnecessary: if a type already crosses contention directly,
then a `contended` value can become `uncontended` without using uniqueness.

## Desired Behavior

For a type crossing operation, UIC should behave like this:

```text
if the type has UIC and the value is unique:
  strengthen the contention mode to uncontended

otherwise:
  do not use uniqueness to strengthen contention
```

This should be a forward use of uniqueness evidence. It should not make the
type checker silently infer hidden uniqueness merely because a context requires
`uncontended`.

For example, this is the desired positive shape:

```ocaml
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()

let ok (x : t @ unique contended) =
  use_uncontended x
```

when `t` has UIC.

This is the shape we do not want:

```ocaml
let f (x : t @ contended) =
  use_uncontended x
```

being accepted by silently strengthening `x` to `unique` in a way that is not
visible in the inferred type.

## Artificial Uniqueness

The `@@ aliased` modality creates an artificial uniqueness crossing. It makes a
type cross the uniqueness axis even when that does not mean the runtime value is
actually uniquely referenced.

Therefore:

```text
artificial uniqueness must not justify UIC
```

The desired rule for a payload behind `@@ aliased` is:

```text
payload @@ aliased has UIC iff payload crosses contention directly
```

Equivalently:

```text
@@ aliased:
  uniqueness crossing: fully crosses
  contention crossing: same as the payload
  UIC: enabled only if the payload already crosses contention
```

This is the critical separation:

```text
the type can cross uniqueness,
but that crossing is not valid evidence for uncontendedness
```

## Other Modalities

The intended modality rules are:

```text
@@ contended:
  enables UIC, because the type now crosses contention directly

@@ aliased contended:
  enables UIC, because the explicit contention modality gives direct contention
  crossing

@@ aliased:
  enables UIC only when the inner type already crosses contention

modalities on other axes:
  preserve UIC
```

The important direction issue is that `@@ uncontended` is effectively the
identity for this purpose, while `@@ contended` is the non-identity contention
modality that gives contention crossing.

## Mutable Data

Mutable payloads have an implicit `@@ aliased` boundary.

Therefore, for mutable data:

```text
'a ref has UIC iff 'a crosses contention directly

mutable record fields behave the same way:
  the record may have UIC through a mutable field only if the field payload
  crosses contention directly
```

Atomic mutable fields are different when the atomic field itself gives direct
contention crossing. In that case, the payload does not disable UIC for the
outer value merely by being behind a mutable field.

## Restriction View

In the ikind restriction order, join adds restrictions. Higher values are more
restrictive. For the combined contention/UIC part, use these restriction bits:

```text
s = shared-side contention crossing is restricted
k = corrupted-side contention crossing is restricted
u = UIC is restricted
```

The semantic invariant is:

```text
u => s or k
```

The raw point `{u}` is not semantic. It says full contention crossing is
allowed, but UIC is restricted. If full contention crossing is allowed, UIC is
irrelevant and should also be allowed.

The semantic points are:

```text
{}        no contention restriction, no UIC restriction
{s}       shared-side contention restricted, UIC unrestricted
{k}       corrupted-side contention restricted, UIC unrestricted
{s,k}     contention restricted, UIC unrestricted
{s,u}     shared-side contention restricted, UIC restricted
{k,u}     corrupted-side contention restricted, UIC restricted
{s,k,u}   contention restricted, UIC restricted
```

The normalization operation is:

```text
norm(s,k,u) = (s, k, u && (s || k))
```

so:

```text
norm({u}) = {}
```

The semantic points are closed under raw join, but not under raw meet:

```text
{s,u} meet {k,u} = {u}
norm({u}) = {}
```

A literal 7-point semantic lattice would use normalized meet. That lattice is
not distributive. The current LDD machinery relies on a distributive coefficient
lattice, so replacing the coefficient lattice with that 7-point lattice is not a
small local change.

Using the raw 8-point lattice is conservative if `{u}` is treated as a real
restriction internally. It can lose precision, but should not accept an unsafe
program solely because of this extra raw point.

## Artificial-Uniqueness Function

For an artificial uniqueness boundary such as `@@ aliased`, the desired function
on restriction points is:

```text
f(x) = norm(x union {u})
```

Equivalently:

```text
s(f(x)) = s(x)
k(f(x)) = k(x)
u(f(x)) = s(x) || k(x)
```

So:

```text
f({})        = {}
f({s})       = {s,u}
f({k})       = {k,u}
f({s,k})     = {s,k,u}
f({s,u})     = {s,u}
f({k,u})     = {k,u}
f({s,k,u})   = {s,k,u}
```

This says:

```text
if the payload fully crosses contention:
  artificial uniqueness does not restrict UIC

if the payload has a contention restriction:
  artificial uniqueness restricts UIC

direct UIC of the payload does not pass through artificial uniqueness
```

The `f` operation cannot be expressed as just meeting or joining with a constant
in the existing ikind-shaped LDD terms. It computes the output UIC bit from the
input contention bits.

## What This Branch Did

This branch adds a UIC bit to the ikind/axis-lattice path and carries it into
`Mode.Crossing.t` as:

```ocaml
unique_implies_uncontended : bool
```

An earlier implementation modeled UIC as a generic mode solver morph:

```ocaml
Value.Monadic.unique_implies_uncontended_unhint
```

That morph is represented in the mode solver as:

```text
unique_implies_uncontended_right
```

That morph's forward action was:

```text
contention := meet contention
  (if uniqueness is Unique then Uncontended else Contended)
```

So, forward:

```text
unique contended => unique uncontended
aliased contended => aliased contended
```

That operation was removed from the generic solver morph language. There are no
`unique_implies_uncontended_left` or `unique_implies_uncontended_right` morphs
now.

The current first-pass implementation keeps UIC as a crossing bit only. In
`Crossing.apply_left`, it:

```text
1. applies ordinary mode crossing
2. checks whether the crossed value is already definitely unique
3. if so, strengthens contention to uncontended
```

This uses an existing valid solver operation for the final contention
strengthening. It does not add a UIC-specific adjoint to the generic solver.

This is safe but not precise. It only fires when uniqueness is visible at the
crossing operation. It still misses cases where default uniqueness is discovered
later, such as:

```ocaml
let int_ref_with_default_unique (x : int ref @ contended) =
  use_uncontended x
```

The test file records those missed positive cases with CRs.

## Problem Encountered

The existing mode solver treats morphs as adjoint-based rewrites. For a
constraint of this form:

```text
a <= f(v)
```

`submode_cmv` first checks:

```text
a <= f(v.upper)
```

If that succeeds, it assumes `a` is in the downward closure of `f`'s image and
rewrites the constraint using the left adjoint:

```text
left_adjoint(f)(a) <= v
```

This assumption is built into the solver. The solver calls `Result.get_ok` after
the rewrite because failure is supposed to be impossible when the adjoint is
valid.

We tried making the UIC left-adjoint behave like an identity-like backward
approximation. That crashed on this example:

```ocaml
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()

let int_ref_with_default_unique (x : int ref @ contended) =
  use_uncontended x
```

The internal failing shape was:

```text
a <= f(v)
```

with:

```text
a =
  aliased,uncontended,read_write,dynamic

f =
  unique_implies_uncontended_right
  . imply(unique,uncontended,read_write,static)
  . unique_implies_uncontended_right
  . imply(unique,uncontended,read_write,static)
  . unique_implies_uncontended_right
  . imply(unique,uncontended,read_write,static)
  . id

f(v).upper =
  unique,uncontended,read_write,static

v.upper =
  unique,contended,read_write,static
```

The image check passed:

```text
a <= f(v.upper)
```

because applying UIC to `v.upper` made the target uncontended.

Then the fake identity-like left adjoint produced:

```text
left_adjoint(f)(a) =
  aliased,uncontended,read_write,dynamic
```

and the solver tried to require:

```text
aliased,uncontended,read_write,dynamic <= v
```

But `v.upper` was only:

```text
unique,contended,read_write,static
```

So the source-side update failed, contradicting the solver invariant, and the
solver raised:

```text
Invalid_argument("result is Error _")
```

The important lesson is:

```text
an approximate backward operation is not the same thing as a solver adjoint
```

The `morph` API assumes adjoint-style equivalence strong enough to
rewrite constraints and mutate variable bounds. A forward-only or
backward-approximate UIC operation needs either a different solver concept or an
implementation that does not put the approximate operation into the generic
adjoint-based morph path.

## Desired Next Implementation

The desired implementation shape is a crossing-aware submode check rather than a
mode expression:

```text
ordinary crossed actual <= expected
```

or, if the only failing part is contention:

```text
UIC is enabled
and actual is already definitely unique
and uncontended satisfies the expected contention mode
```

The important property is:

```text
UIC may discharge a contention obligation using already-known uniqueness.
UIC must not create a new uniqueness obligation.
```

That check belongs above `Lattices_mono.morph`, near the existing places that
combine mode crossing with submode checks. It should not be represented as a
generic solver morph.
