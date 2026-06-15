# Unique Implies Uncontended

`unique_implies_uncontended` is conceptually part of contention crossing. It
is not an independent ordinary axis, because full contention crossing already
implies it.

This note uses the ikind restriction order. In this order, adding a bit means
adding a restriction, not adding a crossing capability.

## Combined Contention/UIC Restrictions

Use three restriction bits:

```text
s = shared-side contention crossing is restricted
k = corrupted-side contention crossing is restricted
u = unique-to-uncontended crossing is restricted
```

The semantic invariant is:

```text
u => s or k
```

Equivalently, if both sides of contention crossing are unrestricted, then UIC
is unrestricted too. The raw point `{u}` is not semantic: it says that full
contention crossing is allowed, but unique-to-uncontended crossing is still
restricted.

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

The normalization operation removes the unsupported `u` restriction:

```text
norm(s,k,u) = (s, k, u && (s || k))
```

So:

```text
norm({u}) = {}
```

## Joins, Meets, and the Extra Point

In the restriction view, ikind join adds restrictions. On the raw 8-point
representation:

```text
join = union
meet = intersection
```

Semantic points are closed under join:

```text
if u is present in x union y, then it came from x or y,
and that input already had s or k
```

Semantic points are not closed under raw meet:

```text
{s,u} meet {k,u} = {u}
norm({u}) = {}
```

This is the same issue as the capability-view statement that full contention
crossing implies UIC, but expressed in ikind order.

A literal 7-point semantic lattice would use normalized meet. That lattice is
not distributive. For example:

```text
{s,u} meet ({s} join {k,u}) = {s,u}
({s,u} meet {s}) join ({s,u} meet {k,u}) = {s}
```

The current LDD machinery relies on a distributive coefficient lattice, so the
implementation cannot simply replace the coefficient lattice with this 7-point
semantic lattice.

Keeping the raw 8-point lattice is conservative if the extra point is treated
as a real restriction internally:

```text
raw {u} is more restrictive than semantic norm({u}) = {}
```

That can cause false failures, but not false successes. The precision loss
happens when raw meet creates `{u}` and no later normalization removes it.

## The Artificial-Uniqueness Boundary

The `@@ aliased` modality makes uniqueness crossing artificial. It should not
let direct uniqueness evidence justify UIC. However, it should preserve the
fact that full contention crossing implies UIC.

In restriction form, the operation for this boundary is:

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

- if the contents fully cross contention, `@@ aliased` does not restrict UIC;
- if the contents have any contention restriction, `@@ aliased` restricts UIC;
- direct UIC of the contents does not pass through `@@ aliased`.

This is the restriction-order version of "drop artificial UIC, then recover it
when full contention crossing justifies it".

## Separate `x` and `f(x)` Variables

The current ikind LDDs express terms using joins, meets, constants, and
variables. The operation `f` is not just a constant mask on `x`: it computes the
output `u` bit from the input `s` and `k` bits.

One way to preserve precision is to keep two views of a type variable:

```text
x     = the ordinary view
f(x)  = the artificial-uniqueness view
```

These are not independent semantic variables. Assigning a semantic value to
`x` determines the value of `f(x)` by:

```text
f(x) = norm(x union {u})
```

A paired LDD normal form could keep `x` and `f(x)` adjacent and enforce the
local dependency. In restriction order, the useful equations are:

```text
x <= f(x)
x join f(x) = f(x)
x meet f(x) = x
```

but with the important non-equation:

```text
u(f(x)) is not u(x)
```

For example, `{s}` has no `u` restriction, but `f({s}) = {s,u}`. So UIC
being unrestricted for `x` does not imply that UIC is unrestricted for `f(x)`.

The point of the separate variable is to avoid pretending that `f` is a simple
meet-with-mask or join-with-constant operation in the coefficient lattice.
The representation can stay distributive while entailment or normalization
remembers that `f(x)` is the dependent artificial-uniqueness view of `x`.

## Mutable Data

Mutable fields contain an implicit `@@ aliased` modality on their payloads.
Therefore the payload dependency should be viewed through `f`.

That means:

- the mutable cell itself may still have UIC;
- direct UIC of the payload does not pass through the mutable field;
- payloads that fully cross contention still support UIC through the mutable
  field.

This is why the distinction matters for mutable data. Mutable data is exactly
where unique-to-uncontended is useful, but mutable payloads also introduce the
artificial-uniqueness boundary that prevents direct UIC from passing through.
