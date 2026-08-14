# Vox bigint

An arbitrary-precision signed integer module, in pure OCaml, with a real
executable implementation.

Specifications need mathematical integers. OCaml `int` is modelled as
`Bitvec 63`, so `x >= 0` does not imply `x + 1 >= 0`, and a specification that
wants unbounded arithmetic has nowhere to say so. `Bigint.t` is that place.

This piece is standalone. It depends on nothing else in vox, and nothing else in
vox depends on it until the translation piece maps its operations to the solver.

## No GMP, and not by preference

Zarith is the standard OCaml answer and it is out of reach here.

- Nothing in the oxcaml tree references zarith or gmp: no `.ml`, `dune`,
  `.opam` or `configure.ac` hit.
- Zarith is not in the opam switch, though `libgmp.so` is on the box.
- No `otherlibs` entry links an external library. The only link flags in the
  tree are `-ldopt`, `-linkall` and `-lthreadsnat`; `str` carries its own regex
  C in-tree and `unix` uses libc. A GMP dependency would be the first of its
  kind and would break bootstrap and cross-compilation.
- OCaml's own `Num` was removed from the distribution in 4.06, so the project
  has deliberately stopped shipping a bignum.

Performance is not the reason to want GMP here anyway. The solver never runs
this code: `Vox_builtin` maps `Bigint.add` by path to SMT `Int` addition, so
proofs are about mathematical semantics regardless of the implementation. The
implementation runs only where a specification is executed.

## It lives in core stdlib

Not `otherlibs/stdlib_alpha`, which is otherwise the right home for something
experimental.

The reason is runtime checking of specifications, which is planned. Runtime
checks are code the compiler emits into a user's module. If the support lives in
`stdlib_alpha`, compiler-generated code carries a link dependency the user never
asked for. This is the same reason `CamlinternalFormat` and `CamlinternalLazy`
are in stdlib rather than beside them.

It is public rather than `Camlinternal`-prefixed, because specification authors
write `Bigint.t` in source.

The cost is real: this joins the public stdlib API and touches `StdlibModules`,
`.depend`, `dune`, `stdlib.ml` and `stdlib.mli`. Worth naming so nobody thinks
it was chosen for convenience.

## The API is determined by the solver

Every exported operation has to be interpretable as an SMT `Int` operation.
Anything else becomes an uninterpreted function, and specifications that use it
silently stop being provable.

    zero one of_int
    is_zero equal compare lt le gt ge
    neg abs add sub mul

Note the absences. No division or modulo, which bring partiality on a zero
divisor and nonlinear reasoning. No exponentiation. vox2 leaves these out too,
and its `int` division needed a separate constant-divisor restriction on the
oxsmt path, which is the shape of the trouble.

Adding a convenient function later is a way to create unprovable specifications
without noticing, so the rule belongs at the top of the `.mli`, not in a commit
message.

Three functions are runtime-only conveniences with no interpretation, and should
be marked as not for use in specifications:

    to_int_opt to_string of_string

## Representation

Two constraints. The kind must be `immutable_data`, so the type crosses modes
and can be used at `@ logical`. And the representation must be canonical.

Sign-magnitude over `int` limbs. Two candidates for the magnitude:

- `int list`, which vox2 uses. Simple and obviously correct.
- `int iarray`, which is the more idiomatic immutable sequence in OxCaml and
  better on allocation and locality.

Runtime checking makes the second more attractive than it would otherwise be,
since checks run in real programs rather than only in spec tests. Either
satisfies the kind requirement, so this is the implementer's call against how
much the arithmetic code suffers.

Decision: `int iarray`. The arithmetic builds each magnitude in a mutable
scratch array and freezes it through a single `trim` helper, so the loops are
the textbook carry/borrow loops rather than vox2's structural-recursion
variants, whose canonicity-on-unwind subtlety in `subtract_magnitude` is
exactly the kind of cleverness this module should not need. Limbs are
half-word (`radix_bits = (Sys.int_size - 1) / 2`) so limb products with their
carries fit in `int`.

## Canonicity is an invariant, and it is load-bearing

No leading zero limbs, and zero has sign 0 with an empty magnitude. Canonical
representation means polymorphic equality agrees with `equal`.

Polymorphic *compare* does not agree with mathematical order, because it
compares the representation. Document that in the `.mli`; it is the kind of
thing that silently produces a wrong sort order years later.

## The oracle is not optional

The solver reasons about mathematical integers. Runtime checks run this code. If
the two disagree, a guarded `assume` can pass at runtime while the obligation it
was standing in for concerned different numbers. That is a soundness gap in the
`assume` mechanism, so the implementation's correctness matters even though its
speed does not.

Test against an independent oracle rather than against itself:

- machine `int` arithmetic for values in range, which covers carries, signs and
  the boundaries cheaply
- a deliberately naive decimal-string implementation for large values, written
  for obviousness rather than speed
- algebraic properties: commutativity, associativity, distributivity, `sub` as
  `add` of `neg`, `abs` of `neg`

## Tests

- the canonicity invariant holds after every operation, including the ones that
  can produce zero: `sub` of equals, `mul` by zero, `neg` of zero
- oracle agreement on the three fronts above
- `to_string` and `of_string` round-trip, and `of_string` rejects redundant
  leading zeroes and bare `-`
- `to_int_opt` at `min_int` and `max_int`, and one past each
- comparison is a total order consistent with `equal`
- polymorphic equality agrees with `equal`, which is the canonicity invariant
  observed from outside

## Deferred

The mode annotations. vox2's signature is
`val add : t @ logical -> t @ logical -> t @@ total` throughout, which needs the
totality piece. Land the module unannotated and add them after.

The `Vox_builtin` mapping from these operations to SMT `Int`, which needs the
solver interface and the translation piece.

Runtime checking of specifications, which is what makes the executable
implementation earn its keep, and which is a piece of its own.
