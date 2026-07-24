# Refactoring plan for backend/ssa/loop/

## Goal

Split the SSA loop passes so that their pure decision kernels — the parts
that do not depend on the semantics of SSA operations — live in separate,
self-contained files. Those files become candidates for formal verification
(Gospel/Cameleer/Why3 on the OCaml directly, or a port to Coq), following the
model set by `fourier_motzkin.ml`, which is already purely arithmetic and
fully isolated. A second goal is to remove straight duplication between the
passes so the subsequent full code review has a smaller surface.

Throughout, only one direction of each analysis result is load-bearing:
`entails = true` and `feasible = false` license rewrites; the conservative
direction is always safe. Any formal specs should encode exactly that
asymmetry.

## New files

### 1. `loop_comparisons.ml` — comparison-direction logic (pure)

Depends only on `Cmm` (the `integer_comparison` type and its
`negate`/`swap` involutions) and `Fourier_motzkin.Affine`. Collects the
hand-rolled comparison reasoning currently duplicated across three passes:

- `facts` (from `affine_ssa.ml`'s `cmp_facts`): signed comparison, possibly
  negated, to the affine inequalities it implies. A sign error here is an
  unsound bounds-check elimination.
- `direction` / `direction_of_step` (from `termination.ml`): the monotonic
  direction of a constant-step induction variable.
- `continue_terminates` (from `termination.ml`): whether monotonic
  progression in a direction must eventually falsify a continue-condition.
- `oriented_continue_comparison`: the swap/negate normalisation that puts
  the IV on the left of the continue-condition — currently written out
  independently in `termination.ml` and `lftr.ml` (where drift between the
  two copies would be an unsoundness).
- `is_signed_order`, `continues_while_upper_bounded` (from `lftr.ml`).

Everything here is a function over a finite domain (10 comparisons x 2
sides x 2 polarities x 2 directions), so beyond proof it can be verified by
exhaustive enumeration against a reference semantics. The wrap-around
question for `continue_terminates` (`Up`/`Clt`: monotone increase can in
principle wrap) is exactly the proof pressure this module should surface;
flagged for the full review, not resolved here.

### 2. `affine_expr.ml` — affine-expression AST and evaluators (pure)

Depends only on `Fourier_motzkin`. A small AST reifying "the affine view of
a machine-integer value":

    Const | Atom of int | Add | Sub | Scale of int
    | Shr_atom of { atom; arg; bits }   (* atomized right shift *)

with three pure evaluators:

- `to_affine : t -> Affine.t * Affine.t list` — the affine form plus the
  side inequalities contributed by atomized right shifts
  (`2^k*t <= a <= 2^k*t + 2^k - 1`), previously built inline in
  `affine_ssa.ml`'s `linearize`;
- `coeff_of_atom : int -> t -> int option` — the coefficient of one atom,
  `None` when the atom occurs in a non-affine position (inside a shift);
  previously interleaved with SSA pattern matching in
  `strength_reduction.ml`'s `biv_coeff`;
- `as_const : t -> int option` — constant-ness of the affine form, used to
  detect constant multiplicands of fused multiply-adds.

The verification statement is clean: both evaluators agree with the AST's
denotational semantics.

`affine_ssa.ml` keeps the single SSA-to-AST recognizer (the only part that
pattern-matches `Operation`/`Arch` shapes), parameterized by a per-caller
policy so the two existing recognizers' behaviour is preserved exactly:

- `linearize` policy: shifts up to 16 bits, right shifts atomized,
  multiplies not decomposed, unrecognised values interned as atoms;
- `biv_coeff` policy (used by strength reduction): target-IV and
  loop-invariant leaves classified by callbacks, shifts up to 62 bits,
  constant multiplies decomposed, unrecognised values reject the candidate.

Known micro-deviations (both sound, neither expected to change any test):

- the constant-multiplicand test for fused multiply-adds is unified on
  "affine form is constant" (previously `linearize` tested the linearized
  form but `biv_coeff` only accepted literal `Const_int`s — the unified
  test is strictly more general for the latter);
- side inequalities for right shifts inside a fused-multiply operand that
  ends up atomized are no longer emitted (previously pushed and then the
  operand discarded); such facts only mention atoms unused elsewhere, so
  Fourier-Motzkin resolves them away without affecting any entailment.

### 3. `natural_loop.ml` — natural-loop discovery (graph-pure functor)

A functor over a small abstract `Graph` signature (`node`, `Set`, `Tbl`,
`equal`, `nodes`, `successors`, `predecessors`, `dominates`) containing the
first half of `induction_var.ml`: back-edge detection (`u -> v` with `v`
dominating `u`), `natural_loop_body` (header plus every node reaching a
back-edge source without passing through the header), and `find_loops`.
Also home to `edge_dominates` — the "taken branch edge dominates target"
sufficient condition (successor dominates target and has the branch source
as sole predecessor) currently inlined in `affine_ssa.ml`'s `guards_at`.

Zero instruction content; classic textbook-verifiable graph theory.
`Induction_var.Make` instantiates it with the SSA block graph and
re-exports `loop` transparently, so no consumer of `IV.loop` changes.

### 4. Shared SSA loop utilities (hygiene, not verification)

Extend `Induction_var.Make` (already the shared analysis module) with the
helpers currently copy-pasted between passes, and delete the copies:

- `op_def ()` — memoized op-id-to-defining-block table. Four copies today
  (`induction_var`, `bounds_check_elim`, `lftr`, `strength_reduction`; the
  latter two compute it eagerly at functor application). Memoization also
  stops `Termination.analyze` and `IV.analyze` rebuilding it per call.
- `is_const`, `const_int` — four copies today.
- `available_at` — verbatim in `lftr` and `strength_reduction`.
- `signed_step` — in `lftr`; re-derived inside `termination`'s
  `direction_of_biv`.
- `back_edge_set`, `entry_predecessors`, `preheader_of` — the
  non-back-edge-predecessor computation, four variants today.

`Termination.find_exit_branch` additionally returns the exit target, which
subsumes `delete_empty_loops.ml`'s `exit_target_of_loop` (same match,
different projection).

Not merged: `bounds_check_elim.ml`'s `atom_invariant` stays separate from
`is_loop_invariant` — it is deliberately more conservative (rejects
`Proj`/`Tuple`/trap instructions rather than approving them), and that
difference is soundness-relevant.

### 5. `loop_chains.ml` — producer/consumer chain logic (pure)

The maximal-chain grouping (`chains`) and largest-odd-prefix selection
(`pick`) from `loop_fusion.ml`, over an abstract `consumes` relation and
abstract per-element predicates. The underlying theorem — a chain of k
reversing maps equals `rev_map` of the composition iff k is odd — is a tidy
list-theory lemma; the extracted functions are the code's use of it.

## Non-goals / deferred to the full code review

- The pass drivers and `Ssa_reducer`-based rewriters stay where they are:
  they are inherently about SSA operation semantics and graph mutation.
- `linearize` builds affine forms with the unchecked `Affine.add`/`scale`;
  nested shifts could overflow coefficients before the overflow-checked
  Fourier-Motzkin path is reached. Not fixed here; the split makes the
  fix (and its spec) local to `affine_expr.ml`.
- `continue_terminates`'s wrap-around caveat (above).
- Enabling constant-multiply decomposition for `linearize` (a precision
  improvement for bounds-check elimination) is deliberately left off to
  keep this refactoring behaviour-preserving.

## Validation

Behaviour-preserving intent: `make -s boot-compiler`, the
`testsuite/tests/backend/ssa` runtime tests, `flambda2/examples`, and the
full `make -s test` must all pass unchanged (modulo the two documented
micro-deviations, which should not be observable).
