# Totality piece — final report

## Summary

Branch `jujacobs/vox/totality` (worktree `totality/dev`), 4 commits:

- `d5642cb0f6` — the piece: totality/logicality axes end-to-end (lattices,
  morphisms, coupling registration, surface syntax, kind modifiers, crossing,
  closure/hereditary/(Rec) rules, primitive allowlist, logicality on mutable
  projection/mutation; ~880 lines of compiler changes,
  `testsuite/tests/vox/totality.ml`, and `design-docs/totality.md` with a
  decision log)
- `be08ab91e6` — principal-mode baseline promotions
- `b7cd48b7fe` — review-loop round 1 fixes and pins
- `c00d624b75` — review-loop round 2: consolidate the lock walk, simplify
  toplevel pinning

**Tests**: the piece test is green; the full suite is 2438 passed / 5 failed,
all 5 pre-existing environmental failures (bytecode-ocamlc magic mismatch;
reproduced on the base branch): `formatting/test_locations.ml`,
`parsetree/test_ppx.ml`, `templates/basic/test.ml`,
`tool-ocamlc-stop-after/stop_after_typing_impl.ml`,
`typing-zero-alloc/cmi_test.ml`.

## Review loop

Two codex + two claude lanes, each in its own worktree off the branch
(`totality/review/<name>`). codex-general delivered a full report
(`totality/review/codex-general/report.md`); the codex defect-search lane was
killed twice by an upstream content filter mid-report but its probe fixtures
were salvageable and were rerun and triaged; the first claude pair stalled
>60 minutes and was replaced per the kill rule; the compact retry delivered.

Accepted after reproduction:

- a mutation-caught missing `Pexp_setvar` fixture (no test exercised the
  instance-variable assignment arm);
- a disproven decision-log equivalence claim for (Rec) (group-wide floor is
  observable on non-recursive siblings) — doc fixed, behaviour pinned;
- one lock-walk mechanism instead of three: `Env.walk_locks_for_totality`
  reuses `walk_locks`, fixing a latent `Const_closure_lock` skip and gaining
  the standard used-inside-the-function error context;
- simpler toplevel pinning (dropped an upward logicality freedom no twin
  axis has; unannotated bindings are back on the constant-legacy path);
- table-derived `of_axis_set` instead of hand-computed masks (reviewer
  checked all 2^13 inputs for equivalence first); stale comments fixed.

Rejected after reproduction: three "blocking soundness holes" (partial
parameter call, `%apply`/`%revapply`, method send on a parameter) that are
exactly how the portability twin behaves — capture-based, parameters exempt —
pinned by fixtures instead of "fixed".

## Decision points needing confirmation

All recorded in the decision log at the end of `design-docs/totality.md`:

1. **Capture-based contract.** `f @ total` may call a `partial` *parameter*;
   safety is compositional (a total context cannot supply the partial
   argument — submoding stops partial values at every entry into the total
   fragment). Twin parity verified: `portable` accepts the same three shapes.
   One reviewer read `total` as call-oriented; the alternative
   (application-site constraint at `Pexp_apply`) would also reject the doc's
   "returned, never applied" recursion test the other way.
2. **"Total implies stateless" is not implemented.** As annotation
   defaulting it makes every totality rejection surface as a statefulness
   error (legacy values violate stateless first) and forces the primitive
   allowlist to claim statelessness too; the observable content is already
   delivered by the effect arms plus the logicality bump on captures.
3. **Known gap, pinned by a fixture**: `module rec M : sig val loop : int ->
   int @@ total end = struct let loop x = M.loop x end` — the signature
   claim justifies its own recursive call; termination is inductive, so the
   self-assumption is unsound (portability tolerates it, being coinductive).
   Fix (weaken totality modalities in the recursive-approximation
   environment, the `module rec` analogue of (Rec)) sketched, deferred.
4. **(Rec) is group-wide**: a non-recursive `and`-sibling is also partial,
   because the base typechecker gives the group a single mode variable.
   Per-member precision needs per-binding group mode variables — follow-up.
5. **`@@ physical` rejected / `nonlogical` spelling** is doc-mandated but
   breaks the existing convention: `@@ uncontended` and `mod partial` parse
   today with a redundancy warning rather than erroring.
6. vox2's `filter_axes_without_kind_modalities` was *not* ported (it would
   let `type t : value mod total = int -> int` typecheck), and two latent
   vox2 defects (`less_or_equal` / `get_max_axes` missing the new axes,
   which made a lone `mod total` invisible to kind printing and equality)
   are fixed rather than inherited.

## Process note

A test was briefly run in the type-formers worktree before the
worktree-ownership rule landed; it was killed immediately, was a single
read-only-ish test run, and their tree was left untouched.
