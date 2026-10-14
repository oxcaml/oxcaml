# Code-hygiene findings from the believers/skeptics consolidation

Three small compiler-source findings surfaced during the adversarial formalism
campaign. They are **documentation/dead-code hygiene**, not soundness fixes, and are
recorded here for a human to action separately. **No compiler source was patched by
the consolidation** — the formalism docs describe the code as it currently is.

## 1. Vestigial `patch_unused_exn_bucket` (dead code)

- **Where:** `middle_end/flambda2/simplify/simplify_common.ml#patch_unused_exn_bucket`
- **Finding:** the function is dead code — both premise prongs are unsatisfiable.
  For a KEPT exn handler, `flow_acc.enter_continuation` unconditionally marks the exn
  bucket param, so `exn_value_is_used` is always true and the patch branch is never
  taken. For a DEMOTED handler, `clear_demoted_trap_action` strips the `Pop` FIRST
  (it runs before the patch), and `Apply_cont_expr.is_raise` requires a `Pop`, so the
  patch branch is again unreachable.
- **Recommendation:** mark vestigial or delete. Confirmed by Hume/Leibniz
  (`exn_demotion.ml`: `raise (E (x*41+1))` with a globally-unused bucket — the mul/add
  chain and the `Pmakeblock` both survive; the arg is never patched).
- **Formalism cross-ref:** `S.Rewrite.LetCont.DemoteExn` NOTES
  (`middle_end/flambda2/docs/formalism/10-simplify-rewrites.md`), case study
  `14-validation/exn_demotion.md`.

## 2. Stale comment naming a nonexistent Expr_builder region special case

- **Where:** `middle_end/flambda2/terms/flambda_primitive.ml`, the
  `effects_and_coeffects` classification comment (around lines 1661-1664).
- **Finding:** the comment claims `End_region` special cases live in "Simplify_let_expr
  and Expr_builder". The `Expr_builder` half is stale: all region-deletion decisions
  originate in `simplify_let_expr.ml#rebuild_let` (via `is_end_region_for_unused_region`).
  There is no `End_region` special case in `expr_builder.ml`.
- **Recommendation:** update the comment to name only `rebuild_let`.
- **Formalism cross-ref:** `INV.Simplify.EffectfulDeletionInventory` NOTES
  (`middle_end/flambda2/docs/formalism/13-soundness.md`).

## 3. Defensive `add_use_of_value_slot` would make DeadValueSlotCoherence structural

- **Where:** `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg` (the
  `Generated` path that materializes a `Project_value_slot` extra-arg
  unconditionally).
- **Finding:** `unbox_arg` materializes a value-slot projection without calling
  `DA.add_use_of_value_slot`, and nothing on the upwards path can record it
  (`UA.used_value_slots` is a creation-dacc snapshot). The whole-unit coherence of
  `used_value_slots` (five subsystems pruning from one set) currently holds only by an
  ACCIDENTAL alignment (survival ⇒ extra param `Used` ⇒ in-handler projection folded
  onto it ⇒ slot recorded via an in-unit `Need_meet` projection; imported slots
  exempt), not by local design.
- **Recommendation:** a defensive `DA.add_use_of_value_slot` in `unbox_arg` would make
  the coherence structural rather than alignment-based (a robustness improvement, not
  a bug fix — Hume's adversarial `slot_incoherence.ml` compiled clean).
- **Formalism cross-ref:** `INV.Simplify.DeadValueSlotCoherence`
  (`middle_end/flambda2/docs/formalism/13-soundness.md`) and `INV.ToCmm.SlotLiveness`
  (`.../20-to-cmm-soundness.md`), case study `14-validation/dead_value_slot_coherence.md`.
