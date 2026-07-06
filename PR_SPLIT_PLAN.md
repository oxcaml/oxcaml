# Splitting the `inlined-frames-params-and-vars` branch into PRs

This document is a guide for agents splitting this development branch into
separate, independently reviewable PRs.

- **Branch:** `inlined-frames-params-and-vars`
- **Base revision:** `a5b58d2abb90cd6f93f7952ef372d24fbd0ef985`
- **Target branch for PRs:** `4.11`
- **Full diff:** `git diff a5b58d2abb90cd6f93f7952ef372d24fbd0ef985..HEAD`
  (~4,900 insertions / ~700 deletions across 150 files)

**Do not split by cherry-picking commits.** The branch history is 24 commits
mostly titled "fixes"/"fix" and does not correspond to reviewable units. Split
by *content*: partition the cumulative diff into the PRs below, applying hunks
(or reimplementing small pieces) onto fresh branches off `4.11`.

## What the branch does

End-to-end DWARF debugging improvements for code compiled with Flambda 2,
in four intertwined strands:

1. **Phantom lets** — variables optimised away by Flambda 2 (including
   closure-captured variables accessed via `Project_value_slot` /
   `Project_function_slot`) get *phantom* bindings that flow from Flambda 2
   through Cmm, instruction selection, CFG, Linear, and finally become DWARF
   location descriptions, so the debugger can print optimised-out values.
2. **Inlined-frame parameters and variables** — each bound variable carries a
   `Debuginfo.t` stamped with the inlining stack plus a parameter/local
   classification, so variable DIEs are attached to the correct
   `DW_TAG_inlined_subroutine` and inlined parameters appear as formal
   parameters of the inlined frame.
3. **DWARF call-site information** — `DW_TAG_call_site` /
   `DW_TAG_call_site_parameter` DIEs and `DW_OP_entry_value` locations, so
   debuggers can recover argument values and identify call edges.
4. **Debuginfo-quality fixes** — several independent correctness fixes
   (spill/reload debuginfo, a stack-offset double-count in variable ranges, a
   `Debuginfo.Dbg.compare` bug, DIE attribute replacement, etc.).

## Glossary (concepts introduced on this branch)

| Concept | Defined in | Notes |
|---|---|---|
| `Bound_var.Is_parameter` | `middle_end/flambda2/bound_identifiers/bound_var.ml` | Flambda2-level: `Local_var \| Parameter {index} \| Implicit_parameter`. `Implicit_parameter` covers `my_closure`, `my_region`, `my_depth`, etc. |
| `Is_parameter` (backend) | `middle_end/is_parameter.ml` (moved from `backend/debug/`) | Backend/DWARF-level: `Local \| Parameter {index}`. **Distinct from the above** — do not conflate. `to_cmm_env.gen_variable` bridges the two. |
| `dbg` on `Bound_var` / `Bound_parameter` | `bound_identifiers/` | Per-bound-variable source `Debuginfo.t`, stamped with the inlining stack exactly once, on the downward simplify traversal (`Simplify_let_expr.simplify_let0`). |
| `Cmm.Cname_for_debugger` | `backend/cmm.ml` | New expression constructor naming a subexpression's value for the debugger (no runtime effect). Produced by `to_cmm_env.will_inline_simple/complex` for user-visible delayed bindings. |
| `phantom_available_before` | `backend/cfg/cfg_intf.ml`, `backend/linear.ml` | New per-instruction field: set of phantom-let-bound `Backend_var`s in scope. `None` when `-grestrict-to-upstream-dwarf`. |
| `fun_phantom_lets` | `Cfg.t`, `Linear.fundecl` | Map from phantom var to `(Provenance.t option, phantom_defining_expr)`. Accumulated during selection. |
| `Cfg.phantom_defining_expr` / `Linear.phantom_defining_expr` | `backend/cfg/cfg.ml`, `backend/linear.ml` | Mirrors of `Cmm.phantom_defining_expr` (which already existed at base), plus `*_optimised_out`. |
| `Reg_with_debug_info.Holds_value_of` | `backend/debug/reg_with_debug_info.ml` | Generalises "register holds var X" to `Var \| Const_int \| Const_naked_float \| Const_symbol \| Projection {base; field}` — feeds call-site parameter recovery. |
| Entry values | `dwarf_variables_and_parameters.ml`, `dwarf_operator.ml` | `DW_OP_entry_value` / `DW_OP_GNU_entry_value` locations for parameters whose registers were clobbered. |

Note: `Cmm.Cphantom_let`, `Cmm.phantom_defining_expr`, `Capply.callees`, and the
`-gno-upstream-dwarf` / `-gstartup` flags **already existed at base**. What is
new is everything that actually *produces and consumes* phantom lets, and the
Linear/CFG plumbing (base had no `Lphantom_*` at all).

## Ground rules for every PR

- Branch off `4.11`; verify with `make -s boot-compiler`, then `make -s test`;
  run `make -s fmt` before committing.
- Keep mechanical churn (adding `~dbg:Debuginfo.none`
  `~is_parameter:...local_var` to call sites) in separate commits — or
  separate PRs — from semantic changes, and say so in the PR description.
- Several defaults are flipped on this branch; keep them **off** until the
  final enablement PR (see PR 21) so intermediate PRs are behavior-neutral.
- Do **not** include the ad-hoc test files at the repo root
  (`inlined_test.ml` … `inlined_test8.ml`): they are manual repros. Either
  convert them into proper tests under `oxcaml/tests/` in the enablement PR or
  drop them. The untracked files (`inlined_frames_errors_*.ml`,
  `flambda2_grammar.tex/pdf`) are not part of the diff; ignore them.

---

## PR breakdown

Stage 0 PRs are mutually independent and can be prepared in parallel.
Later stages list their prerequisites explicitly.

### Stage 0 — independent preliminaries

#### PR 1: Fix `Debuginfo.Dbg.compare` to compare `dinfo_uid`
- **Files:** `lambda/debuginfo.ml`, `lambda/debuginfo.mli`
- `Dbg.compare_aux` previously ignored the pre-existing `dinfo_uid` field;
  now compares it (`Option.compare String.compare`). Standalone correctness fix.
- **Decision needed:** the branch also adds `Debuginfo.remove_outermost_frame`,
  which has **zero consumers anywhere in the repo** (verified by grep). Drop it,
  or hold it until a consumer lands. Do not ship dead code in this PR.
- Size: tiny.

#### PR 2: Move `Is_parameter` to `middle_end/` and add it to `Backend_var.Provenance`
- **Files:** `backend/debug/is_parameter.{ml,mli}` → `middle_end/is_parameter.{ml,mli}`
  (pure R100 move), `middle_end/backend_var.{ml,mli}`, root `dune` (move
  `is_parameter` into the middle_end module list).
- `Provenance.t` gains `is_parameter : Is_parameter.t`; `Provenance.create`
  gains `~is_parameter`; new accessors `Provenance.is_parameter` and
  `With_provenance.is_parameter` (returns `Is_parameter.local` for
  `Without_provenance`).
- The move exists because `middle_end/backend_var.ml` cannot depend on
  `backend/debug/` (layering). Module name is unchanged, so existing backend
  consumers are unaffected.
- The only external caller of `Provenance.create` is
  `middle_end/flambda2/to_cmm/to_cmm_env.ml` (`gen_variable`): update it to pass
  `~is_parameter:Is_parameter.local` as a neutral placeholder (real data
  arrives in PR 17).
- Size: small. Drop the incidental ocamlformat whitespace churn in
  `backend_var.ml` if it makes review noisier.

#### PR 3: Generic DWARF infrastructure additions (`dwarf_low` / `dwarf_high`)
- **Files:** `backend/debug/dwarf/dwarf_low/dwarf_operator.{ml,mli}`,
  `backend/debug/dwarf/dwarf_high/operator_builder.{ml,mli}`,
  `backend/debug/dwarf/dwarf_high/simple_location_description_lang.{ml,mli}`,
  `backend/debug/dwarf/dwarf_high/dwarf_attribute_helpers.{ml,mli}`,
  `backend/debug/dwarf/dwarf_high/proto_die.ml`
- Contents:
  - `implicit_value` gains a `Label of Asm_label.t` case; emission via
    `code_address_from_label`.
  - New operators `DW_op_entry_value_of_register` (DWARF-5, opcode 0xa3) and
    `DW_op_GNU_entry_value_of_register` (0xf3), with ULEB block emission and
    `uleb128_size_in_bytes` helper.
  - `Operator_builder`: `address_of_label`, `add_signed_const`,
    `entry_value_of_register` (dispatches on DWARF version).
  - Location language: `Lvalue.read_field_unguarded`,
    `Lvalue.offset_pointer_unguarded`, `Rvalue.address_of_label`,
    `Rvalue.read_field_unguarded`, `Rvalue.offset_pointer`,
    `Rvalue.entry_value_of_register`.
  - `Dwarf_attribute_helpers.create_call_return_pc_with_offset`.
- **Two behaviour changes to call out in the PR description:**
  - `Lvalue.offset_pointer` now uses `add_signed_const` (was unsigned) so
    negative offsets work.
  - `Proto_die.replace_all_attribute_values` now **mutates in place** instead
    of returning a functional copy — a correctness fix (the DIE is already
    linked into its parent; the copy caused stale attributes to be emitted).
- Target-agnostic; no dependency on anything else in the branch. New builders
  are simply unused until PRs 19/20.
- Size: ~150 lines.

#### PR 4: Strip platform symbol prefix from `DW_AT_linkage_name`
- **Files:** `backend/asm_targets/asm_symbol.{ml,mli}` (new
  `encode_without_prefix`), plus the small hunks in
  `backend/debug/dwarf/dwarf_ocaml/dwarf_abstract_instances.ml` and
  `dwarf_inlined_frames.ml` that switch `Asm_symbol.encode` →
  `encode_without_prefix` for linkage names.
- Fixes macOS `_`-prefixed linkage names. Self-contained.
- Size: tiny.

#### PR 5: Keep the startup object file for DWARF on macOS
- **Files:** `asmcomp/asmlink.ml` (new `keep_startup_obj_for_dwarf`; writes
  `camlstartup` to a stable `<output>.startup<ext_obj>` path and does not
  delete it, in both `link_actual` and `link_shared_actual`),
  `backend/debug/dwarf/dwarf_flags/dwarf_flags.ml` (**only** the
  `dwarf_for_startup_file` default change `false` → `Config.oxcaml_dwarf`),
  root `dune` (add `-gstartup` to the compiler's own `ocamlopt_flags`).
- Rationale: macOS debug maps point into object files, so DWARF for
  `caml_apply`/`caml_curry` in the startup file must survive linking.
- Size: small.

#### PR 6: Immutable loads for closure fields in generic apply/curry functions
- **Files:** `backend/cmm_helpers.ml` only — `apply_function_body`,
  `tuplify_function`, `make_curry_apply`,
  `read_from_closure_given_machtype`: switch closure-field loads from
  `Mutable` to `Immutable` (closure fields are write-once), so the debugger
  can describe call targets and recovered arguments as closure projections.
- Standalone codegen change (corresponds to the "caml_curry fix" commit).
- Size: small.

#### PR 7: Backend debuginfo-quality fixes (spills, reloads, inserted blocks, terminators)
- **Files:** `backend/regalloc/regalloc_utils.ml`,
  `backend/regalloc/regalloc_split.ml`, `backend/cfg/cfg_with_layout.ml`,
  `backend/cfg/cfg.{ml,mli}` (only the `?dbg` parameter on
  `make_instruction_from_copy`), `backend/cfg/sub_cfg.{ml,mli}` (only the
  `?dbg` parameter on `update_exit_terminator`), `backend/cfg_selectgen.ml`
  (only the `_dbg` → `dbg` cleanup in `emit_expr_ifthenelse`,
  `emit_expr_switch`, `emit_tail_ifthenelse`, `emit_tail_switch`, resolving
  the `CR-someday xclerc` comments).
- Contents:
  - `Move.dbg_for_move`: spill/reload (`Load`/`Store`) moves get only the
    outermost `Debuginfo` item (via `Debuginfo.of_items`, pre-existing), so
    they are not attributed to inlined callees.
  - `regalloc_split.ml`: spills/reloads take dbg/fdo from the block
    terminator; new `default_copy_for_reloads` walks unique predecessors
    (fuel 3) to find real debuginfo.
  - `cfg_with_layout.insert_block`: empty inserted blocks take dbg/fdo from
    the predecessor's terminator instead of `Debuginfo.none`.
- All independent of phantom lets; `Debuginfo.of_items` exists at base.
- Size: ~120 lines.

#### PR 8: Fix stack-offset double count in `available_ranges_vars`
- **Files:** `backend/debug/available_ranges_vars.ml` — in
  `Subrange_info.create`, stop adding `Proc.initial_stack_offset` (it is
  already included by `Proc.frame_size`/`Proc.slot_offset`), which made CFA
  offsets wrong by the alignment padding.
- **Extraction warning:** this file is also touched by PR 16 (Holds_value_of)
  and PR 18 (fundecl threading). Extract just the offset arithmetic + comment
  hunks.
- Size: tiny.

#### PR 9: New `-ddebug-avail-sets` dump flag
- **Files:** `backend/debug/dwarf/dwarf_flags/dwarf_flags.{ml,mli}` (new
  `debug_avail_sets` ref only), `driver/oxcaml_args.{ml,mli}`
  (`mk_ddebug_avail_sets`, `Debugging_options` plumbing),
  `backend/printlinear.ml` / `backend/cfg/printcfg.ml` — **only** the gate
  widening `!Oxcaml_flags.davail` → `davail || debug_avail_sets`.
- The `PAB={...}` (phantom-availability) printing lines in those two files
  belong to PR 15, not here.
- Size: tiny.

### Stage 1 — Cmm groundwork

#### PR 10: Symbol-typed phantom defining expressions in Cmm
- **Files:** `backend/cmm.{ml,mli}` (relocate `is_global`/`symbol`/equality
  functions above `phantom_defining_expr` — pure move; change
  `Cphantom_const_symbol of string` → `of symbol` and
  `Cphantom_read_symbol_field.sym : string` → `symbol`),
  `backend/printcmm.ml` (print `sym.sym_name`).
- Grep for any other producers/consumers of these constructors at `4.11`
  before opening; on this branch only `to_cmm_expr` (PR 17) produces them.
- Size: small. **Prerequisite for PRs 15 and 17.**

#### PR 11: `Cmm.Cname_for_debugger` and backend support
- **Files:** `backend/cmm.{ml,mli}` (constructor + `iter_shallow_tail`,
  `map_shallow_tail`, `map_tail`, `iter_shallow`, `map_shallow`),
  `backend/cmm_helpers.ml` (`map_tail1`, `letin`, `cmm_arith_size` arms),
  `backend/cmm_invariants.ml`, `backend/cmm_builtins.ml`,
  `backend/cmm_peephole_engine.ml` (descend + `Cmm_comparator.equivalent`
  case), `backend/printcmm.ml`, plus match arms in
  `backend/amd64/cfg_selection.ml`, `backend/amd64/simd_selection.ml`,
  `backend/amd64/proc.ml`, `backend/arm64/proc.ml`,
  `backend/afl_instrument.ml`, and the `Cname_for_debugger` handling in
  `backend/cfg_selectgen.ml` `emit_expr`/`emit_tail` (emit
  `Operation.Name_for_debugger`) and `backend/select_utils.ml` (`size`).
  Also `to_cmm_env.ml`'s `is_cmm_simple` arm (returns `false`).
- Include the `select_utils.maybe_emit_naming_op` / `cfg_selectgen`
  `which_parameter_of_provenance` change (compute `which_parameter` from
  `Backend_var.Provenance.is_parameter` instead of hardcoded `None`) — this
  needs **PR 2**.
- No producer yet (that arrives in PR 17), so behavior-neutral.
- **Depends on:** PR 2. Size: ~150 lines.

### Stage 2 — Flambda 2 debug metadata

#### PR 12: `Bound_var`/`Bound_parameter` carry `Debuginfo.t` and `Is_parameter` (mechanical)
- **Core files:** `middle_end/flambda2/bound_identifiers/bound_var.{ml,mli}`
  (new `Is_parameter` submodule, `dbg`/`is_parameter` fields, `create ~dbg
  ~is_parameter`, `add_inlined_debuginfo`, `print_is_parameter`),
  `bound_parameter.{ml,mli}` (`dbg` field, `create ~dbg`,
  `var_and_uid_and_debuginfo`, `add_inlined_debuginfo`),
  `bound_parameters.{ml,mli}` (`vars_and_uids_and_debuginfo`),
  `bound_pattern.{ml,mli}` (`add_inlined_debuginfo`),
  `bound_identifiers/dune` (+ `flambda2_term_basics` dep), root `dune`
  (link-order: `flambda2_bound_identifiers` after `flambda2_term_basics`).
- **Mechanical fallout** (every call site passes `~dbg:Debuginfo.none` and/or
  `~is_parameter:...local_var`): the remaining ~25 files in
  `middle_end/flambda2/` — `from_lambda/closure_conversion.ml` (mechanical
  hunks only), `from_lambda/lambda_to_flambda_primitives_helpers.ml`,
  `parser/fexpr_to_flambda.ml`, `terms/flambda.ml`, all touched files under
  `simplify/` (mechanical hunks only), `types/` (4 files), `reaper/` (2
  files), `tests/meet_test.ml`, `simplify_shared/inlining_helpers.ml`, and
  the direct `Bound_var.create`/`Bound_parameter.create` call sites in
  `to_cmm/` (`to_cmm.ml`, `to_cmm_expr.ml`, `to_cmm_set_of_closures.ml`,
  `to_cmm_shared.ml`).
- The `dbg`/`is_parameter` fields are excluded from comparison, hashing,
  renaming and `ids_for_export` — state this in the PR description.
- Behavior-neutral by construction (all values are `none`/`local_var`).
  The signature change is atomic; this PR cannot be subdivided without
  breaking the build.
- **Depends on:** nothing. Size: ~600 lines, almost all mechanical.

#### PR 13: Populate real debug metadata (closure conversion, inlining, simplify stamping)
- **Files & contents:**
  - `from_lambda/closure_conversion_aux.{ml,mli}`: `Env` gains
    `var_debug_uids : Flambda_debug_uid.t Ident.Map.t`
    (`add_var_debug_uid`/`find_var_debug_uid`, `add_var_like ?debug_uid`).
  - `from_lambda/closure_conversion.ml` (semantic hunks): parameters created
    with the function's location as `dbg` and `parameter ~index` (with
    `index_offset` for `my_closure`); value-slot/function-slot projection
    vars get real debug UIDs and `user_visible`; nested-closure UID
    propagation; `my_closure`/regions tagged `implicit_parameter`.
  - `simplify/inlining/inlining_transforms.ml` (`make_inlined_body`):
    parameter indexing via `index_offset`, `var_and_uid_and_debuginfo`,
    preserves `param_dbg` (deliberately *not* stamped here — see next item).
  - `simplify/simplify_let_expr.ml` (`simplify_let0`): stamp the bound
    pattern once per `Let`, downwards, via
    `Bound_pattern.add_inlined_debuginfo` + new `DE.inlined_debuginfo`
    accessor in `simplify/env/downwards_env.{ml,mli}`.
  - `implicit_parameter` tagging in `downwards_env.create`,
    `simplify_set_of_closures.dacc_inside_function`,
    `simplify_common.split_direct_over_application`.
  - `simplify/expr_builder.ml`: comment on `create_let` explaining why
    debuginfo must not be re-stamped on the way up. (Drop the blank-line-only
    change to `expr_builder.mli`.)
- **Review note:** `index_offset` assumes `my_closure` is only ever the first
  parameter (matches `Inlining_helpers.make_inlined_body`); this invariant is
  load-bearing for DWARF parameter numbering.
- **Depends on:** PR 12. Size: ~350 lines.

#### PR 14: Phantom-visibility semantics in Simplify
- **Files & contents:**
  - `simplify/simplify_let_expr.ml` (`rebuild_let`): `can_phantomise`
    relaxed from "all bound vars user-visible" to `not is_depth`, so
    non-user-visible intermediates (inlined `my_closure`, temporaries) get
    phantom lets and user-visible projections from them don't dangle.
  - `simplify/simplify_set_of_closures.ml` (`simplify_function_body`):
    when `my_closure` occurs only at `Phantom` name mode
    (`NO.greatest_name_mode_var`), rebind it phantom-ly to
    `Nullary (Optimised_out K.value)` via `EB.make_new_let_bindings` and
    report `is_my_closure_used = false`.
- Behavior only observable with `-flambda2-expert-phantom-lets` (default still
  off until PR 21).
- **Depends on:** PR 12 (and thematically on PR 13). Size: small.
  May be folded into PR 13 if reviewers prefer.

### Stage 3 — backend plumbing

#### PR 15: Phantom lets through selection, CFG and Linear
- **Files:** `backend/cfg/cfg_intf.ml` (new mutable
  `phantom_available_before : Backend_var.Set.t option` on instructions),
  `backend/cfg/cfg.{ml,mli}` (`phantom_defining_expr` +
  `phantom_defining_expr_of_cmm`, `fun_phantom_lets` field on `Cfg.t`,
  `make_instruction ?phantom_available_before`),
  `backend/linear.{ml,mli}` (`phantom_defining_expr` + smart constructors,
  `phantom_available_before` on instructions, `fun_phantom_lets` on
  `fundecl`, `instr_cons ~phantom_available_before`),
  `backend/select_utils.{ml,mli}` (env `phantom_lets` set,
  `env_add_phantom_let`, `phantom_vars_from_env` — returns `None` under
  `-grestrict-to-upstream-dwarf`; insert helpers thread the set),
  `backend/cfg_selectgen.ml` (`accumulated_phantom_lets` +
  `accumulate_phantom_let`, `Cphantom_let` handling in
  `emit_expr`/`emit_tail`, `ensure_referenced_vars_named`,
  `emit_fundecl` passes `~fun_phantom_lets`),
  `backend/cfg/sub_cfg.{ml,mli}` (`~phantom_available_before` on
  `make_instr`/`add_instruction`/`set_terminator`),
  `backend/cfg/cfg_to_linear.ml` (`phantom_defining_expr_to_linear`,
  propagate field, build `fun_phantom_lets`),
  pass-through/mechanical: `backend/cfg/cfg_merge_blocks.ml`,
  `backend/cfg/cfg_quick_hash.ml`, `backend/cfg/cfg_with_layout.ml`,
  `backend/emitaux.ml`, `backend/branch_relaxation.ml`,
  `backend/llvm/llvmize.ml`, `backend/regalloc/regalloc_utils.ml`
  (`Instruction.dummy`), `oxcaml/tests/backend/validators/utils.ml`;
  printers: the `PAB={...}` hunks in `backend/printlinear.ml` and
  `backend/cfg/printcfg.ml`.
- At base, selection silently recursed into `Cphantom_let` bodies dropping
  the binding; this PR is where the binding starts to be preserved.
- **Known CRs to keep visible in review:** `accumulated_phantom_lets` is
  mutable functor state (contradicts the functor's "must not have state"
  header comment); `~phantom_available_before:None` placeholders in
  `setup_catch_handler` and `insert_param_name_for_debugger`.
- **Depends on:** PRs 2, 9, 10, 11. Size: ~450 lines.

#### PR 16: Track constants and field projections in register availability
- **Files:** `backend/debug/reg_with_debug_info.{ml,mli}` (new
  `Holds_value_of` module; `Debug_info.holds_value_of` retyped from
  `Backend_var.t`), `backend/debug/reg_availability_set.ml`
  (`canonicalise`), `backend/debug/available_ranges_vars.ml`
  (`Range_info.create` matches on `Var` only — **exclude** the PR 8 and
  PR 18 hunks), `backend/cfg/cfg_available_regs.ml` (`~const_for_result`
  tracking of `Const_int`/`Const_float`/`Const_symbol` and immutable
  full-word `Load`s as `Projection`s), `backend/amd64/arch.{ml,mli}` and
  `backend/arm64/arch.{ml,mli}` (new `addressing_displacement_in_bytes`).
- Purpose: lets DWARF call-site parameters (PR 20) describe values that were
  constants or loaded closure/block fields. Only full-word value fields are
  handled (TODO comment covers unboxed/flat fields).
- Independent of PR 15 (verified: `cfg_available_regs.ml` does not touch
  `phantom_available_before`).
- **Depends on:** PR 11 (the `Name_for_debugger`→`Holds_value_of.Var`
  wrapping site sits next to code added there; rebase accordingly).
  Size: ~250 lines.

### Stage 4 — generating phantom lets

#### PR 17: Translate Flambda 2 phantom lets to Cmm
- **Files:** `middle_end/flambda2/to_cmm/to_cmm_expr.ml` (the ~250-line
  `let_expr_phantom`: `Simple`/`Make_block`/`Block_load`/
  `Project_value_slot`/`Project_function_slot`/`Optimised_out` →
  `Cphantom_*`), `to_cmm_env.{ml,mli}` (`gen_variable ~dbg
  ~bv_is_parameter` bridging `Bound_var.Is_parameter` →
  `Backend_var.Provenance` `Is_parameter`; provenance for implicit
  parameters; `add_phantom_let_binding`; `find_bound_expression`; deferral
  of `env.vars` writes to flush time; `Cname_for_debugger` wrapping in
  `will_inline_simple/complex`; `create_bound_parameter(s)` now take
  `(var, debug_uid, dbg)` triples), `to_cmm_result.{ml,mli}`
  (`defined_data_symbols` set + `data_symbol_is_defined`;
  `symbol_must_be_global_for_dwarf` forcing `Global` under
  `-gdwarf-may-alter-codegen` + phantom lets), and the remaining
  non-mechanical hunks in `to_cmm.ml`, `to_cmm_set_of_closures.ml`,
  `to_cmm_shared.ml`.
- **Review flags:**
  - Three uses of hardcoded `Target_system.Machine_width.Sixty_four`
    (32-bit portability smell).
  - `CR mshinwell` in `to_cmm_result.ml`: globalising *every* symbol under
    the flags is over-broad.
- **Depends on:** PRs 2, 10, 11, 12, 13 (and 14 for the phantom bindings it
  translates). Size: ~500 lines. This is the keystone PR — everything
  upstream of it is metadata, everything downstream consumes its output.

### Stage 5 — DWARF ranges and emission

#### PR 18: Availability ranges for phantom variables; range infrastructure
- **Files:** `backend/debug/compute_ranges.ml`,
  `backend/debug/compute_ranges_intf.ml` (thread `L.fundecl` into
  `available_before`/`available_across`; copy `phantom_available_before`
  onto synthesized label instructions),
  `backend/debug/available_ranges_vars.ml` (fundecl-threading hunks only),
  `backend/debug/inlined_frame_ranges.ml` (`dbg_and_parents`;
  spill/reload instructions close inlined-frame ranges via
  `is_spill_or_reload`), **new**
  `backend/debug/available_ranges_phantom_vars.{ml,mli}` (ranges keyed on
  phantom vars, clipped to the defining inlined frame by comparing
  `dinfo_uid`/`dinfo_function_symbol`), **new**
  `backend/debug/available_ranges_all_vars.{ml,mli}` (unifying wrapper over
  vars + phantom vars), root `dune` (register the two new modules).
- **Depends on:** PRs 2, 15, 16. Size: ~700 lines.

#### PR 19: DWARF emission for variables, parameters and inlined frames
- **Files:** `backend/debug/dwarf/dwarf_ocaml/dwarf_variables_and_parameters.{ml,mli}`
  (phantom location descriptions for every `Lphantom_*` form incl.
  `Cphantom_block` via DWARF procedures; entry-value location-list entries;
  `proto_dies_for_vars` sharing; `which_vars =
  Vars_for_inlined_frame | All_remaining_vars` attribution;
  rvalue-DIE fixpoint; parameter demotion logic),
  `dwarf_inlined_frames.{ml,mli}` (per-frame variable emission; bugfix:
  `DW_AT_call_file/line/column` from the call-site item rather than the
  block's own location), `dwarf_concrete_instances.{ml,mli}`,
  `dwarf_abstract_instances.{ml,mli}` (`add_empty` exposed),
  `dwarf_reg_locations.{ml,mli}` (`address_of_cmm_symbol_rvalue`: `Local`
  symbols via label address — macOS relocation constraint),
  `backend/debug/dwarf/dwarf_ocaml/dwarf.ml` (compute phantom + all-vars
  ranges, gated on `not restrict_to_upstream_dwarf`),
  `backend/asm_targets/asm_directives.ml` (`between_labels_32_bit` forces
  assembly-time constants on macOS — needed once phantom DWARF is emitted).
- **Review flags:** dead feature flag `use_dw_op_piece = false` guarding a
  never-taken GDB workaround branch (consider deleting);
  `Available_ranges_all_vars.Range_info.debuginfo` always returns
  `Debuginfo.none` (TODO); `XXX` comment in `dwarf_inlined_frames.ml`.
- **Depends on:** PRs 3, 4, 18. Size: ~900 lines; the hardest review of the
  series — consider splitting entry values into a follow-up if reviewers ask.

#### PR 20: DWARF call-site information
- **Files:** **new**
  `backend/debug/dwarf/dwarf_ocaml/dwarf_call_sites.{ml,mli}` (~650 lines:
  walks `Linear` bodies matching `Lcall_op` variants, tracks `stack_offset`,
  emits `DW_TAG_call_site`/`DW_TAG_call_site_parameter` — GNU variants for
  DWARF-4 — with values recovered via `Holds_value_of` from constants,
  spilled copies, entry values and projections; `insert_label_after` for
  return-address labels; `DW_AT_call_all_calls`), the
  `Dwarf_call_sites.dwarf` call in `dwarf_concrete_instances.ml`,
  `backend/proc.mli` + `backend/amd64/proc.ml` + `backend/arm64/proc.ml`
  (new `extcall_code_size_after_return_address`; arm64 implemented and kept
  in sync with `emit.ml`, **amd64 returns `None`** with a CR — extcall call
  sites are silently absent on amd64), root `dune` (register module).
- **Review flags:** large `CR mshinwell` about function-slot field-0 vs
  field-2 callee identification; self-tail-calls skipped.
- **Depends on:** PRs 16, 19 (uses `proto_dies_for_vars`, entry-value
  machinery, `Dwarf_reg_locations`). Size: ~700 lines.

### Stage 6 — enablement

#### PR 21: Enable phantom lets by default; tests
- **Files:** `driver/oxcaml_flags.ml` — `Flambda2.Expert.Default.phantom_lets`
  `false` → `true`. This is the single most behaviorally significant line on
  the branch; keep it isolated so it is trivially revertable.
- Add real tests here: convert the root-level `inlined_test*.ml` repros
  (printing values of inlined parameters/locals, phantom projections,
  entry-value recovery) into proper tests under `oxcaml/tests/`, and delete
  them from the repo root.
- Run the full testsuite and, ideally, manual LLDB/GDB verification on both
  macOS and Linux before merging.
- **Depends on:** all previous PRs.

---

## Dependency graph

```
Stage 0 (parallel):  PR1  PR2  PR3  PR4  PR5  PR6  PR7  PR8  PR9
                            │    │    │                        │
             ┌──────────────┤    │    └────────┐               │
             ▼              ▼    │             │               │
Stage 1:   PR10   PR11 ◄── PR2   │             │               │
             │      │            │             │               │
Stage 2:   PR12 ──► PR13 ──► PR14│             │               │
             │      │        │   │             │               │
Stage 3:     │      │  PR15 ◄┼───┼──(2,9,10,11)│  PR16 ◄──(11) │
             │      │        │   │             │    │          │
Stage 4:   PR17 ◄──(2,10,11,12,13,14)          │    │          │
                             │                 │    │          │
Stage 5:   PR18 ◄──(2,15,16) └► PR19 ◄──(3,4,18)    └► PR20 ◄──(16,19)
                                                            │
Stage 6:   PR21 ◄── everything                              ┘
```

Suggested landing order: 1–9 in any order, then 10, 11, 12, 13, 14, 15, 16,
17, 18, 19, 20, 21.

## Files with interleaved hunks from multiple PRs

When extracting, these files need hunk-level (sometimes line-level) splitting:

| File | Split across |
|---|---|
| `backend/debug/available_ranges_vars.ml` | PR 8 (stack offset), PR 16 (`Holds_value_of`), PR 18 (fundecl threading) |
| `backend/cfg_selectgen.ml` | PR 7 (`_dbg` cleanup), PR 11 (`Cname_for_debugger`, `which_parameter_of_provenance`), PR 15 (phantom plumbing) |
| `backend/cfg/cfg.{ml,mli}` / `sub_cfg.{ml,mli}` | PR 7 (`?dbg` params), PR 15 (phantom) |
| `backend/printlinear.ml`, `backend/cfg/printcfg.ml` | PR 9 (gate widening), PR 15 (PAB printing) |
| `backend/debug/dwarf/dwarf_flags/dwarf_flags.{ml,mli}` | PR 5 (`dwarf_for_startup_file` default), PR 9 (`debug_avail_sets`) |
| `middle_end/flambda2/from_lambda/closure_conversion.ml` | PR 12 (mechanical) vs PR 13 (semantic) |
| `middle_end/flambda2/simplify/simplify_let_expr.ml` | PR 13 (stamping), PR 14 (`can_phantomise`) |
| `middle_end/flambda2/simplify/env/downwards_env.{ml,mli}` | PR 12 (mechanical), PR 13 (`inlined_debuginfo`, `implicit_parameter`) |
| `middle_end/flambda2/to_cmm/*` | PR 12 (mechanical `~dbg` args) vs PR 17 (everything else) |
| `backend/regalloc/regalloc_utils.ml` | PR 7 (`dbg_for_move`), PR 15 (`Instruction.dummy` field) |
| `backend/debug/dwarf/dwarf_ocaml/dwarf_abstract_instances.ml`, `dwarf_inlined_frames.ml` | PR 4 (linkage names) vs PR 19 (emission rework) |
| root `dune` | PR 2 (`is_parameter`), PR 5 (`-gstartup`), PR 12 (link order), PR 18/PR 20 (new modules) |

## Cleanup checklist (fix while splitting, or note in PR descriptions)

- [ ] `Debuginfo.remove_outermost_frame`: dead code, no consumers — drop (PR 1).
- [ ] `use_dw_op_piece = false` dead flag in
      `dwarf_variables_and_parameters.ml` — delete or justify (PR 19).
- [ ] Hardcoded `Machine_width.Sixty_four` ×3 in `to_cmm_expr.ml` (PR 17).
- [ ] amd64 `extcall_code_size_after_return_address` returns `None` (CR) —
      state the amd64 feature gap in PR 20's description.
- [ ] `accumulated_phantom_lets` mutable functor state in `cfg_selectgen.ml`
      (CR) — acknowledge in PR 15.
- [ ] Over-broad symbol globalisation under `-gdwarf-may-alter-codegen`
      (CR in `to_cmm_result.ml`) — acknowledge in PR 17.
- [ ] Whitespace-only noise to drop: `expr_builder.mli` blank line,
      `backend_var.ml` ocamlformat churn, `dune` `;; TYPING` re-indent.
- [ ] Root `inlined_test*.ml` files: convert to real tests in PR 21; never
      merge them at the repo root.
- [ ] `Available_ranges_all_vars` TODOs (`debuginfo = none`, static phantom
      vars) — keep TODO comments, mention in PR 18/19 descriptions.

## Verification per PR

- Every PR: `make -s boot-compiler`, then `make -s test`, `make -s fmt`.
- PRs 1–14 should be bit-for-bit behavior-neutral for normal compilation
  (phantom lets default off; new metadata excluded from comparisons). Where
  practical, diff generated assembly on a small corpus before/after.
- PRs 15–20: additionally compile a repro like `inlined_test4.ml` with
  `-g -gno-upstream-dwarf -gdwarf-inlined-frames
  -flambda2-expert-phantom-lets` and inspect with `llvm-dwarfdump --debug-info`
  (look for `DW_TAG_inlined_subroutine` children, `DW_TAG_call_site`,
  `DW_OP_entry_value`, phantom variable location lists) and/or LLDB
  (`frame variable` inside inlined frames).
- macOS-specific behavior (PRs 5, 19): startup object retention and
  assembly-time label differences only take effect on `Config.system =
  "macosx"` — test on both platforms if possible.
