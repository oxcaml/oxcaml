# Mode polymorphism printing: where to look

Two review passes. Parsing is out of scope: nothing from
`origin/ageorges/mode-polymorphism-parsing` beyond PR #7098.

- **(A) Mode-polymorphism printing on `main`, skipping curry modes.**
  Version: local `main`. Its `typing/out_type.ml` is identical to the PR's merge
  base `4b128631a9`. `origin/main` only differs after ~3600 (unrelated code).
- **(B) The new curry-mode printing in PR #7098, by itself.**
  Version: PR head `1d5e4ca92e`, fetched as `origin/pr-7098`:
  `git fetch origin pull/7098/head:refs/remotes/origin/pr-7098`
  `git show origin/pr-7098:typing/out_type.ml > /tmp/ot_pr.ml`
  `git diff 4b128631a9 origin/pr-7098 -- typing/out_type.ml typing/ctype.ml typing/ctype.mli typing/mode_intf.mli`

Line numbers are only hints; search by name. `VN.` = inside
`module Variable_names`.

Markers for pass (A):
- ⛔ **skip**: curry-mode logic, reviewed in (B)
- ◐ **partial**: read it, but skip the part named in the Notes column
- (no marker): review in (A)

---

## (A) Printing on main

### A1. `typing/out_type.ml`: helpers before `Variable_names`

| Function / item | main line | Marker | Notes |
|---|---|---|---|
| `printer_iter_type_expr` | 897 | | The traversal used by the `reserve` passes. `fm` is called on arrow modes via `Btype.fold_type_expr` |
| `mode_polymorphism_printing_enabled` | 935 | | Gate: `Mode_polymorphism` ≥ Alpha **and** `Mode_polymorphism_printing` |
| `type const_or_generic` | 1011 | ◐ | Read only `Const` (print constants) vs `Generic` (print a mode variable). The bound carried in `Generic` is curry logic |
| `const_or_generic_upper` | 1017 | ⛔ | |
| `curry_acc` | 1025 | ⛔ | |
| `const_or_generic_of_mode` | 1049 | ◐ | Decides whether an occurrence is zapped to a constant or kept generic. Review that decision; ignore the bound it builds |
| `equate_with_curry_bounds` | 1073 | ⛔ | |
| `erase_implied_axes` | 1085 | | Used by `tree_of_modes_const` and `VN.construct_raw_bounds` |
| `equate_with_const` (top level) | 2412 | ⛔ | |

### A2. `module Variable_names`: state

Signature at 1133, struct at 1164.

| Item | main line | Marker |
|---|---|---|
| `curry_edges_implied` in the signature | 1150 | ⛔ |
| `visible_pair` type | 1169 | |
| `visited_for_modes`, `visited_for_named_modevars` | 1181–1182 | |
| `boxedvar`, `boxedpath` | 1188, 1190 | |
| `morphl`, `monadic_morph`, `comonadic_morph`, `closing_over_morph` | 1202–1208 | |
| `boxedname` (`Simple_name` / `Comonadic_name`) | 1211 | |
| `VarTbl`, `VarPairMap` | 1225, 1231 | |
| `module Paths` (`Store` functor, `table` GADT, `add`/`find`) | 1239–1365 | |
| `visible_pairs`, `aliased_visible_pairs`, `printed_aliased_visible_pairs` | 1369–1371 | |
| `visible_vars`, `visible_paths`, `modename_counter`, `modenames` | 1375–1381 | |
| `edges` record, `edge_table` | 2107, 2112 | |
| `boxedhead`, `suppressed_curry_heads`, `heads_of_mode`, `mem_head` | 2114–2125 | ⛔ |
| `reset_names` | 2129 | ◐ Skip the `suppressed_curry_heads` reset |

### A3. `VN`: preprocessing (`reserve`)

| Function | main line | Marker | Notes |
|---|---|---|---|
| `reserve` | 2396 | | Entry point. Order: zap → `add_named_modevars` → `add_visible_paths` → `add_visible_edges` → backtrack |
| `add_named_var(s)` | 1387/1394 | | Type variables only. Context |
| `zap_non_generic_modes` | 1413 | ◐ | Review the zapping of non-generic modes, including `const_or_generic_of_mode ~arg:true marg` on the argument. Skip `curry_acc` and the hand-off to `equate_curry` |
| `equate_curry` | 1432 | ◐ | Only the `| _ ->` branch (an ordinary return mode) is (A). The `Tarrow _ when equate_with_curry_bounds` branch is ⛔ |
| `eq_pair` | 1443 | | |
| `add_visible` | 1463 | | |
| `add_named_modevar : lr -> unit` | 1472 | | Registers visible pairs and aliases. Shadowed later by the naming function in A5 |
| `add_named_modevars` | 1486 | | |
| `find_visible_morph_opt` | 1502 | | |
| `visible` | 1530 | | |
| `paths_to_visible`, `paths_from_vlowers`, `find_paths` | 1547, 1555, 1579 | | |
| `find_path_from_description` | 1600 | | Pseudocode `record_paths` |
| `add_visible_paths` | 1634 | | |
| `add_visible_edges` | 2147 | ◐ | Skip the `suppressed_curry_heads := []` line |

### A4. `VN`: edges

| Function / type | main line |
|---|---|
| `simple_edge`, `comonadic_edge`, `closing_over_edge`, `edge` | 1642, 1663, 1694, 1707 |
| `dupper_lr`, `dlower_lr` | 1717, 1724 |
| `construct_morphs`, `construct_closing_over_morphs`, `construct_monadic_morphs`, `construct_comonadic_morphs` | 1731, 1754, 1766, 1769 |
| `descr_compare_dec`, `descr_is_var`, `construct_edge_condition` | 1775, 1788, 1804 |
| `construct_name`, `construct_comonadic_name` | 1823, 1842 |
| `construct_simple_between`, `check_closing_over_candidate`, `construct_comonadic_between` | 1857, 1871, 1886 |
| `eq_morph`, `eq_boxedname` | 1902, 1912 |
| `closing_over_composition`, `eq_closing_over`, `dedup_closing_over`, `construct_closing_over_to` | 1928–1945 |
| `construct_edges_between`, `construct_edges_to`, `construct_edges_from` | 1975, 1983, 1992 |
| `edge_as_lower`, `edge_as_upper`, `partition_edges_into_bounds` | 2049, 2054, 2088 |
| `find_edges` | 2159 |

None of A4 is curry-specific.

### A5. `VN`: naming and printing

| Function | main line | Marker | Notes |
|---|---|---|---|
| `pick_name` | 1998 | | |
| `add_named_modevar : boxedname -> string` | 2007 | | |
| `construct_raw_bounds` | 2023 | | On main it goes through `erase_implied_axes`. The PR changes this (see B4) |
| `print_raw_upper_bound`, `print_raw_lower_bound` | 2058, 2069 | | |
| `pair_of_mode`, `acc_heads`, `curry_edges_implied` | 2166, 2172, 2183 | ⛔ | |
| `print_raw_constraints` | 2212 | ◐ | Skip the `List.filter` on `suppressed_curry_heads` at the top |
| `find_mode_already_printed`, `mode_print_alias` | 2269, 2275 | | |
| `name_of_mode` | 2287 | | Pseudocode `print_generic_mode` |

### A6. `typing/out_type.ml`: building the tree

| Function | main line | Marker | Notes |
|---|---|---|---|
| `wrap_mutation` | 746 | | snapshot/backtrack around printing |
| `prepare_type_with_base`, `reset_except_conflicts`, `prepare_for_printing ~base` | 2498, 2505, 2512 | | |
| `tree_of_modes_const` | 2606 | | |
| `tree_of_modes` | 2630 | | `Generic` → `VN.name_of_mode`; `Const` → `tree_of_modes_const` |
| `type modal` | 2640 | ◐ | `Other` is (A). The `acc` field of `Arrow_return` is ⛔ |
| `tree_of_modal_typexp` | 2675 | ◐ | (A): `not_arrow`, and the `Tarrow` case's argument modes (`const_or_generic_of_mode ~arg:true`, `tree_of_modes marg`). ⛔: `curry_acc acc_mode marg` |
| `tree_of_acc_typexp`, `tree_of_typexp` | 2893, 2896 | | |
| `tree_of_ret_typ_mutating` | 2973 | ◐ | Only the `| _ ->` branch (non-arrow return) is (A). The `Tarrow _` branch (parens or elision) is ⛔ |
| `tree_of_typexp ~base` | 3045 | | Wraps everything in `wrap_mutation` |

### A7. Other files (main)

| File | Function / item | Line |
|---|---|---|
| `typing/out_type.mli` | `prepare_for_printing ~base`, `tree_of_typexp ~base`, `Variable_names.reserve ~base` | 86, ~98, 287 |
| `typing/outcometree.mli` | `out_mode`, `out_ret_mode` (`Orm_any` / `Orm_no_parens` / `Orm_parens`), `Otyp_ret` | 84–95, 159 |
| `typing/oprint.ml` | `print_out_modes`, `print_out_type_mode`, `print_out_ret` | 333, 384, 402 |
| `typing/errortrace_report.ml` | callers of `Variable_names.reserve ~base` / `prepare_for_printing ~base` | 109, 177, 233, … |
| `typing/mode.ml` (`Mode_with`, 6550) | `Const.Option.partial_print`, `Const.diff`, `Const.print_axis` | 6882, 6917, 6962 |
| `typing/mode.ml` | `get_monadic_desc`, `get_comonadic_desc`, `meet_const_morph` | 6999, 7001, 7003 |
| `typing/mode.ml` | `pretty_print_mod`, `pretty_print_{monadic,comonadic}_simple_morph`, `pretty_print_{monadic,comonadic}_morph` (emit `close(…)` and `… mod …`) | 7016–7085 |
| `typing/mode.ml` | `C.print_morph` (fallback `f(m)`) | 3731 |
| `typing/mode_intf.mli` | `C`, `Desc`, `obj_monadic`/`obj_comonadic`, `get_*_desc`, `pretty_print_*_morph` | ~825–925 |
| `typing/solver.ml` | `module Desc` (`Var.force` snapshots precise floor/ceil, `vlower`, level), `desc` | 2571, 2592, 2681 |
| `utils/language_extension_kernel.ml` | `Mode_polymorphism_printing` | 13 |

Solver primitives the printer calls (peek, don't review):
- `check_generic` (mode.ml 7233 → solver.ml 622)
- `zap_to_legacy` / `zap_to_legacy_force` (mode.ml 7371 / 7362)
- `Guts.get_floor` / `get_ceil` (solver.ml 2397 / 2408)
- `C.compose` / `left_adjoint` / `compare_morph` (mode.ml `Lattices_mono`)
- `Btype.snapshot` / `backtrack`

### A8. Skip list for (A)

**Skip entirely:**
- top level: `const_or_generic_upper`, `curry_acc`, `equate_with_curry_bounds`, `equate_with_const`
- `VN`: `boxedhead`, `suppressed_curry_heads`, `heads_of_mode`, `mem_head`, `pair_of_mode`, `acc_heads`, `curry_edges_implied`
- `Ctype.curry_mode`, `Ctype.curry_mode_const`

**Skip only part:**

| Function | Part to skip |
|---|---|
| `zap_non_generic_modes` | `curry_acc` / `equate_curry` hand-off |
| `equate_curry` | `Tarrow` branch |
| `reset_names` | suppressed reset |
| `add_visible_edges` | suppressed reset |
| `print_raw_constraints` | suppressed-heads filter |
| `type modal` | `Arrow_return.acc` |
| `tree_of_modal_typexp` | `curry_acc` call |
| `tree_of_ret_typ_mutating` | `Tarrow` branch |
| `const_or_generic` | the bound in `Generic` |

---

## (B) New curry-mode printing (PR #7098)

The commits that matter:

| Commit | Change |
|---|---|
| `8f2f9584e4` | Hiding decided in `reserve` (`curry_candidates`, `register_suppressed_curries`) |
| `2683b41956` | Constant accumulator never matches a generic mode, and vice versa |
| `cd85314542` | `Ctype.Curry_mode` refactor |
| `3180691f21` | `expected_floor` check |
| `78d6fd7a79` | Minimum areality |

`9d867a9eca` and `675245d127` are tests only. `1d5e4ca92e` is not curry; see B4.

### B1. Name map: main → PR

| main (A8 skip list) | PR #7098 | PR line |
|---|---|---|
| `type const_or_generic = Const \| Generic of Comonadic.l * Const.t` | `Ctype.Curry_mode.t = Const \| Variable {comonadic; areality}` | ctype.ml 2130, ctype.mli 316 |
| `const_or_generic_upper` | removed (no constant upper bound is tracked) | — |
| `curry_acc` | `curry_acc`, now a wrapper over `Curry_mode.add_arg` / `add_const_arg` | 1006 |
| `const_or_generic_of_mode` | `curry_mode_of_occurrence` | 1021 |
| `equate_with_curry_bounds` (`in_bounds` check) | `equate_with_curry_bounds` (`expected_floor` + monadic ceil = max) | 1046 |
| top-level `equate_with_const` | `VN.equate_with_const`, which looks up `is_suppressed_curry` | 2207 |
| `VN.curry_edges_implied ~bounds_implied` (runs at print time, mutates) | `VN.curry_edges_implied ~curry_heads` (pure; called from `register_suppressed_curries`) | 2173 |
| — | `VN.curry_candidates` | 1172 |
| — | `VN.register_suppressed_curries` | 2216 |
| — | `VN.is_suppressed_curry` | 2200 |
| `VN.acc_heads` (matches on `Const`/`Generic`) | `VN.acc_heads` (via `Curry_mode.comonadic`) | 2163 |
| `VN.boxedhead`, `suppressed_curry_heads`, `heads_of_mode`, `mem_head`, `pair_of_mode` | unchanged | 2105–2116, 2157 |
| `Ctype.curry_mode`, `curry_mode_const` | unchanged; used by `Curry_mode` | ctype.ml 2114, 2120 |

### B2. What to review in the PR

| Function | PR line | What changed |
|---|---|---|
| `Ctype.Curry_mode` (`comonadic`, `areality`, `add_arg ~upper_areality`, `add_const_arg`) + doc in ctype.mli | ctype.ml 2130–2161, ctype.mli 309–331 | New. `add_arg` joins with a `pinned` minimum areality |
| `curry_acc` | 1006 | Uses `Curry_mode`. Passes `upper_areality` from `get_ceil marg` |
| `curry_mode_of_occurrence` | 1021 | Renamed; `Variable` stores `areality` from `get_ceil m` |
| `equate_with_curry_bounds` | 1046 | New case split. `fatal_error` on `Variable` when printing is disabled |
| `VN.curry_candidates` | 1172 | New |
| `VN.zap_non_generic_modes` / `equate_curry` | 1405 / 1424 | `equate_curry` records candidates; no longer uses a `when` guard |
| `VN.reset_names` | 2120 | Resets `curry_candidates` |
| `VN.add_visible_edges` | 2139 | **No longer** resets `suppressed_curry_heads` |
| `VN.acc_heads` | 2163 | Via `Curry_mode.comonadic` |
| `VN.curry_edges_implied` | 2173 | New conditions (upper edges into `curry_heads`; lower edges from accumulator heads or already-hidden heads) |
| `VN.is_suppressed_curry` | 2200 | New |
| `VN.equate_with_const` | 2207 | Moved into `VN`; queries the hiding decision |
| `VN.register_suppressed_curries` | 2216 | New; runs in traversal order |
| `VN.print_raw_constraints` | 2231 | Also filters **lower** comonadic edges from hidden heads |
| `VN.reserve` | 2425 | Calls `register_suppressed_curries` before the backtrack |
| `tree_of_modes`, `type modal` | 2647, 2657 | Type rename only |
| `tree_of_modal_typexp` (`not_arrow`, `Tarrow` case at 2717, alias case) | 2692 | Renames only |
| `tree_of_ret_typ_mutating` | 2990 | Calls `VN.equate_with_const` |
| `mode_intf.mli`: `With_locality.Guts.get_floor` | 1075 | Newly exposed, for `expected_floor` |

### B3. Tests for (B)

`testsuite/tests/typing-mode-polymorphism/`:
- `currying_implied.ml` and `currying.ml`: the curry tests
- `mode_polymorphism_printing.ml`: includes the two "Demonstrate lost …" commits
- `alloc_mode.ml`, `labelled_args.ml`, `subsumption.ml`, `records.ml`, `functions.ml`: these show the expected output changes

### B4. In the PR but not curry (outside both passes)

- `1d5e4ca92e`: `VN.construct_raw_bounds` (PR 2019) no longer calls
  `erase_implied_axes` (the `bound_diff` helper was deleted), so implied axes
  are printed as explicit min/max. Most of the remaining test churn comes from
  this commit. Review it with (A)'s `construct_raw_bounds`, or on its own.
