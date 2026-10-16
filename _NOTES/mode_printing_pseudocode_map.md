# Pseudocode → code map

All `out_type.ml` lines are from **PR #7098** (`origin/pr-7098`, head
`1d5e4ca92e`; `git show origin/pr-7098:typing/out_type.ml`). Function names are
the same on main, except for the renames listed in section B1 of
`mode_printing_review_map.md`, which also has main line numbers.
`VN.` = inside `module Variable_names`. **[solver]** marks calls into the
mode solver / ctype that are *not* listed in the review map. Look there if
you want to check what a primitive actually does.

Pass tags: **(A)** = main printing without curry modes. **(B)** = the new
curry-mode printing.

| Pseudocode | Pass |
|---|---|
| `reserve`, up to and including the `edge_table` loop | (A). Exception: the `curry_candidates` bookkeeping inside `zap_non_generic_modes` is (B) |
| `reserve`: the `candidates` / `suppressed_curry_heads` block | (B). On main this happens at print time instead (`curry_edges_implied ~bounds_implied`) |
| `record_paths` | (A) |
| `can_elide_generic_curry_mode` | (B) |
| `print_generic_mode` | (A). Exception: the two `suppressed_curry_heads` filters are (B); main has only the `upper` one |
| `add_named_modevar` | (A) |
| "The print-time half" section | (B) |

## `reserve(type)`: `VN.reserve`, out_type.ml:2425  (A, plus B for the last block)

```
reserve(type):
    normalize_type(type)                         -- Ctype.normalize_type, ctype.ml:8526 (main) [not mode-related]
    add_named_vars(type)                         -- VN.add_named_vars 1385 (type vars only)
    if not mode_polymorphism_printing_enabled(): -- 935
        return

    snapshot = take_snapshot()                   -- Btype.snapshot  [btype.ml:763]
    zap_non_generic_modes(type)                  -- VN.zap_non_generic_modes 1405/1436
                                                 --   arrow case: curry_mode_of_occurrence 1021 on marg,
                                                 --   curry_acc 1006, then VN.equate_curry 1424.
                                                 --   NOTE: equate_curry is also where curry_candidates is
                                                 --   filled (1429); the pseudocode leaves this implicit.
                                                 --   It calls equate_with_curry_bounds 1046, which *mutates*
                                                 --   in the Const case.
    for mode in printer_traversal(type):         -- VN.add_named_modevars 1482
                                                 --   = printer_iter_type_expr 897 with fm = VN.add_named_modevar 1468
        if is_generic(mode):                     -- With_locality.check_generic  [solver] mode.ml:7233 -> solver.ml:622
            pair = describe(mode)                -- With_locality.get_{monadic,comonadic}_desc mode.ml:6999/7001
                                                 --   -> Solver.desc solver.ml:2681 -> Desc.Var.force 2592
            for head in boxed_variable_heads(pair):
                visible_vars.add(head, ())       -- VN.add_visible 1459 (only Amodevar; Amodejoin/meet are skipped)
            if pair in visible_pairs:            -- VN.eq_pair 1439 (Desc.equal, solver.ml:2632)
                aliased_visible_pairs.prepend(pair)
            else:
                visible_pairs.prepend(pair)

    for pair in visible_pairs:                   -- VN.add_visible_paths 1630
        record_paths(Monadic, pair.monadic)
        record_paths(Comonadic, pair.comonadic)
        record_paths(Closing_over, pair.comonadic)

    edge_table = []                              -- VN.add_visible_edges 2139
    for pair in visible_pairs:
        outgoing = concat(construct_edges_between(pair, other)   -- VN.construct_edges_from 1988
                          for other in visible_pairs)            --   -> construct_edges_between 1971
        incoming = construct_closing_over_to(pair)               -- VN.construct_edges_to 1979
        incoming += concat(construct_edges_between(other, pair)  --   construct_closing_over_to 1941
                           for other in visible_pairs)
        lower, upper = partition_edges_into_bounds(outgoing, incoming)  -- 2079
        edge_table.append((pair, {lower, upper}))

    candidates = reverse(curry_candidates)       -- VN.register_suppressed_curries 2216
    curry_heads = concat(heads_of_mode(mode) for mode, _ in candidates)   -- heads_of_mode 2111
    for mode, acc in candidates:
        if is_generic(mode) and can_elide_generic_curry_mode(            -- check_generic [solver]
                mode, acc, curry_heads):
            suppressed_curry_heads += heads_of_mode(mode)
    curry_candidates = []
    backtrack(snapshot)                          -- Btype.backtrack  [btype.ml:764]
```

`construct_edges_between` (1971) does more than the pseudocode shows. It
returns nothing unless `construct_edge_condition` (1800) holds. It builds
`Simple` edges from `construct_simple_between` (1853), plus `Comonadic`
edges from `construct_comonadic_between` (1882). The `Comonadic` edges are
added only if there is no monadic morphism and
`check_closing_over_candidate` (1867) fails. The morphisms come from
`construct_morphs` (1727). It returns `[C.id]` if the constant bounds already
decide the relation, otherwise a lookup in `visible_paths`.

## `record_paths(kind, description)`: `VN.find_path_from_description`, 1596  (A)

```
record_paths(kind, description):
    if description is constant: return           -- `Amode _ -> ()` (1605)
    v, f = description                           -- `Amodevar (Amorphvar (v, f))`
    for u, g in direct_generic_vlower_paths(v):  -- VN.find_paths 1575 -> paths_from_vlowers 1551 / paths_to_visible 1543
                                                 --   walks Desc.Var.Head.desc_vlower (snapshotted by
                                                 --   Desc.Var.force, solver.ml:2592); forces each var;
                                                 --   drops non-generic (`desc_level <> generic_level`);
                                                 --   stops at the first *visible* var; cycles cut by `stack`.
        h = find_visible_morph_opt(kind, u)      -- VN.find_visible_morph_opt 1498
                                                 --   NOTE: List.find_map, so it takes the *first* visible pair
                                                 --   with head u and ignores any others.
        if h exists:
            Paths.add(visible_paths, kind, (v, u),        -- Paths.add 1340
                      f ∘ g ∘ left_adjoint(h))   -- C.compose / C.left_adjoint / C.disallow_right
                                                 --   [solver] mode.ml (Lattices_mono): compose 4003,
                                                 --   left_adjoint 3792, compare_morph 3633 (Morph_set order)
```

## `can_elide_generic_curry_mode(mode, acc, curry_heads)`  (B)

No function has this name. The check is split between two functions.
`register_suppressed_curries` (2216) runs
`check_generic m && equate_with_curry_bounds m acc && curry_edges_implied ~curry_heads ~acc m`.

```
can_elide_generic_curry_mode(mode, accumulator, curry_heads):
    -- part 1: equate_with_curry_bounds, out_type.ml:1046 (top-level, NOT in VN)
    if accumulator is constant: return false     -- `Const _, true -> false` (1072)
    expected_floor = merge(                      -- 1061
        comonadic = floor(comonadic(accumulator)),   -- With_locality.Comonadic.Guts.get_floor  [solver] mode_intf.mli:700
                                                     --   of Ctype.Curry_mode.comonadic (ctype.ml 2137)
        monadic = min_monadic)
    if floor(mode) != expected_floor             -- With_locality.Guts.get_floor  [solver] mode.ml:7641 -> solver.ml:2397
       or monadic(ceil(mode)) != max_monadic:    -- With_locality.Guts.get_ceil   [solver] mode.ml:7646 -> solver.ml:2408
        return false

    -- part 2: VN.curry_edges_implied, 2173
    pair = describe(mode)                        -- VN.pair_of_mode 2157
    {lower, upper} = find_edges(pair)            -- VN.find_edges 2150
    if pair in aliased_visible_pairs: return false
    if any edge in upper is not a comonadic edge to curry_heads:   -- Upper_comonadic/Comonadic_name target ∈ curry_heads
        return false
    sources = acc_heads(accumulator)             -- VN.acc_heads 2163 (heads of Amodevar / Amodejoin)
    for edge in lower:
        if edge is a past edge from sources or suppressed_curry_heads: continue   -- Lower_comonadic
        if edge is a close edge whose simple-edge source is in sources: continue  -- Lower_closing_over_to
        return false                             -- incl. every Lower_simple
    return true
```

## `print_generic_mode(mode)`: `VN.name_of_mode`, 2316  (A, except the suppression filters)

Entry from the tree builder: `tree_of_modes` (2647) calls this when the
accumulator is `Variable`. `tree_of_modes` is called from the `Tarrow` case
of `tree_of_modal_typexp` (2742: arg modes), from `tree_of_ret_typ_mutating`
(2990: return modes), and from `not_arrow` (2697).

```
print_generic_mode(mode):
    pair = describe(mode)                        -- get_{monadic,comonadic}_desc (2317-2318)
    alias = lookup(printed_aliased_visible_pairs, pair)   -- VN.find_mode_already_printed 2298
    if alias exists: return alias

    {lo, hi} = construct_raw_bounds(pair)        -- VN.construct_raw_bounds 2019
                                                 --   dupper_lr/dlower_lr 1713/1720, erase_implied_axes 1074,
                                                 --   Const.diff, Const.Option.partial_print (mode.ml 6917/6882)
    -- rest is VN.print_raw_constraints 2231
    {lower, upper} = find_edges(pair)
    upper = remove comonadic edges whose target is in suppressed_curry_heads   -- 2235-2241
    lower = remove comonadic edges whose source is in suppressed_curry_heads   -- 2243-2250
    if lower and upper are empty and lo == "" and hi == "":
        key = construct_name(pair, {monadic: id, comonadic: id}, pair)   -- 1819
        text = add_named_modevar(key)
    else:
        text = format_bounds(                    -- the `[%s%a%s%s%a]` fprintf (~2285)
            upper = map(print_raw_upper_bound, upper), hi = hi,   -- 2049: pretty_print_monadic_morph / "past(%s)"
            lower = map(print_raw_lower_bound, lower), lo = lo)   -- 2060: pretty_print_comonadic_morph (close(...), mod)

    if pair in aliased_visible_pairs:            -- VN.mode_print_alias 2304
        alias = fresh_mode_name()                --   modename_counter + pick_name (shared counter with add_named_modevar)
        printed_aliased_visible_pairs.prepend((pair, alias))
        text += " as " + alias                   --   NOTE: code prints "as %s" right after the bounds via "%a%a",
    return text                                  --   with no space before "as"
```

## `add_named_modevar(key)`: `VN.add_named_modevar : boxedname -> string`, 2003  (A)

Same name as the registration function at 1468, which this definition shadows.

```
add_named_modevar(key):
    name = lookup(modenames, key)                -- List.find_opt with eq_boxedname 1908
    if name exists: return name
    name = fresh_mode_name()                     -- pick_name 1994 on modename_counter
    modenames.prepend((key, name))
    return name
```

## The print-time half (not in the pseudocode)  (B)

The pseudocode stops at `reserve` and `print_generic_mode`. The code that
consumes the suppression decision while printing:

- `tree_of_ret_typ_mutating` (2990) calls `VN.equate_with_const` (2207):
  `equate_with_curry_bounds m acc && (not generic || not enabled || is_suppressed_curry m)`.
  If true, the result is `Orm_no_parens` and the curry mode is not printed.
- `VN.is_suppressed_curry` (2200) checks that every comonadic head of `m` is in
  `suppressed_curry_heads`.
- `tree_of_typexp ~base` (3062) wraps everything in `wrap_mutation` (746). The
  print-time zaps get backtracked here, just as `reserve`'s are.

## Solver / ctype calls to peek at (not in the review map)

| Pseudocode | Real call | Where |
|---|---|---|
| `is_generic(mode)` | `With_locality.check_generic` | mode.ml:7233 → solver.ml:622 (`check_level_morphvar ... generic_level`) |
| "zap non-generic" | `With_locality.zap_to_legacy ~arg` / `zap_to_legacy_force` | mode.ml:7371 / 7362 (inside `curry_acc`, `curry_mode_of_occurrence`) |
| equate in the constant case | `With_locality.equate` | mode.ml:7240 |
| `floor(mode)`, `ceil(mode)` | `With_locality.Guts.get_floor` / `get_ceil` | mode.ml:7641/7646 → solver.ml:2397/2408. **Precise bounds**: see the cautions on `get_floor` in `solver_intf.mli` |
| `floor(comonadic(acc))` | `With_locality.Comonadic.Guts.get_floor` | mode_intf.mli:700 |
| bounds stored in a description | `Desc.Var.force` sets `desc_lower`/`desc_upper` from `get_floor`/`get_ceil` | solver.ml:2592 |
| curry accumulation | `Ctype.curry_mode`, `curry_mode_const`, `With_locality.Comonadic.join` (inside `Curry_mode.add_arg`) | ctype.ml 2114/2120/2146 |
| `∘`, `left_adjoint`, comparison | `C.compose`, `C.left_adjoint`, `C.compare_morph`, `C.equal_morph`, `C.apply`, `C.le`, `C.src` | mode.ml `Lattices_mono`: 4003, 3792, 3633, —, 3762, 1302, 3619 |
| `take_snapshot` / `backtrack` | `Btype.snapshot` / `Btype.backtrack` | btype.ml:763/764 |

## Places where code and pseudocode differ (worth checking during review)

1. **(A) Alias spacing.** `name_of_mode` prints `"%a%a"` and `mode_print_alias`
   prints `"as %s"`, so the output looks like `'mas 'n` / `[...]as 'n`. The
   pseudocode has `" as "`. I found no `as '<m>` output under
   `testsuite/tests/typing-mode-polymorphism/`, so mode aliases look untested.
2. **(B) `is_suppressed_curry` with no comonadic head.** `heads_of_mode` returns
   `[]` when the comonadic description is `Amode`, and `List.for_all` over
   `[]` is `true`. A generic return mode whose only generic part is monadic
   could therefore be elided at print time (`equate_with_const`) even though
   `register_suppressed_curries` added nothing for it, so
   `curry_edges_implied` never decided it. I haven't checked whether this
   can actually happen.
3. **(B) Reserve-time vs print-time accumulator.** The candidates' `acc` comes
   from `zap_non_generic_modes`, which memoizes on `visited_for_modes`, so a
   shared node is visited once with the first `acc`. At print time,
   `tree_of_modal_typexp` recomputes `acc` per occurrence. Elision is
   correct only if these agree. The comment at ~1396 says the zap pass
   "should exactly mirror" the printer.
4. **(A) `find_visible_morph_opt`** returns the first matching pair (see `record_paths` above).
5. **(A) `partition_edges_into_bounds`** has `ClosingOver _ -> assert false`
   for outgoing edges (2085). It relies on `construct_edges_from` never
   producing `ClosingOver`. AGENTS.md discourages `assert false`.
6. **(B) The filter in `print_generic_mode`** only removes edges named
   `Comonadic_name`. `Lower_closing_over_to` edges are never filtered, even
   when they come from a suppressed curry head.
7. **(B) `suppressed_curry_heads` carries over between `reserve` calls.**
   The PR removed the reset from `add_visible_edges`, so only `reset_names`
   clears the list. `errortrace_report.ml` calls `reserve` several times
   without a `reset_names` in between.
8. **(B) `curry_heads` includes curry modes that end up printed.**
   `curry_edges_implied` accepts upper edges into any candidate's head, even
   one that is never hidden. If that target is printed, the edge appears on
   the target but not on this mode, because this mode is hidden.
