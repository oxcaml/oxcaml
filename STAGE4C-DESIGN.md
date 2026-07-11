# Stage 4c design: print-from-ikind + the Name-preserving Ldd primitive

Campaign: ikind-unification (`../IKIND-REFACTOR.md` §3c/§4 stage 4). Branch
`ik/stage4c-print` off `981db01fb` (integration tip = stage-4a carrier +
stage-4b re-resolution, CI-green). Line numbers from that tip.

USER DECISION IN FORCE (from 4a/4b): cmi format compatibility is NOT a
constraint. Carrier is populated only under `OXCAML_IKINDS_VALIDATE` today
(stage 4a; `jkind_with_carrier`, `ikind.ml:1181-1198`).

I own the PRINTING side (out_type/Printtyp/error rendering + the new Ldd
Name-preserving decomposition primitive). ik4d owns cmi save/load and
consumes the primitive's design.

---

## Mandate (verbatim scope)

Make jkind PRINTING derive from the ikind representation instead of the legacy
`mod_bounds`/`with_bounds`, as a step toward stage 5 deleting those fields.
Scope questions to settle: which print paths are in scope; whether printing
reads the carrier or derives via Ikind from the decl; how Ldd terms map back to
printable with-bounds/mod-bounds syntax; design the deferred Name-preserving
Ldd decomposition primitive (minimal + general; also wanted by 4d).

Evidence-first: PROBE that ikind-derived output can match legacy output BEFORE
building. If round-tripping loses information printing needs, STOP-AND-SCOPE.

---

## The single print funnel (established this session)

All jkind surface rendering flows through ONE conversion:

    out_type.ml:1250  out_jkind_of_const_jkind env jkind
                        = Ojkind_const (Jkind.Const.to_out_jkind_const env jkind)
    jkind.ml:1967     to_out_jkind_const -> To_out_jkind_const.convert
    jkind.ml:1786     convert_with_base   (the workhorse)

`convert_with_base` (jkind.ml:1786-1858) reads exactly TWO legacy fields off the
(const, fully-expanded) jkind:
- `actual.mod_bounds` (the floor) -> `get_modal_bounds`/`diff`
  (jkind.ml:1810-1813) -> `Typemode.untransl_mod_bounds` -> the
  `mod portable many ...` clause.
- `actual.with_bounds` (a list of `type_expr * With_bounds_type_info.t`)
  (jkind.ml:1825-1848) -> `!outcometrees_of_types (List.map fst with_bounds)`
  renders each with-bound `type_expr` as an `Outcometree.out_type`, i.e. the
  `with (int,'b) t @@ portable` clauses. The modality/nonmodal split per clause
  comes from `axes_ignored_by_modalities` on the `type_info`.

Error messages, `-verbose-jkinds`/`kind_verbosity`, and `Printtyp` all reach the
same funnel (verbosity only toggles `diff`-vs-`actual` and expansion). So the
in-scope surface is this one function; there is no second renderer to chase.

DAG fact: `out_type.ml`/`jkind.ml` printing sits ABOVE `ikind.ml`
(`ikind.ml` has 42 `Jkind.` refs; `jkind.ml` has 0 `Ikind.` refs). So printing
CAN call into `Ikind` to derive on the fly — unlike the construction sites
(stage-4a F1), which are strictly below the solver. Printing does NOT need to
read the carrier; it can derive.

---

## PROBE RESULTS (evidence-first, the make-or-break)

### P1. mod-bounds ARE derivable from the ikind (exactly), when with_bounds empty.

For a with-bounds-free jkind the ikind is a pure constant leaf:
`ckind_of_jkind` = `const (Mod_bounds.to_axis_lattice mod_bounds)`, optionally
`meet`ed with a `KAtom` if the base is an unresolved `Kconstr`
(ikind.ml:439-449). `round_up` of that node returns the constant
(`Ldd.round_up`, ldd.ml:527-530; a `KAtom` rounds up to `top`, so the meet
collapses to the floor). And `Mod_bounds.of_axis_lattice ∘ to_axis_lattice = id`
is a documented lossless round-trip (btype.ml:1110-1120, covered by
`oxcaml/tests/typing/axis_lattice_roundtrip_test.ml`). So
`Mod_bounds.of_axis_lattice (round_up (ckind_of_jkind jk)) = jk.mod_bounds`
for the empty-with-bounds case -> byte-identical printed mod-bounds.

### P2. with-bound SURFACE SYNTAX is IRRECOVERABLE from the LDD. (STOP-AND-SCOPE)

`ckind_of_jkind_desc` (ikind.ml:452-458) folds each with-bound `(ty, info)` as

    Ldd.join acc (Ldd.meet (Ldd.const mask) (kind ~use_tables:true ctx ty))

i.e. it EXPANDS the with-bound `ty` into its full LDD `kind ctx ty`. Consequences,
all confirmed by reading `kind`/`kind_uncached` (ikind.ml:461-570) and the atom
scheme (`Rigid_name.t`, types.ml:21-69):

- `with (int,'b) t` becomes `kind((int,'b) t)` = `t`'s polynomial with `int`/`'b`
  substituted in. There is NO single atom for it; it is a multi-term
  sub-polynomial over whatever `int`, `'b`, and `t`'s coeffs contribute. The
  surface application `(int,'b) t` cannot be reconstructed.
- Even a bare `with 'a` becomes `Param (get_id 'a)` capped by 'a's jkind
  (ikind.ml:534-538) -- printable ONLY if the printer holds an
  `id -> type_expr` map, which it does not have from the LDD alone.
- The atom vocabulary is `Atom {constr;arg_index} | KAtom path | Param int |
  Unknown uid` -- `Path`/`type_expr`-id/`Uid` keyed. None carries the applied
  type_expr the `with` clause prints.

This DIRECTLY REFUTES the campaign's §3b/§3c premise that "the named-terms
residue prints the with clause": the residue's atoms are the EXPANSION's atoms,
not the with-bound's head. §3c's own hedge ("carries the Path but not
necessarily the exact surface type of a with-bound that was an applied type
constructor") is, empirically, the common case, not an edge case.

Corollary (the floor is also conflated when with_bounds is non-empty): a
with-bound that expands to a constant (e.g. `with int`, which crosses every
axis) raises the LDD's varless base leaf above `mod_bounds`. So even the FLOOR
cannot be separated from with-bound constant contributions once with_bounds is
non-empty. The LDD is a semantic normal form; the mod/with SPLIT the printer
displays is a syntactic artifact the LDD deliberately erases.

### P3. Acceptance tension.

Acceptance (1) demands the expect-corpus printed output be UNCHANGED. Full
print-from-ikind for with-bounds forces either (a) a representation that retains
the with-bound `type_expr`s (a carrier change, stage-5-shaped, not stage-4c),
or (b) a normalized rendering that DIFFERS from legacy -> violates (1). The two
cannot be reconciled inside stage 4c. Hence the flag strategy below.

---

## DECISION

**PROCEED on the derivable half; STOP-AND-SCOPE the with-bounds half (report to
team-lead for adjudication).** Concretely, stage 4c ships:

### D1. The Name-preserving Ldd decomposition primitive (owned by me; ik4d consumes).

    val to_terms : node -> (Axis_lattice.t * Name.t list) list   (* Ldd_intf.S *)

The Name-preserving analog of the existing (unexposed) `to_named_terms`
(ldd.ml:537-569): identical ZDD walk, but keeps each term's atoms as `Name.t`
instead of `V.to_string`. Contract: `inline_solved_vars` first (done
internally); RAISE on `Unsolved`/`Solved` (mirrors `to_named_terms_with`'s
Solved failure) -- a rigid-inlined stored/derived node never has them. Returns
terms in deterministic ZDD-walk order; the base term is the element with
`names = []`. This is EXACTLY §3b's residue shape
`base ⊔ Σ (coeff ⊓ names:Name list)`. Minimal + general; no printing/cmi policy
baked in. Optional inverse `of_terms` if ik4d wants a canonical reconstruction
rather than folding `const`/`join`/`meet`/`rigid`+`node_of_var` by hand.
`to_named_terms` is re-expressed on top of `to_terms` (map `Name.to_string`)
to keep one walk.

### D2. `-print-from-ikinds` dev flag, DEFAULT OFF.

A `Clflags.print_from_ikinds : bool ref` (default false), wired like `-ikinds`
into the four tools. When OFF: `convert_with_base` is byte-identical to base
(legacy read of `mod_bounds`). When ON: for a jkind whose `with_bounds` is
EMPTY, derive the floor via `Ikind` (`Mod_bounds.of_axis_lattice (round_up
(ckind_of_jkind jk))`) instead of reading `actual.mod_bounds`, then feed the
SAME `get_modal_bounds`/`diff` renderer. When `with_bounds` is non-empty, fall
back to the legacy read (documented limitation, P2). This is the honest maximum:
it flips the printing seam onto Ikind for the part that round-trips exactly, and
it is precisely the part stage 5 needs when `mod_bounds` is deleted for
with-bounds-free jkinds.

Gating rationale (from the mandate): print-from-ikind is NOT auto-gated on
`OXCAML_IKINDS_VALIDATE`; it gets its own dev flag so the default path stays
byte-identical this stage and the ikind path can be flipped on in a later
commit/stage once the with-bounds representation question is adjudicated.

### D3. Differential self-test + seeded fault.

Under `-print-from-ikinds`, assert the derived floor equals the legacy floor
(`Mod_bounds.equal`) at the point of use, counting comparisons and mismatches
(reuse the stage-1/3 validation-counter idiom). Acceptance: (i) full expect
corpus byte-identical with the flag OFF (default -> trivially, seam untouched);
(ii) full expect corpus byte-identical with the flag ON (derived == legacy for
empty-with-bounds; legacy fallback for non-empty -> identical either way);
(iii) a seeded fault (perturb the derived floor by one axis) makes the
differential FIRE and, if promoted to the printed value, makes at least one
expect test DIFF -- proving the comparison and the seam are live, same standard
as stages 1-4.

### D4. STOP-AND-SCOPE report (with-bounds printing) -> team-lead adjudication.

with-bounds print-from-ikind cannot be built in stage 4c without either changing
printed output or changing the representation. The adjudication question for
stage 5 / the carrier design (4d): does the stored carrier RETAIN the with-bound
`type_expr`s (or an `atom -> type_expr` sidecar) so printing can render `with`
clauses, OR does stage 5 accept a normalized `with`-clause rendering (and update
the expect corpus)? This is a representation decision, not a print decision, so
it belongs with the cmi/carrier owner (4d) + team-lead, not stage 4c.

---

## In scope / out of scope (print paths)

- IN: the single funnel `Jkind.Const.to_out_jkind_const` (covers Printtyp,
  Out_type, error messages, `-verbose-jkinds`), mod-bounds derivation for
  with-bounds-free jkinds, the Ldd `to_terms` primitive.
- OUT (STOP-AND-SCOPE / deferred to 4d + stage 5): with-bounds clause
  rendering from the LDD; making the carrier the sole print source (requires
  the with-bounds representation decision); `-print-from-ikinds` default-on.

---

## Commit ordering (bank-early, boot-green per commit)

1. Ldd `to_terms` (+ optional `of_terms`) in `Ldd_intf.S` + `ldd.ml`, with
   `to_named_terms` re-expressed on top. `ocamlformat --inplace`. Boot-green;
   no behavior change (new function, unused). Unblocks ik4d.
2. `-print-from-ikinds` flag plumbing (`Clflags` + `main_args` + `compenv`),
   default off. Boot-green; behavior identical (flag never true in CI).
3. Print seam: `convert_with_base` derives the floor via Ikind under the flag
   for empty-with-bounds jkinds; legacy otherwise. Requires a `jkind.ml ->
   Ikind` call -- but `jkind.ml` is BELOW `ikind.ml`, so the derivation must be
   injected as a ref (like `outcometrees_of_types`, jkind.ml:1828) set from
   `ikind.ml` at init, OR the seam lives in a module above `ikind`. RESOLVE in
   commit 3 (see Open item O1). Boot-green.
4. Differential self-test + seeded-fault proof. Boot-green.

## Open items to resolve during build

- O1 (DAG direction for the derivation call). `convert_with_base` is in
  `jkind.ml`, BELOW `ikind.ml`. Deriving the floor needs `Ikind.ckind_of_jkind`.
  Two options: (a) a `float64`-style ref `derive_floor_from_ikind : (Env.t ->
  jkind -> Mod_bounds.t) option ref` in `jkind.ml`, set by `ikind.ml` at
  startup (matches the existing `outcometrees_of_types`/`outcometree_of_*` ref
  pattern in the same module); (b) move the print-from-ikind branch UP into
  `out_type.ml` (above ikind) and pass the derived floor down. (a) is smaller
  and local to the funnel; prefer (a) unless the ref indirection is judged ugly.
- O2 (does deriving the floor even exercise anything for empty-with-bounds?).
  It is near-identity (floor round-trips). Its VALUE is proving the print seam
  consults Ikind end-to-end and that the round-trip holds across the whole
  corpus -- the stage-4-plan intent ("prints from the LDD after it is
  authoritative"). It is the honest maximum given P2, not a no-op: it is the
  first consumer on the PRINT side to read an Ikind-derived value.

---

## EMPIRICAL ADDENDUM (measured on 981db01fb, `-ikinds-debug`)

Source:
```
type 'a box : immutable_data with 'a
type ('a, 'b) t : immutable_data with 'a box with 'b
```
Dumped ikind for `t`'s jkind (`ocamlc.opt -ikinds-debug`):
```
base = ([0,0,1,3,3,1,1,3,3,0,0] ⊓ param[82]) ⊔ [2,1,0,0,0,0,0,0,0,1,2]; coeffs = []
```
- The TWO surface with-clauses (`with 'a box`, `with 'b`) collapse to a single
  `param` term + a constant floor. `box`'s `immutable_data` floor is folded into
  the const `[2,1,...]`; its `'a`-dependence and the bare `'b` are folded into
  `param` term(s). The constructor name `box` is GONE -- there is no `Atom`/
  `KAtom` for it. => the printed `immutable_data with 'a box with 'b` cannot be
  reconstructed from this ikind. Confirms P2 empirically.
- A with-bounds-FREE jkind dumps as a pure constant leaf, e.g.
  `base = [2,1,0,0,0,0,0,0,0,1,2]; coeffs = []`. `round_up` returns that leaf and
  `Mod_bounds.of_axis_lattice` round-trips it exactly. Confirms P1 empirically.

---

## AS-BUILT (commits on ik/stage4c-print, off 981db01fb)

1. `14854d40e` design doc.
2. `9ad1d0fa4` `Ldd.to_terms` primitive (Ldd_intf.S + ldd.ml). `to_named_terms`
   left unchanged (kept separate, not re-expressed) so its Unsolved-placeholder
   debug rendering stays byte-identical.
3. `f10253f92` `-print-from-ikinds` flag (Clflags + main_args, all 4 tools),
   default off.
4. `3614f6bb5` the seam: `jkind.ml` `Const.floor_from_ikind` deriver ref
   (polymorphic-record field for the allowance phantom), consulted in
   `convert_with_base`; `Ikind.mod_bounds_floor_for_printing` installs it.
   O1 resolved to option (a) (ref set from Ikind, like `outcometrees_of_types`).
5. `68e027fdd` test hooks: `OXCAML_PRINT_FROM_IKINDS` env alias (run the real
   suite flag-on with no rebuild), `print_floor_derivations` counter + at_exit
   summary GATED ON `-ikinds-debug` (so the flag never pollutes captured output;
   this bug was caught by the first flag-on `make test-one`, which surfaced the
   at_exit line as a 2-test diff), and the `OXCAML_PRINT_FLOOR_FAULT` seeded
   fault (forces the floor to top).

## RESULTS (real `make test-one`, _install)

| suite | flag-off | flag-on | seeded fault |
|---|---|---|---|
| typing-jkind-bounds | 72/72 | 72/72 | 33/72 FAIL (fires) |
| typing-modules | 54/54 | 54/54 | -- |
| typing-layouts | 45/45 | 45/45 | -- |

- Flag-on byte-identical corpus-wide; the deriver is genuinely exercised
  (`modalities.ml` alone: 4414 floor derivations, all matching the legacy read).
- O2 confirmed non-vacuous: the seam calls Ikind on the print path and the
  derived floor is used for the printed value; the seeded fault proves a wrong
  derivation would be caught (33/72 diverge). It is the honest maximum given P2
  (with-bounds surface syntax irrecoverable) and exactly the subset stage 5 can
  delete `mod_bounds` for. Files that print only with-bounds jkinds (e.g.
  `printing.ml`, `basics.ml`) show 0 derivations -> correct legacy fallback.

## STOP-AND-SCOPE disposition (with-bounds printing)

Adjudicated by team-lead/user (task #170): stage 5 keeps a PRINT-ONLY SIDECAR
of the with-bound type_exprs on the carrier, so `with`-clause printing is
restored there. Stage 4c correctly does NOT attempt it (would change output or
representation). The `-print-from-ikinds` flag is the migration seam; a later
stage flips it to derive fully once the sidecar exists.

## KNOWN RISK (documented, cleared for this stage; matters for stage 5)

`mod_bounds_floor_for_printing` calls `create_ctx`, which CLEARS the global
solver caches (`global_ty_to_kind`/`global_constr_to_coeffs`, ikind.ml:239-240).
If jkind printing ever ran while an outer ikind derivation was on the stack, the
clear would corrupt that outer computation. In practice printing happens during
error/signature FORMATTING, after the checking operation has raised and unwound,
so it is not re-entrant with a live derivation -- and the flag-on sweeps
(171/171 byte-identical, incl. recursive-module and layout tests) show no
corruption or divergence. Cleared for stage 4c (flag default off; deriver never
runs in normal builds). FLAGGED for stage 5: when the flag flips on by default,
the derivation must either be proven non-re-entrant or use a non-cache-clearing
context (a per-print scratch ctx that does not touch the globals).

---

## EXPANDED SCOPE (user decision, 2026-07-11): full ikind rendering under the flag

User resolved the stage-5 representation question as PRINT-ONLY SIDECAR (default
printing stays byte-identical forever) AND directed `-print-from-ikinds` to
render the ENTIRE jkind from the ikind: floor as before PLUS with-clauses
NORMALIZED from the LDD terms. Normalized divergence from legacy surface syntax
is ACCEPTED under the flag (opt-in). ik4d also requested `of_terms` (below).

### Ldd `of_terms` + canonical `to_terms` (commit)

- `of_terms : (Axis_lattice.t * Name.t list) list -> node` — algebraic inverse
  of `to_terms`: each term read as `coeff ⊓ ⊓names`, all joined. Total,
  order-insensitive, duplicate name-sets joined (never raises), `Unknown` Uids
  preserved (re-interned by the deterministic name hash, no re-mint). Edges:
  `of_terms [] = bot`, `of_terms [(c,[])] = const c`.
- `to_terms` now sorts each term's `Name.t` list by `Name.compare` (canonical,
  byte-reproducible); terms stay in ZDD-walk order. `to_terms bot = []`,
  `to_terms top = [(top,[])]`.
- Round-trip property test `oxcaml/tests/typing/ldd_terms_roundtrip_test.ml`
  (in the suite's dune): `of_terms (to_terms n)` semantically-equal to `n`,
  `to_terms (of_terms ts)` idempotent on canonical lists, edge/dup/sort cases.
  PASSES.

### Full rendering (jkind.ml `render_from_ikind` ref + Ikind.render_jkind_from_ikind)

`convert` consults `render_from_ikind` first. It returns `None` for
with-bounds-FREE jkinds (they keep the byte-identical floor-seam path) and on
any derivation failure; `Some` only for with-bounds jkinds under the flag. For
those it derives the ikind, `to_terms`-decomposes it, renders the base
(names=[]) term via the normal path on a synthetic with-bounds-free jkind (so
the layout/abbreviation/floor matches legacy), and folds each non-base term
`(coeff, names)` as `with (name1 & name2 … @ coeff)` (an `Otyp_stuff`),
mirroring the LDD algebra.

### Sample legacy → normalized pairs (measured, `expect` tool, flag off vs on)

    immutable_data with 'a
      -> immutable_data with param[4014] @ [0,0,0,3,0,0,0,0,3,0,0]

    immutable_data with T.t            (inside a functor result)
      -> immutable_data with T/467[16].t.0 @ [0,0,1,3,3,1,1,3,3,0,0]
         (the Atom {constr=T.t; arg_index=0} rendered as a path.index reference)

    value mod contended with 'a with 'b
      -> value mod contended with param[5742] @ [0,0,0,3,0,0,0,0,0,0,0]
                            with param[5743] @ [0,0,0,3,0,0,0,0,0,0,0]

    value mod portable external_ with 'a @@ external_
      -> value mod portable external_ with param[295] @ [0,0,0,0,3,0,0,0,0,0,0]

    value mod contended with 'a        ('a : immutable_data)
      -> value mod contended
         (the with-bound is semantically redundant on the contended axis, so the
          ikind absorbs it into the floor — the normalized form is MORE precise
          than the legacy syntactic `with 'a`)

Notes: the FLOOR (`value mod contended`, `immutable_data`, layout+abbreviation)
is byte-identical to legacy in every pair; only the `with`-clauses are
normalized. `param[N]` = a type-var reference (N is the type_expr id);
`path.index` = a constructor-atom projection; `@ [..]` = the term's
`Axis_lattice` coefficient (the per-axis contribution). Readable and faithful to
the LDD terms; divergence from legacy surface syntax is by design under the flag.

### Revised acceptance (measured)

- (a) flag-OFF byte-identical: typing-jkind-bounds 72/72, typing-modules 54/54,
  typing-layouts 45/45 (re-confirmed 72/72 after adding the convert hook).
- (b) flag-ON with-bounds-FREE render byte-identical: structurally guaranteed
  (`render_from_ikind` returns None for `No_with_bounds`, floor-seam proven
  byte-identical) and observed — files printing only with-bounds-free jkinds
  pass flag-on (e.g. modalities.ml: 4414 floor derivations, passes).
- (c) flag-ON with-bounds render normalized: 27/72 typing-jkind-bounds files
  diverge, ALL on `with`-clause lines only (bases unchanged); samples above.
- (d) coverage counters: floor derivations, with-bounds rendered, render
  fallbacks (genuinely-underivable = derivation raised) — in the `-ikinds-debug`
  at_exit summary.
- (e) seeded fault (OXCAML_PRINT_FLOOR_FAULT) still fires on the floor path.
