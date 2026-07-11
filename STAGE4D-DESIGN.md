# Stage 4d design + investigation: named-terms cmi residue (no-compat)

Campaign: ikind-unification (`../IKIND-REFACTOR.md` §3b/§4 stage 4, "named-terms
cmi residue"). Branch `ik/stage4d-cmi` off `981db01fb` (stage-4a+4b tip, 36/36
CI green). Line numbers from that tip. Parallel to stage 4c (printing,
cleanroom-6202).

USER DECISION IN FORCE: cmi format compatibility is NOT a constraint. Design the
format we want; bump the magic freely, no back-compat shims.

## Mandate (two layers)

1. **DECL ikind** (`type_declaration.type_ikind`, `Constructor_ikind`): verify
   what reaches the cmi today, what's dropped, what's environment-dependent; the
   named-term residue problem (stored ikinds carry Atoms/Params whose Names
   reference paths/ids that need remapping on import).
2. **CARRIER** (`base_and_axes.ikind_carrier`, stage 4a): replace 4a's
   drop-on-save with marshal + sound rehydration/relabel — OR present evidence
   that carrier persistence should wait for stage 5.

Crux the mandate names: mixmod5-style recursive-module decls, whose CLASS-B
foreign-`Param` residues are the named-terms hard case — what happens when THEY
are saved and imported.

---

## Evidence (probes; instrumentation reverted, tree pristine)

### P1. The decl `type_ikind` is already marshaled and authoritatively consumed cross-unit.

cmis marshal each `signature_item` with a generic `Marshal.to_channel oc x []`
(`file_formats/cmi_format.ml:132`), i.e. the raw `Obj.t` LDD DAG. `type_ikind`
is inside `type_declaration` inside `signature_item`, so it rides into the cmi
verbatim. It is NOT stripped on save: `Subst.type_declaration'` runs
`substitute_decl_ikind_with_lookup` on the ikind unconditionally
(`subst.ml:919`, on every action including `Prepare_for_saving`).

Cross-unit black-box probe (`_install/bin/ocamlc.opt`, `OXCAML_IKINDS_VALIDATE=1`):
a library unit `liba` with param-heavy + with-bound decls (`'a box`,
`('a,'b) pair`) consumed by `libb` reports `decl-ikind stored=38 recomputed=7`
and `mismatches=0`. So B **uses A's stored decl ikinds** (38 lookups hit a
`Constructor_ikind` loaded from `liba.cmi`) and B's view is consistent. Since
stage 2 the decl ikind is *authoritative* for sub/crossing, this consumption is
load-bearing, not validate-only.

### P2. Path atoms (Atom/KAtom) are already remapped correctly on save.

`substitute_decl_ikind_with_lookup` (`ikind.ml:1792`) rewrites every `Atom
{constr;arg_index}` / `KAtom path` through `lookup_type`/`lookup_jkind`, which on
the save path call `type_path s`/`jkind_path s` — the same local→persistent path
rewriting `Prepare_for_saving` does for the rest of the signature. Confirmed by
P1's `stored=38` with `mismatches=0`: imported ikinds whose atoms name A's own
constructors resolve correctly in B. **Nothing to fix for path atoms.**

### P3. `Unknown` atoms are already cross-unit stable.

`Unknown` carries a `Shape.Uid.t`, which embeds the defining compilation unit and
is globally unique. No collision, no remap needed.

### P4. Foreign `Param` residues DO cross the cmi boundary (the crux).

Stage 3 established stored decl ikinds are normally `Param`-free (own params are
factored positionally into the coeff array by `decompose_into_linear_terms`).
mixmod5 is the exception: its arity-0 recursive-object decls (`exp0`, `a`) carry
a **foreign** `Param id` (a live `type_expr` id, the recursive-module fixpoint
residue; stage 4b).

Save-path probe (throwaway `IK4D-SAVE-PARAM` scan in `subst.type_declaration'`,
gated to `Prepare_for_saving`, reverted): batch `ocamlc -c` of mixmod5 compiled
as a module (expect-blocks stripped) emits **11** `Param` atoms into saved decl
ikinds (ids 4633, 619, 2484, ...). So foreign-`Param` residues are written into
the cmi verbatim — `substitute_decl_ikind_with_lookup`'s `Param` branch keeps the
atom unchanged (`ikind.ml:1811`, `Param _ -> node_of_var (rigid name)`), keeping
the **stale live-`type_expr` id**.

(Note: stage 3's "batch `-c` produces 0 mismatches" was about the *validate
harness verdict*, not about whether `Param` atoms are present — they are present
in both stored and recompute in batch mode, so the harness saw no divergence. The
atoms still reach the cmi.)

Importer probe: a `usemix` unit consuming `Mixmod5_mod.LamF.exp0` in a with-bound
reports `class_b=1 residue_trusted=1` with `mismatches=0` — the importer's
CLASS-B residue-trust (stage 4b) fires on the *imported* residue, and the verdict
is consistent. Flag-on vs flag-off (`-no-ikinds`) on foreign-`Param` consumers
agree (both accept).

---

## Analysis of the foreign-`Param` residue across the cmi

### It is sound *in isolation*.

with_bounds live only on L-jkinds, always the SUB of `leq_with_reason sub super`
(stage 3); an R-jkind (super) has `No_with_bounds`, so **super is a constant**
(no rigid atoms). The residue's `Param` appears in `sub` under `⊓ coeff` then
`⊔ base` — monotone in the param. A free rigid var is universally quantified in
leq, so `∀p. sub(p) ⊑ super  ⟺  sub(⊤) ⊑ super` (monotone sub, constant super).
So the verdict depends only on `sub`'s **supremum** over the free var — identical
whether the param "means" anything in the importer or is a dangling id. For the
crossing query, a free rigid contributes ⊥ to a *guaranteed*-crossing read (safe
under-claim; ⊤-rounding would be an unsound over-claim — see §"why not ⊤").

### It carries a latent cross-unit collision risk.

Rigid vars intern by `stable_hash(Name)`; `Param n` from unit A and `Param n`
freshly minted in importer B (for B's own type var with `get_id = n`) intern to
the **same** rigid var. `type_expr` ids restart per compilation, so collision is
possible. The harm is not in leq (sup is collision-invariant even on the
diagonal, by monotonicity), but in `decompose_into_linear_terms` when B computes
one of its OWN parametric decls: `decompose ~universe:{B's param ids}` does
`assign_bot`/`assign_top` over each universe id. If a foreign residue param
shares an id with a universe param and the residue appears in that decl's body,
decompose factors the foreign gate into B's positional coeff — mis-attributing an
unconditional residue contribution to an argument, which can under-approximate
B's decl ikind on instantiation (a potential unsound accept). Narrow (requires an
id collision AND the residue in a parametric decl body) and not exhibited
deterministically here (`type_expr` id allocation is not source-controllable),
but structurally real: it is exactly the mandate's "ids that need remapping on
import."

### The fix: neutralize foreign `Param`s on save.

On the SAVE path only, map each foreign `Param id` → a fresh `Unknown uid`.
Justification, all verified against the solver:

- **Verdict-preserving.** The solver (`ldd.ml` leq/round_up/meet/join,
  `compare_var`, `map_rigid`) treats every rigid atom uniformly by id; `Param`
  and `Unknown` differ only in (a) id ordering and (b) env-lookup, where BOTH are
  "kept rigid" (`ikind.ml:275`, `Param _ | Unknown _ -> rigid_name ctx name`).
  Swapping a foreign `Param` for a fresh `Unknown` is a free-rigid-for-free-rigid
  substitution: same lattice range (up to ⊤), same leq/crossing treatment, only
  the id changes.
- **Collision-safe.** `Unknown` uids are globally unique (embed the defining
  unit), so a saved residue can never alias an importer's live `Param` id or
  another unit's residue.
- **Cross-unit consistent.** Neutralization is baked into the defining unit's cmi
  once; every importer loads the same `Unknown uid`, and re-export keeps it
  verbatim (the `Unknown` branch is already identity, `ikind.ml:1812`).
- **Save-only.** Within the defining unit, `instance_declaration`/subst keep the
  `Param` (the fixpoint value is meaningful there; stage 4b). Only
  `Prepare_for_saving` neutralizes, so within-unit precision is untouched.
- **Surgical.** Normal decls are `Param`-free, so the branch never fires for
  them; only mixmod5-shaped residues are affected.

#### Why not ⊤-round the residue on save?

Because crossing reads a free rigid as ⊥ (no *guaranteed* crossing) but a ⊤-const
as full crossing. ⊤-rounding would make the importer believe the residue type
crosses on axes it may not — an unsound over-claim on the crossing path. Keeping
a free rigid (as `Unknown`) preserves the safe under-claim. (The "recompute
rounds to ⊤" of stage 4b is the Round_up SUB-RHS derivation, not crossing.)

---

## Decisions

### D1. Neutralize foreign `Param`s in decl ikinds on the cmi save path. **BUILD.**

Thread `~for_saving:bool` into `substitute_decl_ikind_with_lookup` (the
`Subst.Ikind_substitution` ref); `subst.jkind`/`type_declaration'` pass `true`
iff `additional_action = Prepare_for_saving`. In the `Param` branch, when
`for_saving`, return `node_of_var (rigid (Unknown (fresh_unknown_uid ())))`
instead of keeping the `Param`. This removes every dangling/colliding live
`type_expr` id from cmis while preserving verdicts.

### D2. Full named-terms *explicit on-disk format* (Obj.t-decoupling). **STOP-AND-SCOPE → stage 5.**

§3b recommends replacing the raw-`Obj.t`-DAG marshal with an explicit
`to_named_terms` residue to decouple the cmi from `node_block`/`Axis_lattice`
layout. Evidence for deferring: the raw-DAG marshal is *correct* today (P1–P3;
paths remapped, Unknowns stable, and D1 removes the one unsound-capable residue).
The `Obj.t`-layout coupling is a robustness/future-proofing concern, not a
correctness one, and it bites hardest when the ikind is the SOLE representation
(stage 5) — today the legacy `mod_bounds`/`with_bounds` still coexist and are
still the printing/authority substrate in places. Building the serialize +
rehydrate + re-intern layer now adds a new bug surface (rehydration must
reconstruct sharing and re-intern rigids) for ZERO verdict change while both
representations coexist. It belongs with stage 5's format lock-in, where a magic
bump and full-world rebuild happen anyway. Recorded so it is not re-proposed as a
4d correctness item.

### D3. Carrier persistence (replace 4a's drop-on-save). **STOP-AND-SCOPE → stage 5.**

The `ikind_carrier` on non-decl jkinds is validate-only (behaviorally inert;
authority is the legacy/derive path — stage 4a). On save it is nulled
(`subst.ml:779`). Evidence for keeping the drop: an importer recomputes A's jkind
ikinds from A's persisted legacy `mod_bounds`/`with_bounds` (P1: `carrier
checks=7 mismatches=0` in libb, all recomputed) — so dropping the carrier costs
nothing today. Persisting it would (a) require the same foreign-`Param` neutral-
ization *plus* a positional↔live relabel (its free params key off live ids,
unlike the decl ikind), (b) add marshal weight, and (c) buy no authority until
stage 5 deletes the legacy fields and promotes the carrier to sole rep. Pure cost
now. Defer to stage 5, when the carrier becomes authoritative and its persistence
is load-bearing.

### D4. Cross-unit seeded-fault demonstration. **BUILD.**

The stage-4b `OXCAML_IKIND_RESIDUE_FAULT` hook perturbs the residue-trusting
*reference* within a unit. Extend it (or add a sibling fault) so it corrupts a
decl ikind *on the save path*, then show an importer's flag-on validate harness
escalates the corrupted-imported vs recompute divergence to a HARD mismatch —
proving the cross-unit boundary is not a validation blind spot. Default off;
ordinary builds byte-identical.

### Magic bump.

D1 changes cmi *content* (no more foreign `Param`s in decl ikinds; `Unknown`s in
their place). Old cmis carry foreign `Param`s a D1 compiler would not emit. No
back-compat ⇒ bump `Config.cmi_magic_number` (`Caml1999I581` → next) so a stale
cmi is rejected rather than silently mixed. Full-world rebuild via the build
system (user decision).

---

## Acceptance plan

1. **Cross-unit test (committed).** `liba` (param-heavy + with-bound) + a
   mixmod5-style recursive-module unit, consumed by `libb`; flag-on validation
   shows 0 HARD, only counted classes (class_b/residue_trusted for the residue).
   After D1: the importer sees `Unknown`s, not foreign `Param`s (assert via the
   save-path probe that 0 `Param` atoms reach any cmi).
2. **Flag-off suites green + boot-green per commit.** typing-jkind-bounds,
   typing-modules (incl. mixmod5), minimum; broader sweep for the record.
3. **Seeded-fault proof (D4).** Corrupt a persisted decl ikind; importer's
   harness counts/HARD-fails it.
4. **Magic bump** landed with D1.
5. **Verdict-preservation.** flag-on == flag-off on the foreign-`Param`
   consumers before and after D1; full flag-on sweep 0 HARD (mixmod5's within-unit
   9 stay CLASS-B per stage 4b — unchanged by a save-only change).

## Commit ordering (bank-early)

0. STAGE4D-DESIGN.md (this file). Boot-green (docs only).
1. D1 neutralization + magic bump. Boot-green; flag-off zero churn; cross-unit
   probe shows 0 `Param` in cmis; verdicts preserved.
2. D4 cross-unit seeded fault. Boot-green; fault-off byte-identical.
3. Cross-unit expect/regression test pinning the residue-import behavior.
4. Doc: append 4d disposition to `../IKIND-REFACTOR.md` §4 (D2/D3 deferred to
   stage 5 with evidence).
