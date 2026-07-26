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

### D1. Neutralize foreign `Param`s in decl ikinds on the cmi save path. **STOP-AND-SCOPE → stage 5 (revised after building it).**

Original plan: thread `~for_saving:bool` into `substitute_decl_ikind_with_lookup`;
in the `Param` branch, when `for_saving`, return a fresh `Unknown` instead of the
`Param`, removing every dangling/colliding live id from cmis while preserving
verdicts.

**Built and measured — it regresses the working residue validation.** The
neutralization IS verdict-preserving and collision-safe as designed, but the
fresh `Unknown` erases the marker that stage-4b's CLASS-B residue-trust uses to
RECOGNIZE a recursive-module fixpoint residue on import
(`stored_ikind_has_foreign_param`, `ikind.ml:810`). Measured on a cross-unit
residue consumer (`type wrap : immutable_data with Mixmod5_mod.LamF.exp0`):

| | pristine (no D1) | with D1 (neutralize) |
|---|---|---|
| importer classification | CLASS-B, `residue_trusted=1`, **0 HARD** | **HARD MISMATCH** |

So D1 (collision-safety) and CLASS-B (residue recognition) are in direct tension:
full collision-safety needs a globally-unique id, which erases recognizability;
preserving BOTH needs a dedicated residue-atom representation — which is exactly
the explicit named-terms residue form (D2). D1, D2, and the residue-atom design
are one coherent piece and belong in stage 5, where the ikind becomes the sole
representation and the residue form is redesigned anyway. Shipping the half-fix
now would regress `mixmod5`-style cross-unit validation from clean CLASS-B to
HARD. **Deferred, with the tension documented as the primary design input for
stage 5's residue representation.**

The `~for_saving` PLUMBING is retained (it is uncontroversial and needed by D4);
only the neutralization SEMANTICS are deferred.

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

### Magic bump. **NOT NEEDED for stage 4d (D1 deferred).**

The shipped stage-4d change (D4 + plumbing) does not alter cmi content — the
seeded fault is default-off and byte-identical when off. No format change ⇒ no
magic bump this stage. (A bump was prototyped for D1: `Caml1999I581` → `582`;
recorded for stage 5, where D1/D2 land and the format changes for real. Note the
bump forces a full stdlib rebuild — the whole world must be reinstalled — so it
belongs with the stage that actually changes the format.)

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
4. Doc: append 4d disposition to `../IKIND-REFACTOR.md` §4 (D1/D2/D3 deferred to
   stage 5 with evidence).

---

## RESULTS (measured on `ik/stage4d-cmi`)

### Cross-unit persistence works today (acceptance #1).

`liba` (`'a box : immutable_data with 'a`, `('a,'b) pair`) consumed by `libb`:
`decl-ikind stored=38 recomputed=7`, `mismatches=0`. A mixmod5-shaped module
consumed by a residue consumer: CLASS-B, `residue_trusted=1`, **0 HARD**. Decl
ikinds (incl. foreign-`Param` residues) cross the cmi and are consumed
consistently.

### The foreign-`Param` residue crosses the cmi (crux characterized).

Save-path probe: 11 foreign `Param` atoms reach saved decl ikinds when mixmod5 is
compiled as a batch module (`-c`). They keep stale live-`type_expr` ids. Sound in
isolation (sub-position + constant super + monotone ⇒ sup-invariant; crossing
reads a free rigid as ⊥ = safe under-claim). Latent decompose collision risk
(§analysis) — real, narrow, unexhibited, pre-existing.

### D1 built and refuted as a 4d deliverable (see D1).

Neutralizing foreign `Param` → fresh `Unknown` on save turns the cross-unit
residue consumer from clean CLASS-B into a HARD mismatch (breaks stage-4b's
residue recognition). Deferred to stage 5 with the collision-vs-recognition
tension as the design input.

### Seeded fault fires cross-unit (acceptance #3).

`OXCAML_IKIND_SAVE_FAULT` corrupts a decl ikind (`base → ⊥`) on the cmi save path
only (default off, byte-identical when off). Corrupting a persisted residue and
consuming it in another unit:

| | clean cmi | faulted cmi |
|---|---|---|
| residue consumer (`wrap`) | CLASS-B, 0 HARD | **8 HARD mismatches** |

So a deliberately-wrong persisted ikind is CAUGHT by the importer's harness, not
silently trusted — the cmi boundary and the residue-trust boundary are not
validation blind spots. (Sub-finding: SIMPLE decls like `box` are RECOMPUTED by
the importer, so their corrupted stored ikind is harmless/masked; RESIDUE decls
are authoritatively trusted from the stored ikind, which is exactly where
persistence is load-bearing and where the fault surfaces.)

### Shipped (stage 4d).

`~for_saving` plumbing through `substitute_decl_ikind_with_lookup` +
`OXCAML_IKIND_SAVE_FAULT` cross-unit seeded fault (`typing/ikind.ml`,
`typing/subst.ml`, +mlis). Validate/debug-only, default off. No cmi
format change, no magic bump. Boot-green; flag-off byte-identical.

**"Byte-identical" scope (clarification).** "Byte-identical" means the COMPILE
PATH with the detector/faults OFF (validate off, faults off) is unchanged from
base — the shipped code is entirely under `!Clflags.ikinds_validate` /
env-guarded. It does NOT mean a residue unit's cmi bytes are identical across
flag settings: flag-on and flag-off already produce DIFFERENT cmis for residue
units (pre-existing since 4b's per-derivation `Unknown` uid-minting, not a 4d
change). So do not over-read "byte-identical" as cmi-stability across flags.

### Part 3.5: cross-unit Param-id collision DETECTOR (shipped).

A validate-only, read-side detector that converts the collision hazard (below)
from silent to observable WITHOUT the stage-5 redesign and WITHOUT touching the
persistence format or CLASS-B. Mechanism: `imported_foreign_param_ids` records
every foreign `Param` id seen in an IMPORTED stored decl ikind — recorded at the
load/lookup site (`lookup_of_env`, gated on `Ident.is_global (Path.head path)`),
which is BEFORE interning, so the id's persisted origin is known without any
post-intern marker (this is precisely why the detector does NOT need the
residue-marker redesign that D1 would). Each live `Param` mint (`Solver.rigid`)
checks membership and counts overlaps (`param_id_collisions`). Surfaced in the
validate summary; default-off `OXCAML_IKIND_COLLISION_FAULT` is a seeded negative
control proving the counter can increment.

Verified: consuming the mixmod5 cmi shows `imported-foreign-params=1` (a residue
`Param` id arrives from the cmi) with `param-id-collisions=0` (no natural
collision); with the seeded control, `param-id-collisions=14` (fires). Validate
off ⇒ no summary, byte-identical.

> **POST-5b counter reading (2026-07-11):** after the `Residue` atom (commit
> `e409e1d9f`) a saved residue arrives as a `Residue`, not a foreign `Param`, so
> this detector now reads `imported-foreign-params=0` with the NEW telemetry
> counter `imported-residues=N` (mixmod5 consumer: `imported-residues=9`).
> `param-id-collisions` stays 0 (now collision-free by construction, not merely
> unexhibited). The detector is demoted from soundness-gate to corroborating
> telemetry, as planned. No committed test pins these counters (the
> `residue_import_ikinds` test is `compile_only`). A natural collision is NOT source-constructible
(`type_expr` id allocation is not controllable), so the counter reads 0 in
practice — but the hazard is now observable if it ever occurs. Over-approximate
by design (flags numeric overlap even absent a shared `decompose`); acceptable
for a detector.

**Fix-round corrections (rev_ik1 FIX-FIRST).**
- **Blind spot closed (item 1):** the original gate `Ident.is_global (Path.head
  path)` missed ALIASED imports (`module L = Foo; L.LamF.exp0` → the path head is
  the LOCAL alias, not global → imported-foreign-params=0 while the residue IS
  consumed), and likewise functor-param / local-bind imports. Fixed by keying on
  the decl's OWN uid origin (`decl_is_imported`: compare the decl `type_uid`'s
  `comp_unit` against the current unit) rather than the syntactic access path.
  Verified: direct, aliased, AND functor-arg imports ALL now record
  imported-foreign-params=1 for the mixmod5 residue (aliased was 0 before).
- **Check-site asymmetry closed (item 3):** the harm site is
  `decompose_into_linear_terms`, whose universe params are minted via `Ldd.rigid`
  directly (bypassing `Solver.rigid`). The check (`check_live_param_id`) is now
  called at ALL live-`Param` mint sites — `Solver.rigid` AND the three
  `decompose` universe-mint sites — so the harm site is covered, not just the
  general mint. Seeded control fires there (collisions counted).
- **Claim tempered:** "converts silent to observable" holds for the import shapes
  the detector covers. After the item-1 fix it covers direct/aliased/functor-arg;
  it remains best-effort telemetry, NOT a soundness proof (see the stage-5 gate
  re-point — the sound fix is unit-qualified `Residue` atoms, option (b)).

---

## KNOWN SOUNDNESS RISK (stage-5 MUST-FIX gate)

> **RESOLVED in stage 5b (2026-07-11), commit `e409e1d9f`.** The fix shipped is
> the unit-qualified `Rigid_name.Residue` atom (option (b), below), NOT the
> named-terms remap: on the cmi SAVE path every foreign `Param id` is rewritten
> to `Residue {defining_unit; id}`. A `Residue` is a DISTINCT constructor from
> `Param`, so `compare` never equates them and `decompose_into_linear_terms`
> (whose universe is the importer's own `Param` vars) can never factor a
> `Residue` — the stale-id collision is impossible BY CONSTRUCTION, not merely
> undetected. CLASS-B recognition is preserved because the distinct constructor
> IS the marker (this is exactly what the D1 fresh-`Unknown` neutralization
> destroyed). Verified: residue_import consumer shows `imported-residues=9`,
> `imported-foreign-params=0`, `param-id-collisions=0`, 0 HARD; save-fault → 8
> HARD. So the MUST-FIX gate for 5d is CLOSED independently of the named-terms
> format (which moves to the 5c/5d format-lock-in slice). The original analysis
> is kept below for the record.

**Foreign-`Param` stale-id collision in `decompose_into_linear_terms`.** A
persisted (imported) decl ikind residue carries a foreign `Param id` whose `id`
is a stale live-`type_expr` id from the DEFINING unit. Rigid vars intern by
`stable_hash(name)`, so `Param n` from the imported residue and `Param n` freshly
minted by the importer for one of its OWN type variables (`get_id = n`) are the
SAME rigid var. If the importer computes a parametric decl whose body references
the imported residue, `decompose_into_linear_terms ~universe:{importer's own
param ids}` does `assign_bot`/`assign_top` over each universe id; a shared id
conflates the residue's unconditional contribution with the argument's, factoring
it into the positional coeff. On a small (non-saturating) argument the residue's
contribution then vanishes → the importer's decl ikind UNDER-approximates →
potential unsound ACCEPT.

Properties: **real** (structural, worked through), **narrow** (needs an exact
numeric `type_expr`-id collision AND a parametric decl referencing an imported
residue), **unexhibited** (id allocation not source-controllable; the detector
above reads 0 naturally), **pre-existing** (foreign `Param`s have crossed cmis
verbatim since stage 2 made decl ikinds authoritative — stage 4d did not
introduce it). It is an unsound-accept SHAPE, so it is a **MUST-FIX gate for
stage 5** (not a soft note): stage 5 cannot delete the legacy fields (removing
the recompute-from-legacy safety net) while a persisted residue can silently
alias a live id. The fix is the named-terms residue representation (below): on
import, a residue's Names are remapped to unit-unique atoms, so no stale id
survives to collide.

---

## D1 experiment (verbatim — PRIMARY stage-5 design input)

D1 = neutralize foreign `Param` → fresh globally-unique `Unknown` on the cmi save
path. It is verdict-preserving (the solver treats `Param`/`Unknown` identically
as free rigids, `ikind.ml` `rigid_name`) and collision-safe (`Unknown` uids are
globally unique). **Built and measured on a cross-unit residue consumer** (`type
wrap : immutable_data with Mixmod5_mod.LamF.exp0`):

- **pre-D1 (baseline):** importer classifies the imported-residue divergence as
  CLASS-B (`residue_trusted=1`), **0 HARD**.
- **post-D1:** the same consumer produces a **HARD MISMATCH**.

**Mechanism: marker erasure.** Stage-4b's CLASS-B recognizes a residue by the
foreign `Param` marker (`stored_ikind_has_foreign_param`, `ikind.ml`).
Neutralizing to `Unknown` removes the marker, so the (still tighter-stored)
divergence no longer routes to CLASS-B and falls through to HARD. So D1
(collision-safety) and CLASS-B (residue recognition) are in DIRECT TENSION: full
collision-safety needs a globally-unique id, which erases recognizability. This
is why D1 is NOT shipped and NOT patched with a CLASS-B extension (recognizing
neutralized residues would mean inventing a residue marker — D2's redesign done
hackily, with over-broad-trust risk). Resolving both cleanly requires a dedicated
residue-atom representation = the named-terms format (below). This experiment is
the primary reason the named-terms persistence redesign is stage 5's content.

---

## Stage-5 named-terms persistence design (the deliverable)

USER DECISION (stage-5 gate): **print-only sidecar**. Consequences baked in
below.

### What the stage-5 cmi carries, per decl

1. **AUTHORITATIVE: the decl ikind, as an explicit named-terms payload.** Replace
   today's raw-`Obj.t`-DAG marshal of `constructor_ikind.{base,coeffs}` with
   ik4c's `to_terms : node -> (Axis_lattice.t * Name.t list) list` (base term =
   the `names=[]` element; other terms = `(coeff, atom-set)`), one term-list per
   `base` and per `coeffs.(i)`. Decouples the cmi from `node_block`/`Axis_lattice`
   internal layout (the §3b `Obj.t`-coupling robustness concern) and gives an
   explicit place to remap atoms on import. Rehydrate on load with `of_terms`
   (the canonical inverse; contract agreed with ik4c: `of_terms` interprets each
   term as `coeff ⊓ ⊓Names` and joins them, `of_terms [] = bot`, joins duplicate
   name-sets, within-term Names sorted by `Name.compare` for byte-reproducible
   cmis, does not re-mint `Unknown` uids).
2. **PRINT SIDECAR: the demoted `with_bounds` `type_expr`s.** Per the user
   decision, `with_bounds` survives stage 5 DEMOTED to print-only (ik4c proved
   surface `with`-clause syntax is irrecoverable from the LDD — `ckind_of_jkind`
   discards the `type_expr` and constructor head). They persist through the
   ORDINARY `Subst` type machinery exactly as today (they ARE `type_expr`s, so
   they remap on import via the normal type substitution, NOT via LDD-Name
   remapping). The `-print-from-ikinds` flag renders normalized from the LDD
   instead; the sidecar is only consulted by the default (faithful) printer.
3. **DELETED: `mod_bounds`.** Fully removed; derivable from the LDD for printing
   (ik4c P1). No sidecar needed.

### Name/path remap on import (the crux the format enables)

Each persisted term is `(coeff, Name.t list)`. On import, remap each `Name`:
- **`Atom {constr; arg_index}` / `KAtom path`:** remap `path` through the import
  substitution exactly as `substitute_decl_ikind_with_lookup` does today
  (`type_path`/`jkind_path` → local-to-persistent path rewriting). Already
  correct; the named-terms form just makes it explicit (remap the Name list, then
  `of_terms`).
- **`Unknown uid`:** globally unique, no remap. (Fixed at decl time; stable.)
- **`Param id` (foreign residue):** THE MUST-FIX. The stale id must not survive
  import to alias a live id (the collision above). **PRIMARY FIX = option (b):**
  a dedicated unit-qualified `Residue` atom kind in `Rigid_name.t` carrying the
  defining compilation unit, so residues are collision-free BY CONSTRUCTION (the
  unit qualifier makes two units' residues distinct atoms, and a residue can
  never equal a live `Param`) AND recognizable (a distinct constructor — exactly
  the residue-marker the D1 experiment proved is needed for CLASS-B). This is a
  soundness-by-construction fix, not a detect-and-hope one, which is why it, not
  the detector, is the deletion gate. Fallback option (a), retained only as a
  contingency: map each imported foreign `Param` to a fresh `Unknown uid` AT
  IMPORT (not save) — collision-safe, and viable where save-side D1 was not
  because the importer can recognize the residue by ORIGIN at import time (the
  key save-vs-import asymmetry) — but it loses the residue's structure and is
  weaker than (b).

### Magic-number plan

Bump `Config.cmi_magic_number` at stage 5 (prototyped `Caml1999I581 → 582`). The
format changes (named-terms payload replaces raw DAG; residue atoms remapped), so
old cmis must be rejected, not silently mixed. cmi compat is waived (user), so the
build system rebuilds the world. NB: a cmi-magic bump forces a full STDLIB rebuild
(the compiler rejects the old stdlib cmis) — so the bump lands WITH the format
change, not before.

### Transition validation (proving persisted == recomputed before deleting legacy)

Stage 5 deletes the legacy `mod_bounds`/`with_bounds`, removing the
recompute-from-legacy reference the stage-1-4 harness compares against. Before
deletion, prove the persisted LDD is a faithful substitute:
1. **Round-trip:** `of_terms (to_terms n)` semantically-equal `n` for every stored
   decl ikind (per-decl assertion under validate).
2. **Cross-unit:** the existing flag-on validate harness over a cross-unit corpus
   (this stage's `cross_unit_ikinds` test + the mixmod5 residue consumer) shows
   0 HARD with the named-terms load path — i.e. importer's LDD (rehydrated from
   the named-terms payload, atoms remapped) matches a from-legacy recompute WHILE
   both still exist. This is the last window where both representations coexist;
   run it before the delete commit.
3. **Collision detector as CORROBORATING TELEMETRY, not a sound gate.** part-3.5's
   `param-id-collisions` reading 0 does NOT by itself license deletion — a
   detector observes collisions only on import shapes it covers, and "0" is
   consistent with both "no collision" and "collision on an uncovered shape". The
   SOUND guarantee must come from the REPRESENTATION (option (b) below: unit-
   qualified `Residue` atoms are collision-free BY CONSTRUCTION — no stale id can
   survive to alias a live id). The detector then corroborates (should stay 0
   post-fix) and catches regressions, but it is telemetry, not the proof.

### Residues-in-batch invariant + boundary conditions (corrected)

CORRECTION to the earlier "residues are toplevel-only" hypothesis: **foreign-Param
residues DO reach cmis in batch `-c` compilation** — the save-path scan found 11
foreign `Param` atoms in `mixmod5` compiled as a batch module, and an importer's
CLASS-B/`residue_trusted` fires on the imported residue. So the invariant is NOT
"residues never cross the boundary"; it is the opposite, and it is why the
must-fix and detector exist. Nuance: residues arise from specific recursive-module
fixpoint shapes (mixmod5's polymorphic-variant recursive-object fixpoint
produces them; a plain `< exp : exp0 >` object recursion in the probe did NOT),
so they are rare but real. Boundary conditions that stage 5 must keep in view
(each can only ADD residue-carrying cmis, never remove the need to handle them):
toplevel-produced artifacts, `-pack`, module aliases, and functor result
signatures — any path that saves a signature containing a recursive-module
fixpoint decl. Stage 5's import-remap must therefore handle foreign `Param`s
unconditionally, not assume they are absent.

### Cheap inert plumbing NOW? (cost/benefit)

Considered landing the cmi field/format scaffolding now with an empty payload so
stage 5 only flips content. **Recommend NOT doing the format half now**, because
(a) there is no separate cmi "field" to add — the ikind already rides inside the
marshaled `signature_item`, so an "empty payload + magic bump" would still be a
full format change (magic bump ⇒ full stdlib rebuild) for zero behavior, i.e. all
of stage 5's format cost with none of its value; and (b) the named-terms payload
is only well-defined once `to_terms`/`of_terms` land (ik4c, stage 5) and the
import-remap is designed. The inert plumbing that IS worth having now — the
`~for_saving` seam and the collision detector — is already shipped this stage. So
the stage-5 diff is not meaningfully shrunk by pre-landing format scaffolding; the
seam + detector are the right amount of forward investment.
