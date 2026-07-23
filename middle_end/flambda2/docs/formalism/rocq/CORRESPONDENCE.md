# Reading the Rocq mechanization against the formalism

This directory mechanizes the flambda2 formalism (`../*.md`, 453 rule blocks)
in Rocq 9.2. This file is the reader's guide for checking that the Rocq code
and the Markdown chapters say the same thing.

## Traceability

- One Rocq artifact per rule. Every artifact is immediately preceded by a
  comment `(** RULE <id> (CLAIM <kind>) — <chapter file> ... *)` quoting the
  rule's id, claim kind, and chapter. Premises appear in the same order as in
  the Markdown rule block. (MIGRATION IN PROGRESS, record 76: fences and
  comments not yet swept carry the retired keyword `STATUS`; the census
  invariant during the migration is byte-identity of id|keyword|value TRIPLES
  doc↔.v, so each rule's keyword flips on both sides in one flagged delta.)
- Rule id → Rocq identifier: replace each `.` with `_`
  (`OS.Let.Prim.Pure` → `OS_Let_Prim_Pure`).
- Claim kind → artifact kind:
  - defining clause (normative or interpretive) → constructor of an
    `Inductive` judgment (or a `Definition`/`Fixpoint` equation) — the γ
    family stays an Inductive;
  - property (normative or interpretive) → `Theorem ...` — `Admitted.` until
    proved; a `Qed` never changes the claim kind (it is a formal-axis badge,
    record 76);
  - descriptive → comment block + `Definition <id>_documented : Prop := True.`
    anchor (heuristics/architecture/imprecision witnesses are documented, not
    modeled);
  - EXCEPTION (sanctioned; catalog 37's decision rule): a property of any
    claim kind whose quantification is irreducibly over
    unmodeled pass internals (dacc/flow_acc/required_names/slot_offsets/
    cmx) OR over out-of-scope or absent components (the entry-40
    sharpened boundary) becomes a documented anchor: full rule-text citation,
    `_documented := True`, true CLAIM preserved in the comment. Current
    instances: seven ch. 13 `INV.*` conjectures (`Soundness.v`); the ch. 09
    anchor set (`SimplifyStructure.v`; 16 anchors — count pinned for the
    classifier's set-equality, per the semantics-bundle ruling); and six
    ch. 10 conjectures in
    `RewritesControl.v` (`S.Rewrite.LetCont.InlineForcesElimination`,
    `S.Rewrite.LetCont.ShortcutFlat`,
    `S.Rewrite.Loopify.TailrecEmitsNonRecursive`,
    `S.Rewrite.Loopify.InvariantArgElim`,
    `S.Rewrite.Loopify.SimplifyExposed`,
    `S.Rewrite.Loopify.ResimplifyIdempotent` — downwards/upwards pass
    machinery, dominator fixpoint, phase ordering, resimplify driver); and
    the eight ch. 20 discharging-invariant conjectures in `ToCmmSoundness.v`
    (`INV.ToCmm.SlotLiveness`, `.ClosureScanBoundary`, `.EffectLinear`,
    `.CallConvCoherent`, `.StaticUpdateBarrier`, `.LoweringTotal`,
    `.SymbolInitPlacement`, `.SymbolLocality` — each quantifies over to_cmm
    pass internals: slot offsets, the delayed-effect accumulator,
    calling-convention and linkage metadata; entry 70); and four
    out-of-scope/absent-component anchors sanctioned at their sites and
    list-registered at the migration's day-one mutual check (batch C):
    `P.Unchecked.FrontendInsertsChecks` (`PrimMemoryB.v` — the
    from_lambda frontend, entry 40's boundary), `T.Role.SinglePass`
    (`TypeGrammar.v` — architectural absence claim, entry 14),
    `WF.Syntax.NonRecOccursPositive` (`Syntax.v` — caller-maintained
    metadata invariant, entry 27's whitelist family), and
    `OS.Apply.Effect` (`Opsem.v` — unformalized apply kind; listing
    retires after its adjudicated descriptive flip, when the default
    anchor mapping covers it).
  - HYBRID variant of the exception (sanctioned; strictly stronger than a
    whole-rule anchor): when only SOME clauses of a property quantify over
    unmodeled internals, the modelable clauses are stated as a real
    `Theorem ... Admitted.` under the rule's id and the internals-
    quantified clauses are documented in the same comment (with the
    rule-text citation). Current instances:
    `S.Unbox.Loopify.AccumBoxElim` (`Unboxing.v` — clause (1) stated,
    clause (2) over `required_names` documented);
    `INV.Simplify.AliasesMonotoneDown` (`Soundness.v` increment 2b —
    alias-coarsening half stated, lineage/binding-time framing
    documented); `S.Rewrite.Share.StaticDynamicSplit`
    (`RewritesPrim.v` — hybrid WITH envelope-Qed: eligibility clauses
    proved by inversion on `cse_prim_pred`; types-domain and
    inverse-construction clauses documented); and `INV.ToCmm.AddrConfined`
    (`ToCmmSoundness.v` inc 4 — hybrid WITH envelope-Qed: the
    flush-quantified clauses documented, clause (i) stated on its
    code-witnessed envelope and proved outright via
    `machtype_of_kind_data_no_addr`; entry 70). Instances survive claim
    reclassification (record 76's blanket ruling — `AccumBoxElim` keeps
    this shape under CLAIM descriptive).
  - DESCRIPTIVE-AS-DEFINING-CLAUSE variant (sanctioned): a descriptive
    rule whose documented term shape is load-bearing for the development
    (an inhabited union arm, or exact fold content) may be encoded as a
    defining constructor with CLAIM descriptive preserved in the comment
    and a fidelity caveat (algorithm-dependent shape). Instances:
    `S.Unbox.ContParam.Rewrite` (`Unboxing.v`),
    `S.Rewrite.Prim.ConstFold.Float` and `S.Rewrite.Prim.ObjDupElide`
    (`RewritesPrim.v`); `CM.Context` and `CM.Syntax.Fragment` (`Cmm.v` —
    the machine cannot run without them, catalog 29; list-registered at
    the migration's day-one mutual check, batch C).
  - PROVIDED-PROP variant (sanctioned; entry 37's decision rule, KF-046's
    rewording): a rule's documented anchor may be ACCOMPANIED by a named
    `Prop` `Definition` stated for other chapters' consumption WITHOUT
    assertion — no axiom footprint, id greppable on the RULE comment,
    the anchor carrying the rule and the Prop adjacent. Instance:
    `S.Struct.JoinParams.AnalysisExtraParams` (`SimplifyStructure.v`;
    provided for chs. 10/12 entry-type reasoning, currently unconsumed).
  - ENVELOPE-QED variant (sanctioned; strictly stronger than the mapping):
    a not-yet-proved property (any claim kind) whose code-witnessed ENVELOPE is derivable in the
    encoding may be stated on that envelope under its RULE header (true
    CLAIM preserved) and proved outright (`Qed`), with the comment saying
    "stated on the envelope, proved outright". Cousin of the
    by-construction True-Qed (entry 27) and reflexivity-Qed (entry 34)
    precedents. Current instances: `T.Meet.MutableBlockMissedBottom`,
    `T.Prove.MeetShortcut.NullPremise`, `T.Prove.SimpleModeBoundary`
    (`MeetJoin.v`); `S.Rewrite.Share.StaticDynamicSplit`'s eligibility
    clauses (`RewritesPrim.v`, within its hybrid);
    `INV.ToCmm.AddrConfined`'s clause (i) (`machtype_of_kind_data_no_addr`,
    `ToCmmSoundness.v`, within its hybrid; entry 70).
  - REFLEXIVITY-QED variant (sanctioned; entry 34's precedent, list-
    registered at the batch-4 residual): a rule of any claim kind whose
    pinned transcription is definitionally true in the encoding may carry
    a reflexivity-class `Qed` under its RULE header (true CLAIM preserved;
    the comment says why the proof is free). Instance:
    `T.Meet.Store.CoercionErasure` (`MeetJoin.v`, CLAIM descriptive —
    survives reclassification per record 76's never-drop-a-Qed ruling).
  - REFUTED-CONJECTURE variant (sanctioned; KF-053, entry 73): a
    rule whose asserted property is refuted by the development's own
    record — a `Qed`'d counterexample witness in the same file — is stated
    as a named `Definition <id>_claim : Prop`, NOT asserted (zero axiom
    footprint; the rule id stays greppable on the RULE comment and the
    `_claim` name), with the comment carrying the true CLAIM, the
    demotion rationale, and the witness's name. The plain
    conjectured → `Theorem ... Admitted.` mapping deliberately does NOT
    apply: an `Admitted` universal with a `Qed`'d refutation in scope is a
    usable `False` axiom, the KF-037 poison class in purest form. Upgrade
    path (optional, polish-grade): additionally assert the claim's
    negation as a `Qed` theorem. Instance: `T.Meet.GreatestLowerBound`
    (`MeetJoin.v`; witness `T.Meet.MutableBlockMissedBottom`).
- Coverage check: `grep -roh 'RULE [A-Za-z0-9.]*' theories/ | sort -u` must
  set-equal the ids in the chapters. The regen tooling additionally parses the
  evidence lines (`VERIFIED … @ commit`, `CHECKED @ commit`, `CAVEAT …`) and
  DERIVES per-rule evidence and grades per record 76 — evidence is always
  recomputed, never trusted from disk, and a hand-written evidence summary
  anywhere is an error.
- Prose convention: never write the uppercase words `RULE`, `CLAIM`, `STATUS`,
  `CAVEAT`, `CHECKED`, or `VERIFIED` followed by an ID-SHAPED payload (a
  dotted or capitalized-token form) in prose — such text greps as a phantom
  header line. Terms of art like "the RULE comment" are fine (and are what
  the implemented scan tolerates); when in doubt, use lowercase forms — the
  uppercase tokens are reserved for the traceability headers.

## Encoding conventions (doc notation → Rocq)

| Formalism | Rocq |
|---|---|
| partial map `X ⇀ Y` (ρ, K, H, ε, ...) | `fmap X Y := X -> option Y` (function environments) |
| `m[x ↦ v]` | `fupd eqb m x v` |
| `x fresh` (w.r.t. a map) | `fresh_for m x`, i.e. `m x = None` |
| named variables/continuations/symbols/... | one wrapper inductive per namespace over `nat` (`Mk_variable`, `Mk_continuation`, ...) |
| `⟦p⟧(v̄; H) = (v, H′)` / `= undef` | relation `denot ... p vs H r` with `r : prim_result := PR_ok v H' \| PR_undef` |
| machine config `⟨e, ρ, K, H, T, R⟩` | `Record config` |
| transition `⟶` | `Inductive step ... : config -> label -> config -> Prop` |
| concretization `γ_E(T)` | `Inductive gamma E rho H : ftype -> value -> Prop` (indexed by concrete env+heap, per ch. 07 §4) |
| meet `E ⊢ T₁ ⊓ T₂ = T ▷ ε` / `⊥` | relation into `meet_res := Meet_bottom \| Meet_ok T eps` |
| oracles (`Inline?`, `Unbox?`, `⟦p⟧♯`) | `Parameter`s (the docs themselves treat these as oracles) |
| machine integers | `Z` + two's-complement `wrap` at the appropriate width |
| floats, SIMD vectors | opaque `Parameter` types with axiomatized operations (IEEE internals not load-bearing for statement fidelity) |

## Binders: named atoms, not De Bruijn

The docs are nominal and the machine semantics is environment-based (maps
keyed by *names*), so named atoms + function environments transcribe the rules
line-for-line — the property this whole file exists to protect. De Bruijn
would restructure every rule's statement. The costs are deferred to the proof
phase: `alpha_eq`, `free_vars`, `bound_vars`, `free_conts` (the
continuation-namespace sibling, needed by `S.Struct.Run.ClosedResult`), and
`static_consts_in` (occurrence of a static constant anywhere in an expr,
including nested through code bodies — needed by the KF-040 carve-out; a real
walker would duplicate the whole grammar) are `Parameter`s used only in
premises; the two substitution-using rewrite rules carry explicit
capture-avoidance premises.

## ENCODING NOTE catalog

Every place the Rocq deviates from a literal transcription carries an
`(* ENCODING NOTE ... *)` comment at the site. The known ones are cataloged
here (writers: append as you add them):

1. **The K′ knot** (`Values.v`, from `OS.LetCont.Rec`): the docs define
   `K′ = K[kᵢ ↦ Handler⟨…, K′, …⟩]` — an infinite term if stored literally.
   Encoding: the continuation-env entry for a recursive group stores the
   *syntactic* group plus its defining environments (`CE_rec`), and
   `bind_rec_group` re-ties the fixed point at lookup time. Provably the same
   unfolding; strictly positive.
2. **Two-level kinds** (`Base.v`): `kind` follows ch. 03's grammar exactly
   (`K_value | K_naked_number nnk | K_region | K_rec_info` with
   `naked_number_kind`); the flat names (`K_naked_immediate` …
   `K_naked_vec512`, incl. `K_naked_nativeint`) are provided as
   pattern-compatible `Notation`s.
3. **Variant subkind payload** (`Base.v`, `VS_variant`): the doc's
   `consts : imm set` is encoded as `list Z`, and the tag map as
   `fmap tag (block_shape * list kind_ws)`.
4. **`unarize_component`** (`Base.v`, from `WF.Arity.Unarize`): uses an inner
   fixpoint instead of `concat_map` for guardedness; definitionally
   `flat_map`.
5. **Float/vector operations** (`Base.v`): `Base.v` declares the opaque
   carriers `float64`/`float32` AND the shared per-operation `Parameter` set
   (union of ch. 05's and ch. 15's needs). Rationale: `Cmm.v` and
   `PrimScalar.v` must use *literally the same* Parameters or the ch. 18/20
   commuting statements (Cmm float op ≈ Flambda float op) would be
   unrelatable. (Supersedes an earlier decision hosting per-op Parameters in
   `PrimScalar.v`.)
6. **Rule placement**: `WF.Subkind.Erasable` and `WF.Arity.Unarize` live in
   `Base.v` (the kind/subkind/arity apparatus is Base's); the remaining ch. 03
   `WF.*` rules are in `WellFormed.v`. The effects/coeffects enums live in
   `Syntax.v` (`call_kind`'s C_call payload carries them at wave 1), so the
   RULE comments for `P.Effects.NoEffects`/`OnlyGenerative`/`Arbitrary`/
   `Coeffects` sit on those constructors in `Syntax.v`; the other axes, the
   quadruple, and the named-combination rules stay in `PrimMemoryA.v`.
7. **Machine control states** (`Values.v`): `config`'s program component is
   `control := Ctl_expr (e : expr) | Ctl_jump (k : continuation)
   (vs : list value)`, not a bare `expr`. `Ctl_jump` is the doc's synthetic
   `Apply_cont k (values v̄)` appearing in the conclusions of
   `OS.ApplyCont.Return`/`OS.ApplyCont.ExnBoundary` — it must be a real
   machine state because boundary entries chain. It carries *values*, not
   simples: machine-internal, no change to the language syntax.
8. **Poison** (`Values.v`): `simple_eval` maps `Const_poison` to `None`
   (stuck) — 02-syntax.md calls it "a deliberately-invalid value"; it has no
   runtime denotation. Stuckness, not `undef`.
9. **Storage representations** (`Values.v`): `HO_Bytes`/`HO_Bigstring` hold
   `list Z` bytes; `HO_Bigarray` holds values at *stored* width with
   decode/encode left to the denotations (per `P.Binary.BigarrayLoad`);
   `HO_Boxed` holds the naked-number value. `HO_Closures`' per-slot code
   metadata uses `arity_info := { ai_params_arity : arity;
   ai_is_tupled : bool }` with derived
   `ai_arity := length (unarize ai_params_arity)` for the §6.2 dispatch
   comparison — where `ai_arity := if ai_is_tupled then 1 else length
   (unarize ai_params_arity)`: tupled callees expect the single tuple
   argument on the generic-apply path (`OS.Apply.IndirectUnknownArity.Full`
   NOTES; ch. 20 closinfo arity negation) — (the doc leaves the metadata
   shape abstract). `HO_Bigarray`
   complex kinds (Complex32/64) store elements as unarized scalar pairs —
   two consecutive naked floats per element, `|elems| = 2·Π dims` — since
   there is no pair value form; a `BigarrayLoad` of a complex element
   allocates `HO_FloatBlock Immutable [re; im]`, with the Complex32
   float32→float64 widening `Parameter` (`f64_of_f32`) in `Base.v`'s shared
   op set (entry 5).
10. **Region placement not recorded in the heap** (`Values.v`): ch. 06 elides
    heap-vs-region placement ("the value produced and the heap update are the
    same either way") and `P.Unary.EndRegion`'s reclamation is conceptual
    (heap unchanged, only the region stack popped). `heap` stays
    `fmap heap_key heap_object`.
11. **Divergence split CompCert-style** (`Opsem.v`): the doc's single
    divergence outcome (`OS.Unit.Final`: "an infinite ⟶ sequence") is encoded
    as two behaviors — `Beh_diverge` (silent divergence after a finite trace)
    and `Beh_react` (infinite event trace, coinductive `event_stream`). Their
    union is the doc's divergence; the split avoids mixed induction/
    coinduction over possibly-infinite traces.
12. **Finite sets as lists** (`TypeGrammar.v`): Targetint/Name/String_info/
    Float sets are `list`s; membership is `List.In`; set equations are stated
    up to membership extensionality; non-emptiness stated where the docs
    require it.
13. **Environment invariants as representation invariants**
    (`TypeGrammar.v`): `T.Env.Canonical.Least`,
    `T.Env.Canonical.NoEqualsOnCanonical`, and `T.Env.Equation.Closed` are
    named `Prop` predicates on `tenv` conjoined in `tenv_wf` (the aliases
    domain is *data* — `te_canonical` — in the model). The conjectured
    persistence rules (`T.Env.ConstCanonicalPersists`,
    `T.Env.AliasesAuthoritative`) are `Admitted` theorems over `tenv_wf`;
    for AliasesAuthoritative only part (a) (path compression) is statable —
    parts (b)/(c) contrast the OCaml stored-equation representation, which
    has no counterpart when reads go through `te_canonical` by construction.
    ConstCanonicalPersists' operation-sequence framing is restated as the
    static invariant on wf envs.
14. **`tenv` scope** (`TypeGrammar.v`): drops machine_width, resolvers, and
    binding-time machinery (cross-unit import out of scope per ch. 07 §6) and
    the level stack; `te_canonical` is the path-compressed canonical map with
    coercions on aliases untracked; `te_code_age` maps newer ↦ older;
    `cut_as_extension` is the sanctioned opaque `Parameter`
    (`scope_id := nat`). Name-mode/min-binding-time floors on canonical
    lookups are not modeled. `T.Role.SinglePass` (an architectural *absence*
    claim — no widening operator) is a documented anchor.
15. **Type occurrence relation** (`TypeGrammar.v`): "free names of T"
    (`T.Env.Equation.Closed`) is a mutual inductive `tmentions`; `Rec_info`
    payloads are opaque so their occurrences aren't tracked;
    `Block_size := Z`; `String_info` = size + contents-or-unknown.
16. **Syntax elisions** (`Syntax.v`, each with a site note): `dbg` payloads;
    `Let_cont` non-rec optimizer hints (`num_free_occurrences`,
    `is_applied_with_traps`, `can_be_lifted`) — consequence: ch. 10's
    LetCont rules express use-counts as predicates over terms; `Apply`'s
    `inlined`/`inlining_state`/`probe`/`position`/`relative_history`; `Let`'s
    cached `free_names_of_body`; the unit's `used_value_slots`; name-keyed
    maps as ordered assoc lists; `Switch` arms as `list (Z *
    apply_cont_expr)`; non-opsem `code0` metadata dropped
    (`newer_version_of`'s aging relation lives in `tenv.te_code_age`).
    `alloc_mode_assign` lives in `Syntax.v` next to `Init_or_assign` (not
    `Base.v`). Effects/coeffects enums for `call_kind`'s C_call payload live
    in `Syntax.v`; `PrimMemoryA.v` reuses them. The one elided field ch. 16
    needs (`is_my_closure_used`, in `TC.Apply.Call`) is encoded as the term
    predicate "`c0_my_closure` occurs free in `c0_body`" — the flag is
    definitionally that predicate, so cache drift is impossible.
17. **Cmm chapter encodings** (`Cmm.v`): structural reduction steps for
    constants/`Cvar`/`Clet`/`Csequence`/`Ctuple` have no doc rule ids — the
    `CM_Head_*` constructors transcribe ch. 15 §1's grammar glosses.
    Linkage names modeled as injective preimages (`cmm_symbol := CS_sym |
    CS_code`); Debuginfo dropped; `Cextcall` keeps only func/type;
    name-clash renames (`RC_`/`SC_`/`MC_` prefixes, `cmm_mutable_flag`,
    `cmm_alloc_mode`). `cm_step` is parameterized over ch. 19 extension
    hooks (`head_ext`, `step_ext`, `alloc_fails`) closed by `CmmMemory.v` —
    Inductives are closed and ch. 19 adds rules to the same transition
    system; ch. 19's RULE ids live in `CmmMemory.v`. `CM.Context`
    (descriptive) is a real constructor (the machine cannot run without
    congruence; the descriptive content is the left-to-right order);
    control transfers carry the explicitly discarded context. Runtime-only
    forms `Cval`/`CV_tuple`. `cextern_c` is a local `Parameter` (the
    Cmm-typed image of ch. 04's Cextern; ch. 20 states their compatibility
    through the representation relation). Out-of-range shift amounts are
    `undef` (doc silent; to_cmm never emits them unguarded). Reinterpret
    casts with unspecified upper bits: zeroed for float patterns,
    sign-extended for int32-of-float32 (ch. 18 discipline). The
    `CM.Catch.Rec` fixed point reuses the entry-1 K′-knot encoding.
    `CM.Unit.Final` carries the whole final memory (reachability restriction
    deferred to ch. 20 via the ch. 17 relation).
18. **Loads/duplicates range over addresses** (`PrimMemoryA.v`): ch. 06's
    load/duplicate rules write `ptr ℓ` with ℓ a location, but statically
    allocated blocks live at *symbols* (`P.Static.MixedBlock` installs at the
    bound symbol) and must be loadable; the rules are encoded over addresses
    (`heap_get_addr`), i.e. locations ⊎ symbols. (Candidate doc
    clarification.)
19. **Symbol evaluation consults ρ first** (`Values.v`, `simple_eval`):
    `OS.Simple.Eval` literally says `⟦sym⟧ρ = ptr sym`, but `OS.Let.Static`
    binds static set-of-closures symbols to `clos ℓ f` in ρ, and ch. 04 §1.4
    says ρ "resolves variables and symbols to values" — a literal ptr-sym
    evaluator would strand closure projections on static closures. Encoding:
    `simple_eval` consults ρ for symbols first, falling back to `ptr sym`,
    which keeps `OS.Simple.Eval`'s equation true for every symbol ρ doesn't
    rebind. (Candidate doc clarification: OS.Simple.Eval's sym clause
    silently assumes the ρ-override from OS.Let.Static.)

20. **C-call oracle keyed by value** (`Opsem.v`, `cextern_rel`): the doc's
    `Cextern` takes the syntactic callee simple `s_f`; the encoding keys by
    the callee *value* for ρ-independence (`OS.Apply.CCall`'s premises always
    define `⟦s_f⟧ρ`). DISCIPLINE (KF-057, item 8; W-34): the refinement
    reading of ch. 13 carries a new implicit premise on this oracle —
    `cextern_rel`'s licensed answers must be MONOTONE under
    immutable-identity folding of its heap argument (sharing, dropping, or
    lifting ι-objects changes the heap PASSED TO later externs, distinct
    from the compared snapshots); any future constraint on
    `cextern`/`cextern_c` that could make extern answers fold-sensitive
    must revisit the re-posed `INV.ToCmm.Simulates`' determinacy bridge in
    the same change.
21. **Effects classification as Section Variables** (`Opsem.v`): the premises
    "p has No_effects or Only_generative_effects" / "Arbitrary_effects" of
    `OS.Let.Prim.*` are Section Variables `pure_prim`/`effectful_prim :
    prim_op -> Prop` — the classification is defined with the primitive
    denotations (`PrimMemoryA.v`, same wave); `Machine.v` instantiates.
    Relatedly `denot_R` lifts the plain and region denotations into one
    region-threaded relation (plain primitives leave R unchanged), keeping
    each `OS.Let.Prim` rule a single constructor.
22. **`_` env components** (`Opsem.v`): where a doc conclusion leaves the
    environment as `_` (e.g. `OS.ApplyCont.ExnBoundary`), the encoding uses
    `fempty` — a `Ctl_jump` state never reads ρ.

23. **Undef policy** (cross-cutting: `PrimScalar.v`, `PrimMemory{A,B}.v`,
    `Opsem.v`, `Machine.v`, `Soundness.v`): primitive denotations carry
    explicit `PR_undef` conclusions ONLY for the doc's crisply-stated
    undefined-behaviour conditions (out-of-range index, write to immutable
    array, misaligned aligned access, reaching `Invalid`). Diffuse
    "representation mismatch" undefs (NOTES prose) are left *unrelated*: the
    relation has no derivation for those inputs, so in the machine they
    manifest as stuck non-final configurations. This is faithful because
    `OS.Unit.Final` literally classifies UB as "reaching OS.Invalid **or a
    stuck state**": `Soundness.v`'s `no_ub` excludes both undef-reaching and
    stuck executions, so the headline theorems grant exactly the doc's
    license. Residual imprecision: stuck states are not sub-classified into
    representation-mismatch undefs vs other stuckness (the union is what
    `OS.Unit.Final` names, and the union is what `no_ub` excludes).
24. **γ reads E only through code-age** (`Concretization.v`): the `gamma`
    clauses consult `E` only via its code-age relation; equations/aliases
    constrain through ρ + `consistent`. Hence `gamma` under an extended env
    coincides with `gamma_E` over a fixed ρ, and
    `T.Gamma.EnvExtension`'s containment claim is definitional.
25. **Float blocks and tags in γ** (`Concretization.v`): `HO_FloatBlock`
    stores no tag (runtime `Double_array_tag` implicit); `Float_record`
    row-like cases match the object form and ignore the tag key; `get_tag`
    and tag-keyed-Unknown readings admit only tagged block objects.
26. **`consistent` made definitional** (`Concretization.v`): the doc's prose
    "ρ consistent with E" = non-bottom E ∧ ρ ranges over E's domain ∧ every
    stored equation satisfied ∧ ρ does not distinguish a simple from its
    canonical. The closures γ clause reads the doc's "contains at least" as
    slot containment at BOTH `Known` and `At_least` indices (no `Known`
    sharpening — the doc doesn't state one).

27. **Syntax.v specifics**: `comparison` renamed `prim_comparison` (`PC_*`;
    Stdlib clash). `replace_apply_cont` replaces only expression-position,
    trap-action-free `Apply_cont k` nodes, scope-aware (stops under
    rebinding of k); switch-arm actions untouched. `WF.Syntax.Anf` and
    `WF.Syntax.ContSecondClass` hold *by construction* of the syntax types
    and are `True`-statement `Theorem`s ending in `Qed` (sanctioned
    deviation from the normative-property→`Admitted` mapping).
    `WF.Syntax.NonRecOccursPositive`'s subject is an elided hint field →
    documented anchor. `WF.Syntax.StaticRecThroughCode` = no `clos_trans`
    loop in the shallow symbol-mention edge relation restricted to non-code
    definitions (equivalent to "every cycle passes through code").
    `FD_deleted` keeps `function_slot_size` (ch. 18 layout needs it).

28. **as_pointer** (`PrimMemoryA.v`, `P.Unary.IntAsPointer`): the doc's
    `as_pointer(v)` is a raw address computation not otherwise modelled, but
    it is written in function notation and `Int_as_pointer (Heap)` is
    CSE-eligible, so determinism is load-bearing for ch. 10's sharing
    rewrites: sanctioned opaque `Parameter as_pointer : value -> value`,
    used only by this rule. Contrast `ReadOffset`/`WriteOffset`, whose
    nondeterministic results encode conjectured reads of unmodelled state.

29. **Cmm memory and GC** (`CmmMemory.v`): `cmm_mem` is a record
    `{mem_bytes : fmap Z Z; mem_local : fmap Z region_handle}` — local
    allocation tags bytes as belonging to a region (per *byte*, so
    reclamation gets block extents without decoding ch. 17 headers); ch. 15
    rules touch only `mem_bytes`. `region_word` injectively embeds region
    handles into machine words (the doc uses ι as both identity and word).
    Field layout at natural sizes (vectors 16/32/64 bytes; float32 in the
    low bits of a zeroed word), agreeing with the doc's word-sized gloss when
    all fields are words. The moving GC of `CM.Alloc.GC` is the sanctioned
    opaque `Parameter gc_reloc` — the doc itself models the collector
    axiomatically, and an unconstrained existential step would *allow*
    behaviors the doc forbids; concretely-statable conditions (allocation-
    point firing, frame equalities, locals-not-moved) are real premises,
    ≈-preservation is the wave-4 Admitted conjecture. `CM.Addr.NoSurvive` is
    a syntactic discipline predicate (`expr_addr_ok`) stated
    execution-invariant (Admitted), with barrier `Cextcall`s approximated by
    the whitelist `[caml_modify; caml_initialize; caml_addr_array_initialize]`
    (an under-approximation of the dropped `alloc` flag — flagged for
    fidelity review). `CM.Alloc.Exhaustion`'s `alloc_fails` covers `Calloc`
    (OOM) and `Capply` (stack overflow) redexes; ch. 20 is stated modulo
    this outcome.

30. **Closure projections use `V_clos`, not `ptr`** (`PrimMemoryB.v`,
    `P.Unary.ProjectFunctionSlot`/`ProjectValueSlot`): the doc writes `ptr ℓ`
    for a projection's argument/result, but a closure value in the model is
    `V_clos ℓ f` (the doc's own `clos ℓ f`) — "the closure for move_to
    within the same set" identifies a closure by its slot, which a bare
    `ptr` cannot. Projections are `V_clos ℓ from → V_clos ℓ to` over the
    `HO_Closures` block at ℓ; static sets also live at fresh locations
    (`OS.Let.Static`), so no symbol-address case is needed.
31. **WriteOffset's successor heap** (`PrimMemoryB.v`, conjectured): raw
    addressing is unmodelled, so "H with the value at (b + δ) set to v" is
    an UNCONSTRAINED successor heap — the write dual of `ReadOffset`'s
    kind-constrained arbitrary result — plus an overlapping undef clause.
32. **Float16 bigarray storage** (`PrimMemoryB.v`): no 16-bit float carrier
    exists; `BGK_float16` elements are stored as widened naked floats
    rounded at store time by the local opaque `Parameter f16_round`
    (deliberately NOT in `Base.v`'s shared set — only the bigarray
    denotations use it; promote if Cmm-side accesses ever need it).
33. **Apply-block encodings** (`Opsem.v`): `rho_code` ("symbols/code ids
    visible to the body") = the SYMBOL RESTRICTION of the caller's ρ
    (symbols only accumulate along execution, so caller symbol bindings are
    exactly the visible static env; restriction drops caller locals).
    Partial application returns `V_clos` at a fresh location over an
    existential `HO_Closures` — the closure FORM is forced by the rule's
    conclusion; contents/application behavior deliberately unconstrained
    (wrapper unverified). Over-application adds an explicit `g` freshness
    premise (implicit in the doc). `OS.Apply.CCall`'s raise outcome is a
    second, tagless constructor of the same rule; a `Never_returns` C call
    may raise but its normal return is stuck. `OS.Apply.Method`'s resolved
    closure is an unconstrained existential (resolution ungrounded,
    conjectured). `P.Static.MixedBlock`'s artifact lives on the shared
    `static_const_object` install relation in `Opsem.v`.

34. **Reflexivity-Qed pattern** (sanctioned, first use
    `P.MixedShape.FieldKinds` in `PrimMemoryA.v`): a normative defining
    clause whose defining function lives in another file carries its rule id
    on a definitional restatement proved by `reflexivity` (`Qed`) — cousin
    of the `WF.Syntax` by-construction True-Qed precedent (entry 27); the
    comment must say why the proof is free (the definition *is* the doc
    equation). AMENDED (final polish wave): second instance class — a
    CONJECTURED rule whose pinned transcription is definitionally true
    because the encoding bakes the rule's content into the defining
    function may take the reflexivity-class `Qed` with its true STATUS
    preserved in the RULE comment; the comment must still say why the
    proof is free. Instance: `T.Meet.Store.CoercionErasure` (`MeetJoin.v`
    — the coercion erasure is baked into `record_on`, which stores on the
    canonical simple's underlying name looking through any coercion, so
    the pinned statement discharges definitionally; Plotkin's co-read
    find, deferred by Scott to the polish wave to avoid churning a closed
    chapter's audit state, executed post-revival by Hopper). Joins the
    Qed-under-conjectured support inventory.

35. **WellFormed.v specifics**: the ch. 05/06 per-primitive kind tables are
    Section Variables (`result_kind_table`, `prim_arg_kinds` — a relation,
    since variadic table entries admit several concrete kind lists — and
    `code_params_arity`/`code_result_arity`), universally quantified at
    the ch. 13 statements (see `Soundness.v`'s table note; entry 59) —
    deliberately never instantiated; only the entry-21 dynamic-side
    Variables are instantiated (`Machine.v`). Kinding is stated at `Coercion_id` with `WF.Kind.Coerce`
    transporting across coercions. `WF.Let.Static`'s Γ-extension is a no-op
    (Γ is variable-keyed; symbols unconditionally `Value`). The doc's
    negative rule `WF.Switch.NonEmpty` is a named predicate premised on the
    only switch constructor (non-derivability made literal). `expr_wf` has
    exactly six unannotated plumbing constructors (ch. 03 §5 states no
    congruence rules for those forms). `WF.Apply.Over/Partial` model only
    the return-arity constraints (compilation-scheme halves are ch. 16
    prose); `WF.Arity.ApplyFlavours` reduces to "return arity all
    singletons" ([`Complex`] is by construction);
    `WF.Apply.DirectArity`'s "agrees on common prefix" = componentwise
    `erase_subkind` over min-length prefixes.
36. **The static knot** (`Opsem.v`, `install_static`): `OS.Let.Static`
    installs a whole group "once" with mutual reference through code ids,
    so `install_static` threads (ρ, H) left-to-right while reading against
    the FINAL (ρ′, H′) passed in as parameters; `OS_Let_Static` ties the
    knot by feeding the relation its own outputs. Same flavor as the K′
    knot but relational, so no positivity issue. `OS.Unit.Init`'s ambient
    predefined symbols/heap are parameters ρ_pre/H₀ (doc doesn't enumerate
    them) with a premise that ρ_pre binds no variables. Static empty arrays
    are zero-length `HO_Array`s.

37. **Ch. 09 modeling stance** (`SimplifyStructure.v`): ch. 09 documents the
    Simplify *algorithm* (traversal, dacc/uacc, flow analysis, lifting
    machinery), which the mechanization deliberately does not model — the
    model's simplifier is a rewrite relation (chs. 10–12 + `Simplify.v`).
    Under the decision rule "real Prop iff another chapter leans on it",
    exactly five `S.Struct.*` rules get real artifacts, splitting
    (REWORDED per KF-046) into three unit contracts consumed by
    `Simplify.v`'s `SU_intro` (`Run.ClosedResult`, `Run.NoPendingConstants`,
    `Lift.EmptyAtEnd`) plus two join Props provided for chs. 10/12's
    entry-type reasoning and currently unconsumed (`JoinParams`,
    `Rec.NoFixpoint` — wave-4 consumer expectations that were never
    cashed: ch. 12 legitimately quantifies entry types directly, and
    ch. 10 carries no join machinery); the remaining 32 are documented
    anchors keeping their true STATUS in the RULE comment. KF-046's other
    parts were comment-only (the `.v` labels the two Props
    provided-but-unconsumed, and `ClosedResult` gained an ENCODING NOTE
    disclosing its deliberate conclusion-side widening — the code's
    free-var check is strictly tighter, a ghost region trips it too).
38. **Lifted constants** (`SimplifyStructure.v`): the lifted-constant
    accumulator is algorithm state with no term-model counterpart — in a
    term, a constant exists only where it is placed. The model-visible
    content of `Run.NoPendingConstants` + `Lift.EmptyAtEnd` (with
    `Lift.PlaceAtToplevel`'s policy) is `all_statics_placed`: the output
    unit carries no static-const binding under a lambda. Stated as
    predicates on the output unit; wave-6 files quantify over them.

39. **Nested-run trichotomy and Cmm divergence** (`Cmm.v`, from the
    `CM.Apply.Raise` doc fix): `cm_returns` gains a sibling `cm_escapes`
    (nested run to an uncaught raise); `CM_Apply_Raise` re-raises at the
    call site. `CM.Unit.Final`'s "hereditarily" is encoded by
    `cm_call_frame` + `cm_reach` (an observable run prefix that may descend
    into never-returning callees) — caller-level runs only cross a call
    when the nested run comes back, so events/divergence inside a
    never-returning callee are reachable only by descending at the call
    frame. The divergence split mirrors entry 11: `CMO_diverges` (finite
    trace then silence) / `CMO_reacts` (coinductive event stream), so
    ch. 20 relates behaviors case-by-case. Guard: `cm_reacts` is generated
    by NONEMPTY event bursts over `cm_reach`, not per-step — per-step
    coinduction can't cross never-returning calls, and the nonempty-burst
    premise keeps silently-diverging programs from spuriously inhabiting
    the reactive case with arbitrary streams.

40. **Out-of-scope-component boundary** (general rule, refining the
    `FrontendInsertsChecks` precedent): encode a rule's content when it is
    statable over in-scope syntax, even if an out-of-scope component
    computes it in the compiler (`S.Loopify.Attribute`'s decision table over
    a syntactic `is_purely_tailrec` — encoded, with the site comment noting
    the computing component); use a documented anchor only when the content
    is itself about the out-of-scope component
    (`P.Unchecked.FrontendInsertsChecks`'s content IS the Lambda lowering).
41. **Loopify metadata as judgment state** (`RewritesControl.v`): the
    `code0` elisions dropped the `recursive` flag and loopify attribute; the
    `S.Loopify.*`/`S.Code.RecursiveRecompute` rules carry them as explicit
    arguments of the code-level rewrite judgment (local `loopify_attribute`/
    `recursiveness` types, `loopify_attribute_update` as a pure table,
    `RecursiveRecompute` as a defining relation over `free_vars`) — judgment
    state instead of resurrected cache fields, so drift is impossible.

42. **Code environment for rewrites** (`Syntax.v` + all rewrite files):
    inlining must look up code bodies by `code_id`, and `tenv` (faithfully
    mirroring ch. 07) carries no code map — the compiler's Simplify likewise
    carries code in the downward env separately from the types env. All
    rewrite relations are typed `code_env -> tenv -> expr -> expr -> Prop`
    with `code_env := fmap code_id code0` (most relations ignore the
    argument; the union stays uniform). NOT a `Parameter`: code is defined
    *by the unit under compilation*, and the soundness statements must
    connect the lookup to the term's own static code bindings — an oracle
    would sever exactly the link `INV.Simplify.Preserves` needs. What the
    code table supplies in ch. 10's Apply rules: the availability premise
    (`C cid <> None` for IndirectToDirect), exact over/partial-application
    split points against the callee's parameter list, the stub's pinned
    remaining-parameter kinds, and `Apply.Invalid`'s arity-mismatch
    disjunct (recorded vs code arities after unarize + subkind erasure).
    Rewrite relations additionally take AMBIENT flags (Section
    `Variable flags : eff_flags`; closed type `eff_flags -> code_env ->
    tenv -> expr -> expr -> Prop`): the pass runs under one flag setting
    and the doc's rules assume it — per-premise `forall`-flags would be
    stronger than the doc (and flag-gated const-folds unstatable).

43. **to_cmm control translation** (`ToCmmControl.v`): Θ = (V, Φ, D) has Φ
    concrete (`fmap continuation realization` in a `tc_env` record over an
    opaque `data_state` bundling V+D); everything the doc annotates "owned
    by [18]" enters as Section Variables closed by `ToCmmData.v` (the
    extension-hook precedent of entry 17) — notably `simple_translates`
    (list form non-pointwise, since `remove_skipped_args` may drop
    arguments), `tc_flush` (flush as a pure side condition; materialization
    is ch. 18's), `classify_handler` (stands for the elided
    `num_free_occurrences`/`is_applied_with_traps` hints), and the
    indirect/extcall call images (closure-layout expansions). Direct calls
    are concrete. `is_my_closure_used` per entry 16's term predicate,
    disjunctive in `TCall_Direct`. `TC.Switch`'s tagging/tested-arm choices
    are compile-time-deterministic in code but nondeterministic in the
    relation (all permitted forms agree on in-range scrutinees; the
    `Cinvalid`-with-no-gap case is a harmless over-approximation).
    `Capply`'s result machtype and `region_close` are nondeterministic in
    `TC.Apply.Call` (ch. 18/20 constrain them). `INV.ToCmm.Control` lands
    in a second increment (it quantifies over Opsem's step relation).

44. **Terminating behaviors carry the final heap; observation up to a
    pinned location bijection** (`Opsem.v` behaviors, `Soundness.v`
    `obs_equiv`; joint Plotkin+Knuth). `OS.Unit.Final`'s "observing
    H(sym_mod)" and ch. 13 §1's "same final module block value / same
    C-call effect trace" are encoded in two layers. (1) `Beh_return`/
    `Beh_exn` carry the final HEAP, not an extracted object: the
    observation is the structure *reachable* from `sym_mod` (per ch. 15's
    "bytes of the module block reachable from the module symbol" and
    ch. 19's opaque locations), which needs the heap. (2) `obs_equiv`
    compares behaviors up to ONE partial bijection `b` on addresses per
    behavior pair, scoped over the entire behavior (every C-call event's
    values, the exception payload, the final observation), with `b`:
    identity on symbols; identity on every address populated in the shared
    starting heap H₀ (pins — the context retains pointers into H₀, so
    cross-run identity of initial-heap objects, cross-event pointer
    identity, and initial-heap objects' final contents are observable at
    the boundary); and heap-consistent (`heap_sim`: b-related addresses
    hold structurally related objects under the same b — seeding b at the
    observation roots closes over exactly the reachable substructure; the
    CompCert memory-injection shape). Trace values are compared through b,
    NOT literally (literal equality of location-carrying event values
    contradicts opacity and falsifies `INV.Simplify.Preserves` for any
    pointer-capturing module block). Closures objects are observation
    LEAVES (Simplify renames code ids, rewrites bodies, prunes dead value
    slots; structural comparison would falsify the theorem on its own
    validation examples). Ignoring mutable-through-events aliasing is
    sound only because `cextern_rel` threads real heaps through each
    event. The doc constrains none of this beyond declaring
    alpha-renaming and coercions non-observable; the bijection reading is
    the only one that neither trivializes nor weakens the theorem.
    AMENDMENT (joint, co-signed): (a) *Event payloads.* Events carry
    call-time memory — `Ev_ccall_return (callee, args, H_call, results)`
    and `Ev_ccall_raise (callee, args, H_call, payload)`. Doc grounding
    (Milner): `CM.Unit.Final` observes "the trace of Cextern effects", and
    a Cextern effect is literally an application `Cextern(func, v̄, M)` —
    call-time memory was always part of the doc's observation, so the
    payload is transcription, not invention (now pinned in `CM.Extcall`'s
    NOTES; mirrored by `CME_extern` on the Cmm side). The machine never
    inspects these labels (observation-only payload). Rejected
    alternative: instrumented runs — they would break "behaviors alone
    determine obs_equiv". (b) *Per-event comparison with cumulative
    retention.* Each event pair relates under a sub-bijection `b_ev` of
    the behavior-wide `b` (pins ⊆ b_ev; callee/args related; call-time
    heaps via `heap_sim b_ev`; results and raise payloads under `b`
    itself). `event_sim` threads a `seen` accumulator of value pairs
    retained by all earlier events (callees, arguments, results, raise
    payloads), and a `Forall` premise forces every retained pair into
    `b_ev` — so the call-time-heap comparison covers everything the
    external world can still reach, ruling out runs that mutate a C-shared
    buffer differently between calls even if they re-converge before the
    final heap (KF-015, resolved). `trace_sim`/`stream_sim` are []-seeded
    wrappers; `beh_sim`'s statement is textually unchanged. (c) *Standing
    verification.* `Vsim_clos`'s literal same-slot premise is
    code-verified independently by both signatories — exactly two
    `Function_slot` rename/create sites exist
    (`simplify_set_of_closures.ml:687`, symbol naming only;
    `simplify_apply_expr.ml:468`, the wrapper absorbed by
    `OS.Apply.IndirectUnknownArity.Partial`'s existential); the
    bijection-not-injection choice is grounded in `PhysEqual`. Doc cites
    for (a), independently verified: `15-cmm.md` `CM.Extcall` NOTES (the
    observable event is the full application `Cextern(func, v̄, M)` with
    results) and `CM.Unit.Final` ("the trace of Cextern effects");
    `Cmm.v`'s `CME_extern` field order matches `Ev_ccall_return`.
    `ToCmmSoundness.v`'s trace matching states its per-event
    correspondence with the same cumulative retention and cites this
    comparison rather than defining a parallel one.
    ITEM-8 AMENDMENT (2026-07-22; entry 75): the location relation
    generalizes from a pinned partial BIJECTION to the FOLD DISCIPLINE
    of 13 §1 — `b` is bijective on non-ι objects only (mutables and
    Immutable_unique), UNCONSTRAINED by injectivity/functionality on
    ι-objects (one shared target block may stand for two source blocks
    and vice versa — CSE sharing and re-boxing respectively), and may
    relate a dynamic ι-location to a symbol (lifting); `heap_sim`
    descends structurally through ι-objects under the same discipline.
    The shared core is HOSTED IN `Values.v` (the value-owner precedent;
    ruling (a)): one heap-parameterized, object-class-aware simulation
    relation, with identity-pinning an INSTANTIATION property — this
    entry's `obs_equiv`/`beh_sim` instantiate with the pins-and-
    non-ι-bijection discipline above, and `PrimMemoryB.v`'s PhysEqual
    premise takes the DIAGONAL instance (fold-equality within one heap
    is simulation against self). The fold is INDUCTIVE under an explicit
    acyclicity premise (ruling (b)): dynamic ι-allocations cannot
    self-reference (their constructors take already-evaluated values),
    static recursion passes through code ids
    (`WF.Syntax.StaticRecThroughCode`), closures and non-ι objects are
    relation-leaves — descent is well-founded. TRIPWIRE (W-35): real
    OCaml's `let rec x = 1 :: x` builds cyclic immutables via the
    alloc_dummy/update_dummy MUTATION idiom, absent from the model (and
    if modeled, the block is built Mutable, hence a leaf); any future
    extension minting cyclic ι-objects breaks inductive totality and
    must revisit the fold. Closures remain LEAVES, now with the full
    rationale (ruling (c)): identified through the location relation —
    non-injective on ι, so one target closure stands for two shared
    source closures, while literal slot equality keeps distinct slots of
    one set distinct — never structurally recursed; this simultaneously
    delivers the sharing license and keeps fold-to-1 underivable on
    distinct closures under the diagonal instance, matching the
    prover's never-fold-closure-equality discipline. `obs_refines` is
    `obs_equiv`'s second conjunct alone (every transformed-side behavior
    has a source-side match); the reflexivity-lemma family (identity
    relation on non-ι, structural reflexivity on ι) is LOAD-BEARING
    twice — it witnesses 13 §1's "identity-free programs get both
    directions" sentence AND constructs the code-matching abstract
    behavior in the re-posed `INV.ToCmm.Simulates`' determinacy bridge.
    The event apparatus of amendment (b) keeps its shape; only the
    `b`/`b_ev` discipline changes as above.

45. **The control simulation statement** (`ToCmmControl.v`,
    `INV.ToCmm.Control`): the control relation is encoded at the doc's own
    granularity — `ctrl_kenv` links each Φ realization to a CLASS of
    K-entry (Jump/Inline/Return/Exn); `ctrl_traps` is a `Forall2` of
    handler-realization identities; R ≈ RR is *equality* (both are handle
    lists; ch. 19's one-to-one correspondence). The matched transitions
    are restricted to silent (`L_tau`) steps between `Ctl_expr` endpoints
    (`Ctl_jump` is machine-internal, entry 7; value-level and
    event-emitting rows belong to chs. 17/20), and "H ≈ M preserved
    unchanged" reads as H′ = H ∧ M′ = M under that restriction. Stated
    over the open ch. 18 hooks — read under the intended `ToCmmData.v`
    instantiation (arbitrary instantiations could falsify it; provability
    is ch. 20's business, per the rule's own "specialization" NOTES).
    GRANULARITY caveat (recorded, no doc change): a Flambda `OS.Apply`
    step enters the callee small-step while `CM.Apply` big-steps the call,
    so the Apply row is not step-for-step matchable at the call boundary —
    the eventual proof uses ch. 20's call-frame devices.

46. **`code0_wf`** (`WellFormed.v`, from fidelity finding W-23): the binder
    disciplines the doc assumes by α-fiat for code bodies — kret ≠ kexn,
    `NoDup` parameter names, body free continuations ⊆ {kret, kexn} —
    stated as one predicate, carried by ch. 13's code-tying WF hypothesis
    (NOT premised in rewrite rules: the doc discharges these as syntax
    well-formedness, not inlining preconditions). The fourth conjunct
    (my_region ≠ my_ghost_region) was initially believed unstatable
    ("code0 elides the region binders") — REFUTED by fidelity finding
    KF-023: both names live inside `c0_my_alloc_mode` (`App_local r g`),
    and r = g would send callee ghost uses to the caller's ordinary
    region through the sequential renames. The conjunct is stated as a
    match on that field; no shape change.

47. **Layout and runtime-constant oracles** (`CmmMemory.v`,
    `Representation.v`; the `data_ptr`/`gc_reloc` category): sanctioned
    opaque `Parameter`s for values the doc names but treats as given —
    `header_is_local : Z -> Prop` (from fidelity finding KF-009; restores
    `CM.Alloc.Local`'s caml_local header premise);
    `fun_slot_offset : function_slot -> Z` and
    `val_slot_offset`/`val_slot_is_scanned` (Slot_offsets, per R.Val.Clos's
    own "assigned by Slot_offsets and treated as given here");
    `generic_entry_word : arity_info -> Z` (caml_curryN /
    fail_if_called_indirectly — runtime symbols outside the modeled symbol
    space); `custom_ops_word : string -> Z`, `bigarray_ops_word : Z`,
    `ba_flags_word` (custom-block ops pointers and bigarray flag words);
    `unboxed_array_base_tag : array_kind -> Z` (the doc pins only the
    vector tags and witnesses one base in a NOTES example). Each declared
    ONCE and shared across all consuming rules — cross-rule consistency is
    the sanction's substance.
48. **Parameter-characterization theorems** (convention): a sanctioned
    opaque `Parameter` may be pinned by an `Admitted` support theorem
    (naming convention `*_spec`, e.g. `header_is_local_spec` in
    `Representation.v`: the color-bits reading), carrying NO rule id and a
    comment citing the sanctioning catalog entry. These are
    axiom-adjacent: they appear in `Print Assumptions` and are audited as
    part of the sanctioned-Parameter inventory, not the rule ledger.
    SIBLING CLASS (obligation kernels): an `Admitted` support theorem with
    NO rule header that states an obligation an ENCODING NOTE hands off
    (typically the semantic content of a disclosed under-approximation),
    citing the sanctioning catalog entry in its comment. Same audit
    treatment. Current instances: `TC_Switch_TestByValue`
    (`ToCmmControl.v` — the must_tag selection obligation from the KF-032
    under-approximation note, entry-62 family).
49. **`hdr_word` takes the ENCODED prefix field** (`Representation.v`;
    doc finding #10, fix APPLIED to 17-representation.md): ch. 17 uses R.Header's fourth
    parameter inconsistently (raw scannable prefix vs encoded field —
    mixedness as an implicit fifth input); the encoding takes the encoded
    `prefix_field` (0 non-mixed, p+1 mixed, Array's already-encoded
    prefix passes through) and the doc amendment restates `hdr`
    accordingly.

50. **Meet/join relational envelope** (`MeetJoin.v`): `meet` is a
    derivation relation, not a function; corners the doc leaves
    algorithmic are *deliberately unrelated* (no constructor, nothing
    claimed): componentwise array/closure meets, multi-tag row-like meets,
    the structural Value-head join (Unknown soundly over-approximates),
    `R_lift` reification, Heap-vs-Local alloc-mode meet, `expand_head` on
    constant canonicals. `stored_type` over-approximates constant simples
    to unknown-of-kind (exact constant types are concretization-side) —
    hence `expand_head`'s const-exclusion premise. Left-biased slack is
    all in the γ-enlarging (sound) direction (null witnesses, variant
    extensions, block-case extensions). `T.Meet.AliasAlias` is one
    constructor with a wlog binding-time premise. `T.Join.ConstAgreement`:
    the flag gate is out of model; per-branch envs collapse to the single
    E; KNOWN TRANSCRIPTION GAP (flagged): the dead-branch premise
    (`ftype_is_bottom = false`) does not look through an alias resolving
    to a Bottom stored type. `prove_single_closures_entry` is an
    auxiliary, id-free success-case model. Status upgrades: three
    conjectured ch. 08 rules are PROVED outright
    (`T.Meet.MutableBlockMissedBottom`,
    `T.Prove.MeetShortcut.NullPremise`, `T.Prove.SimpleModeBoundary`),
    plus the Tier-2 pilot `meet_sound_naked_float` (a set-intersection
    instance of `T.Meet.Sound`).

51. **The descent lineage** (`MeetJoin.v`, hosting the `tenv_define` /
    `tenv_extends` / `tenv_descends` trio for
    `INV.Simplify.AliasesMonotoneDown`): `tenv_extends` is OPERATIONAL —
    `TE1_alias` transcribes `aliases.ml#add`'s demotion (earlier-binding-
    time canonical wins, demoted class redirected pointwise) — so the
    coarsening conjecture is a genuine induction, not a corollary of its
    own definition. Encodings: (a) the merged canonical map is
    characterized extensionally, not constructed (no boolean equality on
    `simple` is constructible — consts carry opaque floats); (b)
    `TE1_type` discards the inner meet's extension (the established slack
    family), and a `Meet_bottom` outcome has no step — bottom-discovering
    extension application aborts the descent in the code, unmodeled in
    the lineage; (c) fold order over the extension is existentially
    chosen (function-typed fmap has no intrinsic order); (d) `tenv_define`
    enters unknown-of-kind at a binding time strictly above all existing
    entries; a binding-time TIE in the demotion premise is deliberately
    nondeterministic (the code's name-mode/definition-order tiebreak is
    not resolved). Related fix: `T.Join.ConstAgreement`'s dead-branch premise
    now reads provable bottomness (`meet E Tᵢ Tᵢ = ⊥`), which looks
    through aliases — the entry-50 transcription gap is CLOSED; the
    collapsed-per-branch-env half remains documented slack (undoing it
    would change `nway_join`'s arity against compiled-in consumers).

52. **Representation-relation encodings** (`Representation.v` inc. 2):
    object colors are existential over the two header bit patterns
    (`col_ok`) — placement data is deliberately absent from H (entry 10).
    `rep_obj` omits the doc's "H ⊢" context (no clause consults H; pointer
    fields go through `rep_val`'s L; the context actually needed is SA,
    link-time symbol addresses, for code pointers and R.Heap's symbol
    clause). `R.Obj.Array`'s unboxed-product clause constrains only the
    header shape — the doc defines no component-offset seam for arrays;
    packing is deferred to ch. 18's access images. Closures'
    total_size/startenv/is_last are existential Slot_offsets outputs
    (startenv SHARED across slot constraints — that sharing is the rule's
    content); unscanned value-slot words read at an existential chunk.
    Bigarray float16 bytes are length-constrained only; complex kinds
    store unarized halves at half stride (entry 9). `R.Observe`'s trace
    equality is `Forall2` over a Section-parameterized event relation
    (event vs cm_event differ in type; equality grounds in the shared
    cextern oracle; ch. 20 instantiates per the KF-015 discipline). GC
    preservation is `gc_reloc_preserves_rep_heap` (Admitted) with NO
    second CM.Alloc.GC header.

53. **The simplification closure** (`Simplify.v`): `simplifies` holds the
    typing environment FIXED through congruence — threading
    `tenv_descends` at binder crossings was considered and REJECTED:
    `tenv_extends` applies an arbitrary unvalidated extension, so a
    descends-threaded closure would admit derivations under unsound
    environments and falsify `INV.Simplify.Preserves` as stated over it.
    Fixed E under-approximates the pass on the sound side (env evolution
    is dacc machinery, out of model). Congruence rewrites one subterm per
    step; simultaneous/repeated rewriting is recovered through
    transitivity (pilot-witnessed, including group-wide rec-handler
    simplification). `simplify_unit` models the unit-to-unit meaning of
    `run` only; side results (free_names, final typing env, all_code,
    slot offsets) are unmodeled, with ch. 09's contract entering as the
    three `SimplifyStructure.v` output Props. The file hosts no rule ids.

54. **Control-rewrite refinements from review** (`RewritesControl.v`):
    `S.Rewrite.Apply.Invalid`'s arity-mismatch disjunct mirrors the code's
    `arity_mismatch` (prefix, per-parameter, unarized-kind comparison;
    result-arity check only for exact applications) — plain arity
    inequality was refutable-`Preserves` as stated (fidelity finding
    KF-025). `S.Rewrite.Apply.PartialApplication`'s freshness is pinned
    pairwise for the quantified stub-internal names (two `NoDup`
    premises) — prose freshness protects only named binders (KF-026).
    `S.Rewrite.Apply.IndirectToDirect` licenses only the
    resolution-lands-on-the-type's-cid direction of
    `Code_age_relation.meet_set` (documented under-approximation; the
    converse redirect case is deliberately unlicensed).
    `S.Rewrite.LetCont.Shortcut` follows the KF-024 keep-binder amendment
    (APPLIED to both the chapter and the `.v`): the conclusion retains
    the `Let_cont` binder per `rebuild_let_cont`'s keep-until-zero-
    occurrences; `LetCont.DeadHandler` composes to recover the
    binder-dropping form; the interim Apply-exclusion premise is gone.
    The `to_alias` pure-alias case is a companion constructor under the
    same id (the two-constructors-under-one-id pattern, as for
    `rw_unbox`) which ALSO retargets Apply return/exn uses, mirroring
    `apply_continuation_aliases` — the only case where the code touches
    Apply positions (`continuation_shortcut.ml#to_alias`).

55. **The headline statements** (`Soundness.v` increment 2b; all twelve
    ch. 13 ids on disk). `INV.Simplify.Preserves` carries two premises
    with no doc counterpart, both implicit abstraction-soundness ties:
    `consistent E0 ρ_pre H0` (the initial tenv soundly describes the
    ambient runtime — without it the conjecture is FALSE outright: a
    lying E0 licenses a behavior-changing arm prune) and C-carried-by-H0
    (every table-bound code id pre-installed; covers imported code).
    Unit-internal code-version coherence is documented as outside the
    envelope — the code-side twin of entry 53's fixed-E
    under-approximation; both narrow coverage on the sound side.
    `INV.Rewrite.Local` is stated at the config level with three
    documented deviations: E's soundness taken at the context's start
    (hole-internal E-facts narrow coverage), reachability replaced by
    consistency-quantified machine states, and an EXPLICIT `no_ub`
    premise — the doc's rule text omits the modulo-UB license but the
    statement is false without it for Invalid-introducing rewrites
    (deferred clarification: the rule block should state it, as its
    section-1 sibling does). `INV.NameMode.Coherent`: no debug positions
    in modeled syntax, so the Phantom clause sharpens to "no free
    occurrence at all"; mode ≠ Normal covers Phantom and In_types in one
    clause. DISCLOSURE (fidelity finding KF-030; REWRITTEN at the item-8
    resolution — ruling (d), partial re-entry): the rewrites union
    excludes `CSE.Replace`/`Extend` and `Loopify.SelfTailCall` on the
    STRUCTURAL ground alone, which stands and is item-8-independent —
    the CSE table is denv state, so they live in side judgments with the
    table an explicit argument (entry 56) and cannot be union arms. The
    former SECOND ground is DISSOLVED: CSE.Replace's local obligation was
    false as stated only against the pre-resolution equivalence (the
    item-8 witness in a one-let context); under `Rewrite.Local`'s
    refinement reading it is true-shaped ({true} ⊆ {true, false}), so
    the local refinement obligation on `rw_cse` is now STATABLE — booked
    as a wave/follow-up item (it hands a future `Preserves` proof
    exactly the per-arm lemma the old text said was impossible); until
    it lands, the `S_cse` and `simplify_code` arms keep their composed
    obligations.

56. **Prim-side rewrites** (`RewritesPrim.v`): the CSE table is denv
    state, so `CSE.Replace`/`Extend` live in side judgments
    `rw_cse`/`cse_extend` with the table an explicit argument
    (pointwise-updated; no decidable equality on `prim`), NOT arms of the
    union (the code-level-judgment precedent). "The prover proves s = c"
    is `prove_equals_to_simple_of_kind` on the alias-type view,
    representing the per-kind `meet_*` Known_result family. The general
    ConstFold constructor excludes float arithmetic — floats fold only in
    the flag-gated `ConstFold.Float` clause (ambient flags). The
    type-determined is_int fold is a fourth constructor stated on
    `prove_is_int`. `Projection` transcribes the fast-path group only
    (the length+load group is the Transfer+Reify composition, documented).
    `CompareRecovery` is a catalog-37 anchor (quantifies denv's
    comparison_results). `Parameter prim_arg_kinds : prim_op -> list kind
    -> Prop` is the sanctioned ch. 03 argument-kind table (the same table
    `WellFormed.v` defers as a Section Variable; a Variable here would be
    pruned or would widen the frozen 5-argument type). FINDING #16 is
    recorded at the CSE site: the doc's share-immutables license
    (`CSE.Eligible`/`Replace` NOTES) conflicts with deterministic
    `P.Binary.PhysEqual` — transcribed faithfully on BOTH sides;
    resolution is a formalism design decision (see the known-discrepancies
    escalation).

57. **The loopify pipeline in the closure** (`Simplify.v`, from fidelity
    watch W-24): `stc_deep` is the scope-stopped congruence closure of
    `rw_self_tail_call` — descent stops at continuation binders rebinding
    k/kret/kexn (k against capture of the emitted apply_cont; kret/kexn
    because those premises are syntactic name comparisons), with no
    code-body frame (loopify state is per-function) and my_closure under
    the closure-wide fixed-E convention. `simplify_code` stages the
    rebuild inside-out (redirects under the wrap's own k-binder, then
    plain `simplifies` on the wrapped body) and re-runs
    `S_Rewrite_Code_RecursiveRecompute` on the FINAL body, discarding the
    wrap-time flag — which is what makes `Non_recursive` reachable. At
    the code-rebuild frame the loopify attribute is a quantified input
    (closure-conversion metadata, not a `code0` field per the
    no-cache-fields ruling). `simplifies` is mutual with
    `loopify_simplifies`/`simplify_code`; closed types unchanged.

58. **`env_rep` covers symbols** (`ToCmmData.v`; the ch. 18 companion of
    entry 16): `TC.Simple`'s correctness premise "ρ ~ ce" is written in
    the doc over variables only, but the entry-16 encoding consults ρ
    first for SYMBOLS too (the static-closure fix), so `env_rep` gains a
    matching symbol conjunct — without it the sym clause is false for
    ρ-rebound static-closure symbols. `symaddr_agree` (L agrees with the
    program's symbol addresses per R.Heap) is a named Definition premise,
    not a Parameter. Increment order within ch. 18 deviates from the doc's
    section order (tc_prim precedes TC.Let.* — the Let rules quantify over
    the per-primitive emissions), documented in the file preamble.

59. **Universal kind-table quantification** (`Soundness.v`):
    `WellFormed.v`'s `expr_wf` abstracts four kind/arity tables (Section
    closure), and no canonical instantiation exists in the development, so
    `INV_Simplify_Preserves` and `INV_Rewrite_Local` quantify the tables
    universally and pass them through. At the ch. 05/06 canonical rows
    this specializes to the doc's "Γ ⊢ U ok" premise; the universal form
    in principle STRENGTHENS the conjectures (wf satisfiable under
    non-canonical tables too). Recorded decision: exporting a canonical
    table assignment and specializing is strictly statement-weakening and
    can land at any time without re-audit risk.

60. **CSE in the closure** (`Simplify.v`, `cse_deep`): the CSE table from
    `RewritesPrim.v` is threaded DOWN the traversal — sound where
    tenv-extension threading wasn't (the mirror image of entry 53's
    fixed-E decision) because `cse_extend` validates each entry against
    the defining expression it crosses, whereas `tenv_extends` applies
    unvalidated extensions. One replacement per firing (repetition via
    transitivity); the table starts empty (validity = reachability from
    the empty table); handler frames thread the table; join-point
    by-scope filtering stays documented as dacc machinery; no code-body
    frame.

61. **Primitive emission images** (`ToCmmData.v` inc 2): `box_number_image`
    reproduces `R.Obj.Boxed` byte-for-byte (same `hdr_word` calls and
    ops-word strings) so ch. 20's commuting obligations relate the two
    directly; write barriers are explicit `Cextcall` images
    (caml_modify/_local/caml_initialize); `NumConv` is a total dispatch
    with int→float32 as ONE `Cstatic_cast` (the single-rounding form —
    the doc's recorded double-round bug lives in the Simplify fold, not
    here). Disclosed slack: `MakeBlock`'s `Alloc_block_kind` is
    unconstrained (not stated by the rule) and the large-block
    `caml_alloc_shr` path is elided per the rule's own "same resulting
    layout" NOTES; `ProjectValueSlot` reads at an existential chunk
    (slots carry no kind; the `R.Obj.Closures` precedent);
    `array_indexing`'s generic emission uses the direct untag-then-scale
    form (the code strength-reduces; doc states only folds + contract);
    bigarray float16/complex and the product-vec128 exception are
    excluded via None-chunks rather than clauses; `StringLoad`'s 32-bit
    access folds into the signed-32 chunk. (The last four are flagged
    for fidelity review.) From the emission review (CF batch):
    `box_number_image`'s custom-ops words are `Cconst_natint` where the
    code emits `Cconst_symbol` — a deviation not forced by the grammar,
    value-equal under `symaddr_agree` (CF-3, with CF-4's companion note
    cohabiting at the same site); the write-barrier images
    carry the CF-1 3-argument `caml_modify_local` correction and CF-5's
    low_bits elision note at the same site. RULINGS LANDED (supersedes
    the former doc-lane hold): CF-6 was ruled WRITER-SIDE — the doc's
    flagless "plain Cstore(Word_int)" matches `setfield_computed`
    exactly (flambda2 routes static field indices through it, so the
    init-flag drop is unconditional); the `.v` over-refined with
    `cmm_init ia`, fixed to a constant `Assignment` on
    `block_set_image`'s immediate branch (:429, site note :413-418,
    Hopper-greened). CF-11, the vector-row companion caught by the
    doc-quoted-shapes sweep after the code-vs-.v pass under-checked the
    row: `array_load_vector` takes no mutability parameter (the
    unaligned helpers are unconditionally Mutable) and the doc quotes
    the helper without routing μ — writer-side (.v-vs-both, the CF-6
    pattern), fixed to constant `CMut_mutable` on
    `TC_Prim_ArrayAccess_Vector_Load` (:966, note :951-958). CF-2 was
    ruled a DOC divergence, escalated and resolved as KF-045: the
    ch. 18 rule's emission line now reads `Mutable` with a NOTES
    disclosure (array_load drops the primitive's mutability annotation
    at dispatch; the CR asks for a block_load-style refactor — flip
    back if it lands) plus the μ-does-not-reach-the-load rider on
    .Vector; the `.v` scalar load row follows the corrected doc
    (constant `CMut_mutable`, closing Church's last open CF item).
    Same stakes for all three: an Immutable hint would license backend
    CSE the code does not perform.

62. **Delayed bindings placed at bind-point** (`ToCmmData.v` inc 3): the
    doc binds Drop/Regular lets in D and materializes at flush time, but
    the frozen `tc_flush` hook is a pure side condition and cannot emit —
    so the concrete closure places each `Clet` at the BINDING point, and
    "the gap between bind-point and flush-point placement is exactly the
    reordering `TC.Let.Subst` governs." Flush timing is to_cmm algorithm
    state (the dacc/lifted-constants class). UNDER-APPROXIMATION (stated
    obligation): real to_cmm outputs sink bindings, so they are related
    through the `TC.Let.Subst`-closure of the relation; ch. 20's
    simulation is posed against that closure. The concrete `tc_flush` is
    "stage stacks empty" (trivial); the stacks remain as `TC.Let.Subst`'s
    premise vocabulary. `TC.Let.Subst` itself is stated as a KERNEL
    (silent, state-restoring defining evaluations reproduce at later
    points with the same ce/M): the coeffect/barrier quadrant becomes the
    M-unchanged hypotheses; hoist-direction validity clauses are not
    represented (the bind-point closure never hoists); the recursive-
    continuation restriction lives in ch. 16. `TC.Prim.Sound` states only
    the PR_ok implication (undef imposes no obligation), with existential
    trace and pointwise L-extension. AMENDED (fidelity review): the Subst
    kernel's conclusion pins M/TT′/RR′ (only the environments are
    existential), mirroring its hypothesis — trap pushes and region opens
    are SILENT, so stack restoration is semantic content the conclusion
    must transfer, not an automatic property of silent runs; and the
    ch. 20 Subst-closure definition must carry (or disclose the absence
    of) the validity side conditions, drop-of-unused-pure, and flush
    placement. RE-AMENDED (KF-038, severe class — the kernel as
    previously cataloged was FALSE, not conjectured: a bare `Cexit` in
    the defining expression resolves through the config's χ, and a free
    χ′ may rebind or drop the label): the sanctioned statement carries an
    exit-closedness premise on the defining expression (trivially true of
    every LPE defining expression). Kernel-shaped `Admitted` statements
    are strength-checked against the two counterexample recipes
    (binder-extension leak; free-χ bare Cexit) before landing; the shared
    `cmm_mentions` predicate is hosted in `Cmm.v` (the syntax-owner
    precedent), together with its label sibling `cmm_mentions_label`
    (exit-label/trap-action helpers; trap actions count as mentions — a
    safe-direction over-approximation). KF-038's fix is the uniform
    χ-agreement premise over labels the defining expression mentions
    (vacuously discharged by every LPE defining expression). FURTHER
    (KF-043, the fourth false-Admitted — the verifier's own public
    erratum on the previously-certified TT/RR channels: Pop trap actions
    READ the stack top and region-end matches into RR, so the
    generalized stacks were touchable): the kernel gains the syntactic
    guard `cmm_touches_stacks e_dfn = false` (hosted in `Cmm.v`'s mention
    family; vacuous on LPE expressions). FURTHER-STILL (KF-044, the
    guard's completion — the reviewer's second erratum, retiring his own
    "internally-caught raises remain allowed" justification, struck from
    the code comments): the one-walk guard flags Pop trap actions,
    `Cendregion`, and ALSO `Craise` and `Capply` operator heads —
    `CM.Catch.Exn` installs handlers WITHOUT pushing TT, so a direct
    `Craise` reads the ambient trap top (and χ at a label the expression
    never mentions), and `Capply` tunnels both TT and RR reads through
    callee bodies in `cp_funs`, invisible to any syntactic scan of
    `e_dfn`. Push-only trap actions and `Cbeginregion` remain allowed
    (neither can restore a pinned stack). `Cextcall` is exempt because
    `CM_Extcall` emits a `CME_extern` event unconditionally, so the
    kernel's trace-silence hypothesis already excludes extern steps.
    Still LPE-vacuous. EXEMPTION RECORD (KF-048): allocations pass the
    guard even though `CM_Alloc_Local` silently READS RR (matches
    `iota :: RR0`) — the completeness sweep's original "ch. 19 heads
    touch only M" rationale was wrong; the statement stays safe via
    M-pinning + strict allocation monotonicity (`mem_alloc` only extends
    M; the sole M-shrinker `mem_reclaim` sits behind the flagged
    `Cendregion`; `CM.Alloc.GC` contributes no provable steps while
    `gc_reloc` is an unconstrained Parameter). TRIPWIRE (KF-048/W-31,
    binding on future GC work): any axiom cashing `gc_reloc`'s
    condition (iv) as provable step existence — or any concrete GC
    instantiation — re-opens the channel via a GC-assisted
    address-recycling replica witness and flips `TC.Let.Subst`'s
    Admitted statement false; the oracle may only be constrained
    alongside a guard revisit. Output-term condition (W-28):
    `Parameter cmm_unique_binders : cmm_expr -> Prop` (`Cmm.v`; the
    Barendregt condition — binder variables and handler labels pairwise
    distinct, in-scope only), premised on `Simulates`' translated body —
    the cross-pollution channel is a property of the OUTPUT term while
    the colliding binder choices live existentially inside the quantified
    derivation, so without it a hostile derivation refutes the Admitted
    statement; intended instantiation is the code's
    create_local/next_raise_count freshness, and the one premise also
    discharges `ToCmmControl.v`'s deferred global-freshness pointers.
    Sink moves preserve the binder multiset, so the premise on the SUNK
    output covers the pre-sink image too.

63. **ch. 18 closure specifics** (`ToCmmData.v` inc 3b): `tc_simples`
    admits any skipped-argument subset (which positions are skipped is an
    elided analysis input; arity re-fixed by ch. 16's parameter pairing
    at the CALL sites — at the two raise-operand sites, where no pairing
    exists, the relation pins length directly: fidelity finding KF-042).
    `tc_bind_var_to_cmm` is substitution-style (the hook has no emission
    slot); effect-ordering soundness defers to `TC.Let.Subst`.
    `TC.Let.SetOfClosures` reuses `R_Obj_Closures`' vocabulary
    byte-for-byte with the same existentials; arity via the quantified
    `lookup_code` hook. `TC.Let.Static`: RESOLVED at grade (i) (fidelity
    findings KF-035/KF-036; the original "under-determined by design"
    sanction was WRONG-SIGNED — an over-approximation of the translation
    relation, the unsafe direction). The update list is pinned by a
    structural walk: `sc_holes` (total over the 26-constructor
    static_const grammar) yields (byte offset, variable) per hole;
    `static_group_holes` lifts to groups in binder-then-field order; the
    rule requires `Forall2 (static_update th)` against exactly those
    holes. The walk EXPOSED A LATENT EXACTNESS BUG the old shape could
    not express: packed int8/16/32/float32 arrays update at ELEMENT byte
    strides, so `strided_field_address_image` (byte-offset Cadda,
    mirroring `cmm_helpers.strided_field_address`) replaces the
    word-offset form in both update arms. `static_update` is the KF-036
    disjunction (caml_initialize extcall image for pointer holes; bare
    Cstore Initialization otherwise). SOC value slots via a relational
    `soc_holes` (the designated closure symbol is existential — every
    choice addresses the same word). Disclosed residue: SOC emission
    order, and the code's add_symbol_init hoist of Cvar-valued updates
    (the doc's sequenced-before-body placement is modeled). The layout
    obligation remains `R.Obj.*` on the initial memory.

64. **The ch. 20 simulation apparatus** (`ToCmmSoundness.v`):
    `callee_name : value -> string -> Prop` is a quantified Section hook
    (the value-to-linkage-name correspondence is calling-convention
    material axiomatized at `CM.Extcall`; the ch. 16 six-hook precedent).
    The `TC.Let.Subst`-closure is SINK-ONLY: inline and drop moves are
    already inside the bare translation (the LPE rows), so the only gap
    to real outputs is Regular bindings at bind- vs flush-point; the
    closure is the congruence closure (at Flambda-image positions) of
    `Clet x d (SL[b]) ~> SL[Clet x d b]` with SL a straight-line context
    (a sink never crosses a branch or loop header — the doc's
    `FM_branching_point`/`FM_entering_loop` made structural), guarded by
    (i) syntactic scoping (`cmm_mentions`) and (ii) a SEMANTIC commuting
    license (kernel-shaped, via `cmem_run`): the doc's PROVIDED quadruple
    is stated on Flambda effects that Cmm terms no longer carry, so the
    license is the quadruple's content rather than its vocabulary.
    Rejected alternative: unguarded syntactic sinks — that closure
    contains behavior-changing reorderings, so `Simulates` posed against
    it would be trivially FALSE rather than conjectured. Disclosed
    inherited narrowing: the Cmm machine models only RETURNING externals,
    so traces with raising externals are outside the correspondence
    (ch. 15's restriction). Event/trace correspondence mirrors the
    KF-015 cumulative-retention discipline heterogeneously, with a Qed
    bridge to `R.Observe`'s `Forall2` form.

65. **The sunk closure, implemented** (`ToCmmSoundness.v` inc 2; entry 64
    realized, W-26 discharged at the definition site): `cmm_mentions`
    (binders included — a disclosed over-approximation of free
    occurrence), `sl_ctx` straight-line contexts, `sink_license`,
    `sink_step`, the congruence closure `sunk`, and `tc_expr_data_sunk`
    (the judgment `Simulates` is posed against). CORRECTION (KF-041,
    severe class): the original "W-26 discharged at the definition site"
    claim holds for parts (1)/(2) only — check (c) found `Sink_intro`'s
    guards left the binder-extension leak open (licensed d sunk past a
    prefix rebinding a variable it reads changes results; the label/χ
    channel had no guard); the fix is a symmetric mention-disjointness
    premise (binders included, closing all variable quadrants) plus the
    `cmm_mentions_label` guard. Design point:
    `sink_license` is stated POSITIVELY (totality — from every state the
    defining expression runs silently with M/TT/RR unchanged — plus
    value agreement across mention-preserving states); a conditional
    "if it runs purely here then also there" form would be vacuously
    true for effectful expressions and reopen exactly the KF-035
    over-approximation trap. The unguarded-sinks rejection rationale is
    at the site verbatim: guards are fidelity, not caution.

66. **Value payloads are unbounded Z** (cross-cutting; disclosure sites
    at `PrimMemoryB.v`'s GetHeader/ReadOffset): the value grammar carries
    raw `Z` payloads with no width predicate, so nondeterministic results
    (conjectured clauses only — GetHeader's header word, ReadOffset's
    payload, the vector pre-image up to width) may be out-of-width; all
    normative arithmetic normalizes through `wrap`. Width discipline
    (a global `value_wf` threaded through nondeterministic
    constructions) is recorded as PROOF-PHASE DEBT — at statement level
    it buys no fidelity, and the affected clauses impose no obligations.

67. **The exception wrapper, encoded** (`ToCmmControl.v`,
    `TC.Apply.ExnWrapper` — doc finding #17): `tc_exn_wrapper` with
    `TCWrap_None`/`TCWrap_Extras` transcribing the corrected rule
    (Exn_handler outermost; debug-gated reraise via the `debug_flag`
    hook; Push/Pop on the Cexits), threaded so Return placement consumes
    the wrapped call — the doc conclusion verbatim. Disclosures: (a)
    `x_res`'s return-machtype annotation drops (handler params carry no
    machtypes — a `CM.Syntax.Fragment` fact); (b) freshness is LOCAL per
    file convention (labels via `phi_label_fresh` + pairwise
    distinctness; the one local capture hazard — `x_exn` occurring in
    the translated extras — via a `cmm_mentions` guard, deliberate
    KF-035-class hygiene: unguarded, the relation would admit capture
    instances no real output has); global freshness is ch. 20's
    discharge, as for the KF-037 premises.

68. **`INV.ToCmm.Simulates`, stated** (`ToCmmSoundness.v` inc 3): posed
    against `tc_expr_data_sunk`; premises include two SANCTIONED
    boundary-wiring conditions (the return/exn continuations realized as
    `R_return`/`R_exn` — the ch. 20 sibling of entry 55's discovered-
    necessary hypotheses; unwired, a degenerate realization refutes the
    conjecture; transcribes "K = χ via Φ" at the two continuations
    `initial(U)` knows). Disclosures: the trap-depth gloss "T = TT same
    depth" holds only above the halt boundary (Flambda's halt exn entry
    is on T; the Cmm uncaught config requires an empty stack);
    `fl_unit_behavior`'s existential is unfolded so the initial region
    stack seeds RR (R = RR, the Control convention); `rep_stream` is the
    coinductive trace image for the react pair (entry 11's union);
    resource exhaustion relates with NO trace obligation (the doc's
    sentence, exactly); the §5.6 known violation (classic-mode PhysEqual)
    is quoted in the header — the statement transcribes the rule as
    written, violation and all, and is Admitted, never used as an axiom.
    Returning-externals-only narrowing is in the statement's header.
    KF-040 CARVE-OUT premise: no static const of U is a custom-boxed
    number with an `Or_variable` payload (`custom_boxed_var_payload` over
    the sanctioned `static_consts_in`) — excludes the compiler-bug
    territory so the conjecture isn't false against `R.Obj`; flips to
    nothing if the compiler is fixed upstream.

69. **KF-040: the static custom-boxed update bug** (`ToCmmData.v`
    `ov_hole`; COMPILER-BUG CANDIDATE): to_cmm's deferred updates for
    statically-allocated boxed int32/int64/nativeint/float32 with
    `Or_variable.Var` payloads target word 0 — the custom-OPS pointer —
    while the payload lives at +8 (as `R.Obj.Boxed` itself documents);
    latent (no production Var-payload producer for those kinds),
    fexpr-reachable, armed for future lifting extensions. RULING: the
    model is CODE-FAITHFUL — `ov_hole` keeps the code's index-0 target
    with the divergence disclosed at the site (the old "payload hole:
    word 0" assertion was wrong for the custom four) — and ch. 20's
    `Simulates` carries the entry-68 carve-out premise so the conjecture
    is not false against `R.Obj`. If the compiler is fixed upstream,
    `ov_hole` flips as a strictly local change and the carve-out premise
    is deleted. Escalated to the maintainers with a suggested fexpr
    confirmation test. ITEM-8 UPDATE: the entry-68 carve-out premise and
    this ruling are unaffected by the item-8 resolution (KF-040 is a
    write-target bug, not an identity question).

70. **Ch. 20 completion** (`ToCmmSoundness.v` inc 4 — the census-closing
    increment: `INV.ToCmm.EndToEnd`, `INV.ToCmm.InvalidUnreached`, nine
    discharging invariants; 12/12 ch. 20 ids). `EndToEnd`: the doc
    "INHERITS INV.Simplify.Preserves as a premise", but theorem statements
    cannot be hypotheses — the statement inherits BOTH components' premise
    SETS verbatim (Preserves' expr_wf/code-tying/consistent/code-heap tie
    on U0; the full `Simulates` block on U′), and the conclusion is the
    transitive chain b0 –beh_sim– b′ –tocmm_beh_rel– o with the
    intermediate behavior b′ exposed existentially (the two transitivity
    legs need it; judged faithful — the doc's modulo-§5.1 caveat is
    inherited through the ch. 13 machinery). KNOWN-COUNTEREXAMPLE
    transcription: the doc itself records the rule as FALSE for units
    with a compile-time int→float32 conversion; the "modulo" words are a
    reading instruction, not a formalizable carve-out; transcribed as
    written, `Admitted`, never used as an axiom (the `Simulates`
    §5.6 precedent — the poison class is doc-believes-TRUE statements,
    not doc-acknowledged counterexamples with a quoted reading
    instruction and grep-confirmed leaf status). This import also pulls
    the ch. 13 chain (WellFormed/TypeGrammar/Concretization/Simplify/
    Soundness) into `ToCmmSoundness.v`. `InvalidUnreached`: a REAL
    conjecture, conclusion ¬ cmem_unit_behaves … CMO_undef, with two
    ENCODING NOTEs: (i) mild disclosed STRENGTHENING — the Cmm machine
    does not separate reaching-Cinvalid from other stuckness
    (`CM.Unit.Final`'s undef is the union), so the statement asserts the
    union ("never CMO_undef"; honest direction, larger refutation
    surface); (ii) the doc's slot-liveness premise has NO model
    counterpart — of the four Cinvalid site-classes only (a)
    `TC_Invalid` and (b) `TC_Switch`'s synthesized cases exist in the
    mechanization; (c) the nullary Invalid prim and (d) Dead arms are
    unmodeled (zero Dead hits in the ch. 18 images), so the premise
    excluding (d) is not needed and its absence is disclosed at the
    site. The nine discharging invariants follow entry 37's decision
    rule: eight documented anchors (now listed in the Traceability
    EXCEPTION instances) and `AddrConfined` as HYBRID with envelope-Qed
    (`machtype_of_kind_data_no_addr` — a real exclusion, the grammar HAS
    `MC_addr`; support-theorem inventory member). Comment-round rider
    (KF-047, resolved): the SlotLiveness anchor carries the doc NOTES'
    `at_normal_mode` qualifier on leg (a), and the EffectLinear anchor
    carries the `apply_expr` Case-3 edge (the call itself enters the
    delayed set; sound because Arbitrary_effects is never dropped).
    Final-report flags preserved verbatim in the anchors:
    CallConvCoherent leg (2) UNDISCHARGED (a null-code slot applied is a
    jump to address 0); SymbolInitPlacement's recursive-handler
    init-drop soft spot; StaticUpdateBarrier's barrier-placement blind
    spot in the tocmm-* validation.

71. **The ch. 07 environment repairs** (KF-049/KF-050, the
    refutable-Admitted family in the types domain; both fixed forward,
    statements untouched, both theorems now `Qed`). KF-049
    (`TypeGrammar.v`): `tenv_wf` under-constrained `te_canonical` — a
    canonical 2-cycle and a constant canonicalized to a different
    constant were both wf, refuting `T.Env.AliasesAuthoritative` and
    `T.Env.ConstCanonicalPersists`; fix adds the two representation
    invariants entry 14 already attributed to `te_canonical` (canonical
    idempotence = path compression; constants self-canonical), plus the
    NoEqualsOnCanonical dual-sentence disclosure rider. `TypeGrammar.v`
    is Admitted-free. KF-050 (`Concretization.v`): `G_equals` was γ's
    single kind-crossing channel (it admitted ρ(s) at ANY kind's Equals
    type — a K_value constant could inhabit γ at a K_naked_float type,
    refuting `T.Gamma.Kind`; consequential because ch. 08's entire rule
    set quantifies over this γ); fix is one premise,
    `value_has_kind v (kind_of_ftype T)` — the code's by-construction
    alias kind-homogeneity. `Concretization.v` is Admitted-free. Both
    theorems join the Qed-under-conjectured support inventory. Riders:
    the stale cross-file `meet_code_id` pointers in both files were
    corrected (comment-only; `MeetJoin.v` models no `meet_code_id`).

72. **The alias-channel re-sweep repairs** (KF-051/KF-052, found by the
    committed post-KF-050 refutability re-sweep of `MeetJoin.v`'s nine
    γ-quantified Admitted statements; six were clean, three refutable —
    the third is KF-053, entry 73). KF-051 (`T.Meet.Sound`): `sat_ext`
    (and `consistent`'s stored-equation clause) demanded a LITERAL ρ
    binding for every recorded-equation name, but γ reads symbols
    through `simple_eval`'s static fallback — an alias-meet extension on
    an ρ-unbound symbol was unsatisfiable under a consistent ρ, refuting
    the sat_ext conjunct under the empty environment (and falsifying the
    file's own "sound direction" disclosure on the self-alias case).
    Fix: `sat_ext`/`consistent` read names via `simple_eval`, the
    machine's own name denotation. Statement unchanged, stays
    `Admitted`, stops being refutable. This is the mechanization-side
    face of the KF-054 symbol-binding seam (entry 74) — the repairs cite
    the ch. 07 §4 preamble guarantee landed there. KF-052
    (`T.Expand.Head`; the reviewer's erratum #4 — the row-48 green
    review had this rule in his slice and passed it as faithful): the
    doc's γ-preservation equality lives on the
    SET form (γ_set); the `.v` had transcribed a per-ρ iff whose
    backward half is generically false (expansion discards the alias's
    singleton constraint), and whose forward half retained one refutable
    corner (an entry-less symbol canonical at a non-Value kind expands
    to any_value while ρ could bind the symbol to a non-Value-kinded
    value). Fix: restated as the per-ρ FORWARD implication with the
    quantifier split documented (set-form equality deferred), and
    `consistent` gains its fifth clause — ρ's symbol bindings are
    K_value — which is the direct transcription of the KF-054 preamble
    sentence (entry 74). The repaired statement consumes the KF-050
    kind premise and is blessed for a polish-wave `Qed` attempt.

73. **The GLB demotion** (KF-053; the ruling instantiating the
    REFUTED-CONJECTURE variant, Traceability above).
    `T.Meet.GreatestLowerBound` sat `Admitted` while its own comment
    disclosed the refutation — `T.Meet.MutableBlockMissedBottom`,
    `Qed`'d later in the same file, IS the counterexample (its result γ is
    disjoint from the left input's), so the Admitted let anyone derive
    `False`: the KF-037 poison class in purest form, on disk since
    wave 4, in nobody's frame as a finding until the re-sweep. RULING
    (option (a) of the filed pair): demote to
    `Definition T_Meet_GreatestLowerBound_claim : Prop := …`, stated and
    NOT asserted; RULE comment keeps STATUS conjectured, the demotion
    rationale, and the witness's name; id remains greppable; MeetJoin's
    governed Admitted count 12→11. Option (b) — asserting the negation
    as a `Qed` — remains the sanctioned upgrade path if anyone wants to
    build the refutation term through the meet layer.

74. **The symbol-binding seam** (KF-054, doc lane — the root cause
    behind three independently-found symptoms: KF-051's sat_ext
    refutation channel, the H_mod heap-simulation both-sides-populated
    discovery, and the auditor's rho-first-with-fallback candidate).
    The formalism never said where symbol bindings are guaranteed to
    live, while the model makes symbols statically evaluable
    (`simple_eval`: ρ first, `V_ptr (Addr_sym s)` fallback — empty ρ/H
    suffices); every file touching symbols re-decided the seam.
    RESOLVED: ch. 07 §4's preamble now states the guarantee — symbols
    need no ρ entry (always evaluable: through ρ when ρ binds it, as
    `OS.Let.Static` arranges for static sets of closures, and to the
    static pointer otherwise — Value-kinded either way; §04
    `OS.Simple.Eval`); obligations built on §4 read names
    through ⟦·⟧ρ, never by demanding a literal ρ binding; ρ's symbol
    bindings, when present, are Value-kinded per symbols-denote-statics.
    `consistent`'s clause 5 (entry 72) is the sentence's direct
    transcription; the KF-051 repair is its enforcement in `sat_ext`.
    Does not supersede either .v fix — it gives them a stated premise
    to cite.

75. **The item-8 resolution** (user decision, 2026-07-22; doc revisions
    across chs. 06/10/11/13/20 + the classic validation study (plus the
    =-premises reading note covering ch. 04, hosted at ch. 06's ⟦p⟧
    preamble) —
    all Dijkstra-stamped, Knuth design-PASS with KF-055/056/057 filed
    and closed/riding; the .v wave follows this record). DECISION:
    13 §4 item 8 resolved as option (a) SCOPED TO IMMUTABLE HEAP OBJECTS
    plus option (b), jointly with the classic-mode sibling. Doc content:
    `P.Binary.PhysEqual` is RELATIONAL on ι-operands (ι = immutable
    heap objects EXCLUDING Immutable_unique blocks/arrays — extension
    constructors are identity-pinned; closures included, ruling: the
    pipeline shares/duplicates/lifts closures and the manual's ==⇒=
    guarantee is vacuous on functionals) — result 0 always derivable
    (even for identical pointers: duplication/dropping), result 1
    derivable exactly up to 13 §1's folding (preserving ==⇒structural
    equality); the ∋ relational-denotation notation is declared at the
    ⟦p⟧ judgment preamble with the membership reading of
    `OS.Let.Prim.*`'s =-premises (KF-055). Ch. 13 §1 compares
    observations up to FOLDING of ι-objects and defines REFINEMENT;
    `INV.Simplify.Preserves` and `INV.Rewrite.Local` conclude
    refinement, not equivalence — forced, not chosen: the
    `simplify_phys_equal` folds and CSE-of-phys_equal PRUNE outcomes,
    so no two-sided statement survives a loose denotation.
    `INV.ToCmm.Simulates` is RE-POSED in the backward direction
    (∀ Cmm outcome ∃ Flambda behavior; KF-056 — the forward direction
    is refutable: an abstract run may resolve
    `phys_equal(ptr ℓ, ptr ℓ) → 0` where emitted word-equality code
    cannot follow), with the determinacy-modulo-oracles bridge in its
    NOTES; `INV.ToCmm.EndToEnd`'s conclusion is unchanged (already
    refinement-shaped; its transitivity gloss re-worded
    equivalence→refinement). Consumers swept (Knuth): `R.Observe` needs NO
    change — the folding localizes to the flambda-side relation.
    MECHANIZATION PLAN (Hopper's prep + rulings (a)-(f), W-33 inventory):
    `is_iota` classifier + the fold core in `Values.v` (entry-44
    amendment has the discipline; is_iota consults μ at the three-way
    granularity — Base.v carries it verbatim, no gap); PhysEqual
    4→8 constructors in `PrimMemoryB.v` (deterministic non-ι clauses on
    `phys_same_word`; ι-clauses `∋ 0` always and `∋ 1` under the
    diagonal fold premise — deliberate overlap on equal pointers, so
    the fold core must be REFLEXIVE on ι-objects); `obs_refines` +
    revised `b`-discipline in `Soundness.v`; both headline statements
    re-shaped; `ToCmmSoundness.v`'s Simulates re-posed (Flambda-side
    quotient, `tocmm_beh_rel` untouched) with its header caveat
    rewritten; `RewritesPrim.v`'s CSE.Replace comment re-quoted and the
    FINDING-16 pointer updated (the in-model refutation composition is
    now a licensed resolution); `Soundness.v`'s KF-030 disclosure
    comment (:569-588) re-worded to mirror entry 55's rewrite (it still
    asserts the local form "is FALSE" with the old witness — a .v
    comment the docs no longer support); `Pilot.v`'s const-fold proof re-greens
    against the renamed relations (equivalence implies refinement), with
    an optional second conclusion at equivalence strength witnessing the
    identity-free-coincidence sentence. CENSUS: the resolution is
    census-neutral (453 = 303/66/84; no id or STATUS moved; verified
    thrice). COUNTING TRAP (recurring, now catalogued): line-anchored
    STATUS extraction over-counts ch. 18 by one via the prose line
    beginning "STATUS conjectured —" in TC.Let.Subst's NOTES
    (18-to-cmm-data.md:143); use fence-aware extraction. WATCHES:
    W-33 (superseded-.v inventory, closes as the wave lands), W-34
    (cextern folding-monotonicity, entry 20), W-35 (cyclic-ι tripwire,
    entry 44). Finding records: KF-055/056 closed on the doc letter;
    KF-057 codified at entry 20; KF-030's disclosure rewritten at
    entry 55 (partial re-entry).

76. **The solidity schema** (migration spec; user-directed 2026-07-22;
    Knuth's DESIGN RULING row and the auditor's invariant blessing,
    codified — this record gates the migration sweep; the Traceability
    and README rewrites are the sweep's batch 0).
    CLAIM replaces STATUS as the claim-kind: {normative, descriptive,
    interpretive}, classified by the rule's CONSTRAINED SUBJECT —
    normative when the subject is something the code computes or the
    runtime does, regardless of checkability or vocabulary; interpretive
    when the subject is formalism-only apparatus (the γ clauses and
    γ-properties, R.Heap, environment-extension semantics), where
    falsity is remediable only by a doc/model edit; descriptive for
    current-behavior documentation INCLUDING imprecision witnesses that
    legitimately retire when the code improves. Classifier tiebreak:
    "if this were false and that were unacceptable, what would you
    change?" The old `conjectured` DISSOLVES as an evidence level
    masquerading as a kind: each of the frozen 84 conjectured ids maps
    to exactly one of the three, with an id-keyed rationale row; the
    classification output is the migration's audit artifact, audited
    set-equal in both directions.
    EVIDENCE is DERIVED, never declared, on TWO INCOMPARABLE AXES:
    empirical (stated < code-read < validated) and formal
    (unmechanized < mechanized; proved-in-model is a badge atop
    mechanized, never more). "Mechanized" REQUIRES the fidelity-review
    stamp (unreviewed Admitted certifies falsities — the Admitted-false
    campaign is the existence proof) and EXCLUDES documented anchors
    (else the rung is uniformly true and stops discriminating). Fence
    event lines feeding derivation: `CHECKED @ <commit>` (code-reading
    verification), `VERIFIED <study> @ <commit>`, and
    `CAVEAT <kind>: <text>` with kinds {known-false, compiler-bug,
    pending-upstream, watch(W-nn), disclosure} — compiler-bug is the
    KF-040 polarity (rule TRUE of the code, code diverges from intent;
    the inverse sign of known-false). KIND PRINCIPLE (ruled at the
    kind's first live instance, the float32 double-round family): a
    caveat's kind is judged against the claim of the RULE IT SITS ON,
    not the story it participates in — the true-of-mainline ch. 05
    denotation carries an informational compiler-bug family caveat
    (fix branch named in text only), while the divergent ch. 10 fold
    rule carries the compiler-bug + pending-upstream pair with the
    machine-watched key. MULTI-SITE LIFT (for whoever fires it): the
    `fix-float32-double-rounding` key is lift-watched at the ch. 10
    fold rule ONLY; its landing retires FOUR sites in one change —
    the ch. 10 pair, ch. 05's NumConv family caveat, 13 §4.7's OPEN
    marker, and ch. 18's TC.Prim.NumConv disclosures. A pending-upstream
    caveat REQUIRES its branch/commit key — keyless is a syntax error;
    the kind means depends-on-an-unlanded-KEYED-branch, and a keyless
    instance is invisible to the reverse-proviso checker. The key must
    name an EXISTING branch or commit: aspirational deferrals (planned
    or hoped-for upstream work with no watchable key and no falsifiable
    lift condition) are not provisos — they belong to the 01 scope
    ledger.
    GRADE (derived by checked-in tooling only; hand-computed grades are
    forbidden): A = validated × mechanized, not false-as-stated and not
    pending-upstream (those two flags demote — they undermine current
    truth; compiler-bug DISPLAYS WITHOUT DEMOTING: the rule is true of
    the code, the flag marks the code's divergence from intent);
    B = validated
    alone, OR code-read × mechanized; C = exactly one axis inhabited;
    D = neither. DISPUTED — derived as "an open FIDELITY finding whose
    RULES: header names this id" — SUSPENDS the letter entirely.
    Display flags: false-as-stated, compiler-bug, pending-upstream;
    HYBRID rules carry clause-granular evidence and grade at the
    weakest clause with an explicit (hybrid) flag; the tooling refuses
    rule-level VERIFIED on HYBRID-listed ids. A Qed is never a grade
    input; an envelope-Qed's code evidence is the CHECKED event that
    verified the envelope. Staleness: evidence is marked stale when any
    CODE-anchored file changed since the evidence commit — and STALE
    EVIDENCE DEMOTES: it does not count toward the grade (display
    persists, stale-marked). Commit keys are 7-40 lowercase hex.
    "Mechanized"'s fidelity-stamp provenance is FIDELITY.md's
    machine-readable '## Stamps' table (file-granular STAMP: rows,
    author-maintained, each backed by a review-log row; a rule's stamp
    resolves through the classifier's artifact-location join).
    END-STATE STRICTNESS: when the migration's STATUS count reaches
    zero, STATUS rows become hard phantom-class errors in the join
    (keyed to the completion event). LEGACY FLAG-DAY:
    legacy_verified_ok flips to False within the VERIFIED-backfill
    batch itself, whose own green run passes strict.
    CENSUS INVARIANT (the auditor's, binding): end-state = fence-aware
    byte-identity of id|CLAIM pairs doc↔.v, 453 conserved, stated as
    SET LAWS (AMENDED to the three-target ruling): normative =
    old-normative ⊎ A, descriptive = old-descriptive ⊎ C, interpretive
    = B, with A ⊎ B ⊎ C = the frozen conjectured 84 — end split
    (303+a)/(66+c)/b, a+b+c = 84. Every conjectured→descriptive id (the
    C set) is tagged in the classification output with its
    artifact-conversion consequence (Theorem+Admitted → documented
    anchor is a statement-level .v delta with its own compile round),
    and batches carrying C-set ids call them out for the
    untouched-subset checks. Mid-migration the extraction emits TRIPLES
    id|keyword|value with keyword ∈ {STATUS, CLAIM}; byte-identity of
    triples doc↔.v holds at EVERY step (forcing each rule's flip to
    land doc-side and .v-side in one flagged delta); STATUS ⊎ CLAIM =
    453 always, disjoint, STATUS strictly shrinking; per-batch: mapping
    legality (normative→normative, descriptive→descriptive,
    conjectured→{normative, interpretive, descriptive} + rationale
    row), conservation and duplicate checks PRE-uniquing, untouched-id
    byte-invariance, and phantom scans for ALL keywords — the prose
    convention extends to CLAIM/CAVEAT/CHECKED/VERIFIED. Batch flags
    ENUMERATE their files (W-36).
    ARTIFACT-MAPPING RE-KEY: defining clause (normative OR
    interpretive) → constructor/equation; property (normative or
    interpretive) → Theorem, Admitted or Qed; descriptive → documented
    anchor; the five sanctioned variants unchanged in substance, the
    REFUTED-CONJECTURE variant's text re-keyed to the new vocabulary.
    STRAND INVENTORY the sweep must cover: nine doc-side prose
    "STATUS …" mentions (13:80, 13:607, 16:426, 17:456, 18:143 — the
    counting-trap line, RETIRED by rewording, not inherited — 19:155,
    20:75, 20:122, 20:160); the theories-side prose trio ("STATUS
    preserved" ×2, "STATUS marks" ×1); this catalog's own variant
    texts.
    REGEN TOOL obligations: exactly one keyword line per fence;
    duplicate ids pre-dedup; CODE/CAVEAT/CHECKED/VERIFIED syntax with
    commit-key well-formedness AND repo reachability (a proviso whose
    keyed branch has landed but whose text has not lifted is flagged —
    the reverse-proviso check, first test rows: the string-load
    provisos); EVIDENCE always recomputed, never trusted from disk; the
    artifact-kind classifier CONSUMES this catalog's Traceability
    instance lists as ground truth and errors on mismatch (parser and
    catalog mutually checking); the derived index join FAILS on any
    doc/.v disagreement, never picks a side; double-run determinism;
    every regeneration diff row traces to a stamped source delta.
    PHANTOM-SCAN SCOPE (ruled): the scan protects MACHINE-CONSUMED
    surfaces — chapter fences, theories/ comments, FIDELITY RULES:
    headers, and this catalog's Traceability section (parser ground
    truth); the catalog's numbered records carry an ERA CUTOFF —
    records ≤ 76 are historical documents in their era's vocabulary
    and are exempt (rewriting them falsifies the record), records
    > 76 are post-migration prose and are scanned. If the parser ever
    consumes record text beyond the Traceability section, the
    exemption is revisited in the same change.
    FIDELITY.md gains structured `RULES: <id>, …` headers on all 54
    finding entries (`RULES: none` for findings targeting no rule —
    doc-preamble and process-level findings), catalog-cross-derived
    plus machine-drafted, author-verified in full before feeding the
    index; DISPUTED derivation makes these headers load-bearing for
    grading, accepted knowingly.

## Build

```
make            # rocq makefile -f _CoqProject -o CoqMakefile && make -f CoqMakefile -j
```

Single file: `rocq compile -Q theories Flambda2 theories/Foo.v` (deps' `.vo`
must exist). The development is independent of the compiler's dune build; the
`(dirs :standard \ rocq)` stanza in `../dune` keeps dune from scanning it.
