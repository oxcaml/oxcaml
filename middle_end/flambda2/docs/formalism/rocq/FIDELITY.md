# FIDELITY.md — adversarial fidelity audit record

Auditor: Knuth (Team Popper). This file tracks every semantic-fidelity finding
from cross-reading green `.v` files against their chapters' rule blocks, and
each finding's resolution. A domain expert should be able to use this as the
audit trail for the mechanization.

Scope of review (per file, after Hopper reports it green):
- missing / reordered premises; dropped side conditions (freshness,
  effects/coeffects class, mutability, prover results);
- wrong quantification (metavariables shared vs. distinct);
- conclusion mismatches (doc `undef` vs. Rocq stuck/None, etc.);
- ENCODING NOTEs claiming more than the encoding delivers;
- STATUS-mapping violations per CORRESPONDENCE.md.

Naming/style is out of scope unless it changes meaning.

Finding states: **open** → **resolved** (writer fixed) / **rebutted** (writer
justified; justification recorded) / **escalated** (doc itself suspect; sent
to main as a FINDING).

Per user-directed process change, reviews also run at DRAFT stage (before a
file compiles); those are marked DRAFT REVIEW and re-checked at wave-gate
green, since compile-fix churn may moot or introduce drift.

## Review log

| File | Stage | Reviewed against | Verdict |
|---|---|---|---|
| Values.v | DRAFT REVIEW | 04-opsem.md §1, §4.1-4.3, §6.2, §8; README value grammar; ch. 06 heap objects (spot) | Faithful transcription overall; K′-knot encoding (CE_rec/bind_rec_group) structurally matches OS.LetCont.Rec pending Opsem.v's unfold rule (W-2). One finding: KF-001. |
| Opsem.v (observation layer only) | DRAFT REVIEW | 04-opsem.md §6.3, §8.2 | Behaviors/closures faithful; CompCert divergence split (catalog 11) delivers what it claims (union = doc divergence; `reacts` productive). One finding: KF-002. |
| PrimMemoryA.v (increment 1, 34 rules) | DRAFT REVIEW | 06-primitives-memory.md: mixed shape, effects axes, Make_block/array, static mixed, regions, Block_load/set, duplicates, ObjDup/Opaque/MakeLazy/IntAsPointer | Effects apparatus and allocation/load/duplicate families transcribe cleanly (premise order kept; nth_error/list_set encode bounds correctly; catalog 18 addresses change sound). Findings: KF-003 (medium), KF-004 (doc gap, escalated), KF-005 (low), KF-006 (low). IntAsPointer unconstrained-result encoding awaiting main's ratification (noted in file). |
| Cmm.v (all 20 ch. 15 rules) | DRAFT REVIEW | 15-cmm.md (whole chapter), cross-refs 04-opsem.md §4/§6/§8 for the boundary contrast | Strong transcription: all 20 RULE ids present; K′-knot Cmm instance (CHandler_rec/chi_lookup/chi_bind_rec) re-ties the CM.Catch.Rec fixed point correctly; trap actions, raise, catch depths, store totalization, Switch bounds, and the shift-undef choice (catalog 17) all match; CM.Context's proviso holds by construction (cm_head has no control transfers). Value-canonical-form discipline (to_unsigned64/to_signed64 at use sites) is consistent throughout. Findings: KF-007 (high, doc-rooted, escalated), KF-008 (low). Watch: W-8..W-10. |
| Base.v (GREEN; 2 rules per catalog 6) | GREEN REVIEW | 03-kinds.md §1-3 (grammars, WF.Subkind.Erasable, WF.Arity.Unarize); frozen-interface list; 02/06 spot-checks for the modes/mutability apparatus | CLEAN — no findings. Both RULE artifacts exact (erase_subkind's two equations incl. forcing Nullable on Value; unarize's four equations, catalog-4 flat_map encoding + all five derived ops). Grammars match constructor-for-constructor and in doc order: 11 nnk cases (incl. the recovered Naked_nativeint), 27 value_subkind cases, flat_suffix_element set (no tagged immediates), block shapes with field_kinds correctly *derived* from (p, ē) per §2 prose rather than stored. wrap = canonical signed range at given width; nnk_int_width has the §1 asymmetry right (immediate 63, nativeint 64). Pilot lemmas Qed as required. Cross-file note folded into W-8. |
| Cmm.v | GREEN RE-CHECK | (delta over the draft review row above) | Draft-review verdict carries over. W-8 CLOSED: the transitional local float/vec Parameter block is deleted; Base.v's shared names are in use (grep clean). KF-007's CM.Apply/CM.Apply.Raise/CM.Unit.Final rework and KF-008's trace-carrying divergence are deferred-by-design to Milner's post-green increment (main's ruling); the green file transcribes the pre-fix doc. |
| CmmMemory.v (all 7 ch. 19 rules) | GREEN REVIEW | 19-cmm-memory-gc.md (whole chapter); backend/cmm_helpers.ml (barrier externs, array_indexing, atomics — the mandatory W-11 code check); 15-cmm.md fragment scope | Strong on the allocation/region core: CM.Alloc.Heap/Local, CM.Region.Begin/End, CM.Alloc.Exhaustion transcribe cleanly (site-documented strengthenings: freshness range includes the header word; canonical-address side condition; natural-size vector fields). CM.Region.Begin's freshness (not open AND not tagging any live byte) is *stronger* than the Flambda side's `~ In iota R` — the right direction. mem_reclaim matches "ι and every region above it", bytes and tags. The mandatory Cadda-through-Clet sub-check PASSES (let-bound Cadda rejected via the bare-Cadda case). Exhaustion's Capply inclusion + trace-loss caveat is candid and site-noted. Findings: KF-009 (low, dropped caml_local premise), KF-010 (medium, doc-rooted, escalated — GC pins e, pending Val words never relocated), KF-011 (low, whitelist names a non-existent extern), KF-012 (medium, expr_addr_ok rejects array_indexing's nested-Cadda output). W-11 discharged (results in KF-011/KF-012/W-17); new W-16, W-17. |
| Cmm.v §8/§10 rework (CM.Apply, CM.Apply.Raise, CM.Unit.Final, cm_outcome) | GREEN RE-CHECK #2 (the KF-007/KF-008 increment) | 15-cmm.md:516-573, 619-644 (amended); 19-cmm-memory-gc.md interplay (alloc_point, CM.Alloc.Local) | The rework transcribes the amended doc faithfully. CM_Apply/CM_Apply_Raise premises in doc order (symaddr/funs agreement, arity, nested cm_returns/cm_escapes with fresh ce/χ and TT=[]); the bucket-only re-raise matches the doc conclusion literally. Hereditary-divergence device (cm_call_frame/cm_reach/CM_Diverges_Call) checked: no spurious derivations through returning callees (return configs are stepless: Cexit Return_lbl matches no cm_step, no call frame), and descent-committed derivations track real execution paths. cm_reacts' nonempty-burst guard verified load-bearing AND sufficient (silent infinite descent excluded; every real infinite-trace execution decomposes into bursts; cofix productive via stream_prepend on tr ≠ []). KF-008 mirror delivered as claimed (CMO_diverges tr / CMO_reacts s, catalog 11). New findings from the recheck: KF-013 (GC identity self-stutter pollutes divergence — doc-rooted, escalated), KF-014 (nested-run RR=[] strands exclave/local-returning calls — escalated). New W-18 (extras-expecting caller handler vs bucket-only re-raise). KF-007 residual (a) now concrete: CM_Returns (Cmm.v:1240-1246) discards the nested run's TT′ — no balance clause, matching the doc's silence. |
| Syntax.v (all 13 WF.Syntax.* + grammar) | DRAFT REVIEW (in compile rounds) | 02-syntax.md (whole chapter); ch. 05/06 §1 enum inventories (spot); 04-opsem.md §6.2 for the code0-metadata elisions; catalog entries on lines 256-263 | CLEAN — no findings. All 13 WF.Syntax rule ids present with correct statuses; the three status-mapping deviations (Anf/ContSecondClass as by-construction True-Theorems — both by-construction claims verified against the actual types; NonRecOccursPositive as documented anchor over the elided hint field) are already cataloged. StaticRecThroughCode's no-cycle-in-non-code-edges encoding checked equivalent: the shallow mention relation covers *all* symbol-bearing positions of static consts (set-of-closures value slots, Block fields, Immutable_value_array; the boxed/numeric-array forms hold only or_variable literals). ImmutableArrayNonEmpty covers exactly the 12 Immutable_*_array forms (Float_block correctly excluded). ExnHandlerFirstParamBucket encodes the checkable residue (count = 1 + extra_args; kind_ws lists align). Grammar matches the doc form-for-form with cataloged elisions; enum spot-checks pass: nullary prims (9, vs ch. 06 P.Nullary.*), bigarray_kind (14, doc order), string_accessor_width (10, incl. Eight_signed/Sixteen_signed), unary_int_arith_op (Swap_byte_endianness only, ch. 05:196). code0's elided param_modes/first_complex_local_param are safe: ch. 04 mentions them only in the NOTES of the conjectured IndirectUnknownArity.Partial, never in premises. replace_apply_cont's scope handling verified (nonrec handler always descended, k'-shadowing stops body; rec-group shadowing stops both; code0 kret/kexn shadowing stops body). Resolves W-4. New: W-12, W-13, W-14. |

| Values.v | GREEN RE-CHECK (delta over the draft review row) | 04-opsem.md §1.1/§1.4; Hopper's intake deltas | Draft verdict carries over. Deltas verified: (1) `value_kind` (Values.v:77-93) matches ch. 04 §1.1's by-kind listing exactly — V_tagged_imm/V_ptr/V_clos/V_null all K_value (closes W-6's residual sub-check), each naked form to its kind, V_region/V_rec_info to Region/Rec_info; (2) local lazy_block_tag dropped for Syntax.v's (:101-102); (3) KF-001's tupled ai_arity fix in place (:125-127, verified earlier); (4) OS.Simple.Eval's ρ-first symbol fallback (:357-366) matches catalog 16 with an accurate site note, Poison → None per catalog 8. |
| WellFormed.v (all 25 remaining ch. 03 WF.* rules) | GREEN REVIEW | 03-kinds.md (whole chapter); Base.v hosting of Erasable/Unarize (catalog 6); Syntax.v cross-refs (W-12/W-14) | CLEAN — no findings. Coverage grep = exactly the 25 ids, correct statuses (2 descriptive anchors: Scannable, Check.Gated — both comment summaries faithful incl. §6's by-construction vs re-checked split). Kinding judgment: const_kind table case-for-case with the doc incl. Poison's carried kind; the Coercion_id-base + WF_Kind_Coerce-transport encoding correctly delivers kind-invariance-under-coercion for the name-only-coercion syntax. Premise fidelity spot-highlights: WF_Let_Singleton keeps the doc's redundant kx/kx=kn two-line form; WF_Switch_NonEmpty's negative rule encoded as a premise of the only switch constructor (literal non-derivability, site-noted); Over/Partial's asymmetry (CODE's result arity for Over vs APPLY's return arity for Partial) preserved exactly; DirectArity's common-prefix agreement via deep subkind erasure matches equal_ignoring_subkinds; Let.Static's no-op context extension is justified (symbols kind unconditionally via WF_Kind_Symbol) and site-noted; bsp_matches_scc shape table correct (code↔code/deleted_code, SoC pattern↔SC_set_of_closures, block_like↔non-SoC). The In-based rec-handlers plumbing is positivity-safe and scopes Delta correctly (inv ++ params; all group members visible in every body and the let-body). W-12 RESOLVED (kinding … K_naked_immediate is WF_Switch_Scrutinee's first premise, :500). W-14 RESOLVED (Syntax.v:631-634 note + WF_Arity_ApplyFlavours). W-3 NOT settled here (no distinctness premises; doc states none). New: W-20, W-21. Nit (no finding): line 67's "the RULE / comment" prose wrap dodges the phantom-id grep only by the line break — lowercase it. |
| Cmm.v + CmmMemory.v (KF-011/012/013/014 sync batch, in Hopper's queue) and Soundness.v (KF-015 fix) | GREEN RE-CHECK #3 / DRAFT RE-REVIEW | 19-cmm-memory-gc.md (amended CM.Alloc.GC, fused); 15-cmm.md:516-584 (amended CM.Apply/CM.Apply.Raise, RR-threading); backend/cmm_helpers.ml (barrier/array shapes, prior W-11 ground truth) | All five fixes verified faithful; details in each finding's Status. KF-011 verbatim; KF-012's flag encoding (`expr_addr_ok'` addr_pos) is semantically my chain sketch, guard-checker-safe, with the marginally-stronger innermost-offset condition site-noted and acceptable; KF-013's fused CM_Alloc_GC kills the self-loop by construction (mandatory cmem_head allocation; no standalone collection) and embeds the KF-010 unpin; KF-014's RR-threading makes the exclave pattern run, requires both balances on return (trap balance also closes KF-007 residual (a) — struck), and threads RR′ through escapes per the runtime; cm_call_frame updated consistently, descent-device soundness carries over. KF-015's cumulative-seed accumulator is exactly the candidate fix, extended to retain raise payloads (right call). |
| Inlining.v (all 10 ch. 11 rules; pre-compile draft, audited at Girard's request) | DRAFT REVIEW | 11-inlining.md (whole chapter); Syntax.v seams (code_env :827, free_vars/bound_vars :1151-1152, exn_continuation/trap types :594-606); Base.v (unarize, ws_of_kind); CORRESPONDENCE Binders + status table | Coverage exact (10/10 ids, no phantoms); statuses clean — 3 normative constructors + Substitute.Region as a Definition equation (sanctioned form), 6 descriptive anchors per the standard mapping (no catalog-37 exception needed); the code_env seam has landed, so this is a full audit, not the conditional-textual one originally scoped. Structure-faithful throughout: let order (depth outermost, closure, params with x1 outermost) matches :95-98; absent-callee handling (no myclos let, do_not_inline depth) exact; region_rename's three-case table exact incl. both ∅ rows; the 2.1 ENCODING NOTE's total-rename-under-target-freshness argument is CORRECT (incl. the shadowed-source case, and cont_occurs counting binder occurrences is precisely what capture-freedom needs); ExnExtraArgs wrapper literal to the doc rule (exn::ā, Reraise pop, k_pop's plain pop, Never_returns omission via exn_wrap_ret, is_exn_handler on k1 only); d0-unconstrained ENCODING NOTE claims exactly what it delivers (sound over-approximation, meeting folded into the oracle side); inline_dec is a sanctioned oracle per the CORRESPONDENCE table row. Findings: KF-016 (medium — wrapper lets can capture caller-side names in argument simples; the doc's α-freshening NOTE has no premise counterpart), KF-017 (medium — sequential renames vs simultaneous θ when kexn-target = k_ret^c), KF-018 (medium — k1 unconstrained vs caller's return cont in the wrapper). New W-23 (code0 binder-distinctness WF reliance, W-21-shaped). Re-audit at compile; re-check the C-tying WF premise shape when Soundness.v inc 2b states it. POST-AUDIT (same day): the fifth frozen-interface amendment landed (leading unused `flags : eff_flags` on rw_inline, :722 — the Section-Variable-pruning rationale in the site note is correct and the closed rw_* type matches Machine.v's), and Girard applied all three fixes (verified on disk, statuses updated in each finding; constructors now at :735/:776/:820). Verdict upgraded: no open findings; W-23 remains the only open item, pending main's premises-vs-WF-layer ruling. |
| Opsem.v (all 28 in-file ch. 04 ids + cross-file OS.Simple.Eval / P.Static.MixedBlock hosting) | GREEN REVIEW | 04-opsem.md (whole chapter, §4/§5/§7/§8 line-by-line this round; §6 dispatch verified across the draft/KF-001/prim_op-amendment rounds); Values.v seams (handler_entry, at_apply_cont, CE_* entries) | Coverage grep exact cross-file. The §4 apply-cont family transcribes cleanly: handler_entry covers the K′ knot's both entry forms (W-2, resolved earlier); at_apply_cont (catalog 7) folds ⟦s̄⟧ρ into the match and correctly gives Ctl_jump entries no trap action, so boundary entries chain; Return/ExnBoundary's unconstrained t matches the doc rules' silence (site-noted); ExnBoundary's stored-extras supersession follows the doc's own conclusion; Raise-vs-ExnBoundary dispatch is by entry shape, exactly the doc's NOTES. OS.Switch exact; undef_next (Invalid/Switch.Undef/prim-undef) matches the doc's explicit-UB inventory, kept separate from `stuck` for nondeterministic denotations (right call, site-noted); OS.Unit.Init's rho_pre/H0 parameterization matches "from every starting heap". Two notes, no findings: the doc states \|x̄\|=\|v̄\| only in OS.ApplyCont, not the TrapPush/TrapPop/Raise variants — Rocq transcribes the asymmetry exactly (env_upd_vars truncation unreachable in WF programs; doc-precision candidate, not drift); OS.ApplyCont.Return ignores rather than executes a Pop on a return jump — matches the rule block (T discarded wholesale), the §4.3 "executed before the jump" prose notwithstanding. ONE finding: KF-019 (medium-high) — final_config/HB_return/HB_exn demand a trap-action-free jump, but the doc's halting shapes are trap-silent and a toplevel raise is a Pop-carrying jump to k_exn⁰: uncaught exceptions at the unit toplevel classify as Beh_undef, corrupting no_ub and the v_exn observation. KF-019 FIX VERIFIED on disk (2026-07-18): trap action quantified in final_config + HB_return/HB_exn per the candidate fix — see the finding entry; no other delta relevant to this row, review durable. |
| Unboxing.v (all 18 ch. 12 rules; pre-compile draft, audited at Girard's request) | DRAFT REVIEW | 12-unboxing.md (whole chapter); CORRESPONDENCE HYBRID-variant wording (:36-46); TypeGrammar.v seams (is_null_ty :91-93, HV_variant/HV_closures field order, alias_type_of :443); Syntax.v Mk_cont_handler (:757-758, third field = is_exn_handler); MeetJoin.v meet signature (:595-603); types/provers.ml ground truth for KF-020 | Coverage exact (18/18 ids, no phantoms). Statuses clean: 15 descriptive anchors (all comment summaries faithful to their rule blocks, incl. Variant.Discriminator's three-way table and Mutable.Rewrite's five-row rewrite table), Denv.Equation as a normative Definition, ContParam.Rewrite as the sanctioned single constructor (logged decision), AccumBoxElim as the HYBRID Theorem+Admitted — the clause-(1)/(2) split matches CORRESPONDENCE's sanctioned wording (clause (1) stated under the rule id, clause (2) documented in the same comment with the composition chain). Decision language mirrors Unboxing_types constructor-for-constructor (incl. New_let_binding_with_named_args's simples→prim builder); unbox_uses mirrors make_decisions' Non_recursive/Recursive; unbox_dec is a sanctioned oracle Parameter. The exn-handler exclusion IS pinned (the constructor's literal `false` in is_exn_handler position). Findings: KF-020 (medium — AccumBoxElim quantifies any boxed head incl. Maybe_null; code-falsified overclaim), KF-021 (medium — Denv.Equation's meet stated in E where the component params need not be bound), KF-022 (medium-low — decision_shape's closure/variant maps pinned in one direction only, undisclosed slack). Low notes, no findings: (i) only the E_let_cont_nonrec form is covered though the chapter's flagship example and AccumBoxElim's composition are recursive-continuation cases — disclosed in the ENCODING NOTE, but the rec constructor is cheap and recommended; (ii) `uses` in RW_Unbox_ContParam is existentially free, never tied to k's use sites in body — should be disclosed; (iii) `map fst extra = ud_vars U` pins the extra params' variables but not their kinds, though the kinds are derivable from U; (iv) the conclusion pins `params ++ extra` (nothing dropped) where the doc's conclusion drops unused params — a canonical-form choice the caveat lists but the constructor actively contradicts rather than leaves open; (v) the AccumBoxElim note cites bare "catalog 37" — cite the HYBRID variant. Re-audit at compile. GREEN RE-AUDIT (2026-07-18): KF-020/021/022 fixes verified on the compiled source (details in each finding's Status); notes (i)/(ii)/(iv)/(v) all actioned — RW_Unbox_ContParam_Rec (:683-703) is one handler of an E_let_cont_rec group with UU_recursive pinned, invariant params unchanged, siblings pinned by map fst, rw_unbox's closed type untouched; uses-free and pre-dropping-form disclosures in the ENCODING NOTE (:640-646). ONE NIT (to Girard, no finding): the note says sibling "bodies are unconstrained" but map fst pins only names — sibling params/flags are free too; one word ("handlers" for "bodies") closes the disclosure gap. Ch. 12 closed 18/18. NIT DISPOSITION (2026-07-18): Girard agrees it's a genuine disclosure gap but DEFERS the one-word fix — a comment delta on a green, audit-closed file would cost a compile slot + Dijkstra delta check during the waves-6/7 bottleneck; booked as first line of any future Unboxing.v edit, else ships as-is under the descriptive caveat. Accepted (my no-finding call stands). |
| RewritesControl.v (all 35 ch. 10 control-side rules; pre-compile draft, reviewed at Church's request) | DRAFT REVIEW | 10-simplify-rewrites.md (all control-side blocks incl. Invalid.Propagate :1424-1442); Syntax.v replace_apply_cont (:844-931, the W-13 check); simplify_apply_expr.ml (:824-1000 arity dispatch + arity_mismatch), flambda_arity.ml#partially_apply (:164-173), simplify_common.ml#split_direct_over_application; CORRESPONDENCE catalog-37 list (now naming this file's six anchored conjectures) | Coverage exact: 35 headers set-equal to ch. 10 minus RewritesPrim.v's 19 (Prim.* ×14, CSE.* ×3, Alias.Canonicalize, Share.StaticDynamicSplit). Statuses clean: normative constructors; DemoteExn as conjectured defining clause (B-side precedent); ValueSlotExempt conjectured→Admitted; Phantom/Specialize descriptive anchors; the six pass-machinery conjectures anchored per the CORRESPONDENCE amendment (list matches exactly). Church's deltas all verified against code: leading code_env (ruled, site ENCODING NOTE); OverApplication split at length (c0_params cd) with component-level trigger (matches split_direct_over_application's assert); PartialApplication component-boundary fix CONFIRMED (partially_apply = split_at over components; num_params measures at :451-452) with the explicit whole-leading-components coverage premise; IndirectToDirect C cid <> None. Ambient-flags DeadBinding matches the Machine.v pattern (rejected forall-fl documented). W-13 RESOLVED (no double-filtering; use-kind classification faithful). wrap_loopified does NOT silently rely on NoDup params (pure syntactic wrap, no substitution; k-freshness premised) — concur with Church's no-code0_wf-premise determination, modulo two composition contracts flagged to Simplify.v (W-24). Findings: KF-024 (medium — Shortcut drops the binder while Apply-position uses of k are never retargeted; doc shares the imprecision), KF-025 (medium-high — Apply.Invalid disjunct 3 fires on every valid over/partial application; code truth is prefix-per-parameter mismatch + exact-only result check), KF-026 (low-medium — PartialApplication stub-internal binder distinctness unstated but instantiable), KF-027 (low — ValueSlotExempt iff refutable when the pattern binds my_closure). Low notes: Inline lacks is_exn_handler=false (benign under WF); DeadRegion narrower than doc, disclosed; Invalid.Propagate's te_is_bottom umbrella deviates from the doc's five-source disjunction — sound, but the comment should be marked ENCODING NOTE and msg is free; IndirectToDirect's consistency direction (cid newer-eq cid0) — confirm against Code_age_relation.meet's resolution; Demote adds a scoping-necessary invariant-params premise absent from the doc (doc-precision candidate, disclosed). Re-audit at compile. DELTA RE-VERIFY (2026-07-18, still draft-stage): all four findings fixed on disk and verified (KF-025/026/027 statuses updated; KF-024 landed per ruling (b) — keep-binder + S_Rewrite_LetCont_Shortcut_Alias companion with retarget_apply_aliases, both grounding cites verified: expr_builder.ml:558-573, continuation_shortcut.ml:59-63). Low notes actioned: Invalid.Propagate comment now carries the ENCODING NOTE marker; IndirectToDirect direction confirmed by Church against code_age_relation.ml:54-101 (meet keeps the NEWER id; the rule licenses only the resolution-lands-on-type's-cid case, a safe under-approximation, documented at the helper); Loopify.Body's wrap-time recompute staging now names Simplify.v's final-body re-run obligation (W-24(b)). GREEN RE-AUDIT CLOSED (2026-07-18): Hopper's recompile green (Dijkstra re-stamped 02:28:46/02:30:21 on this source/vo pair); sole .v delta since the delta re-verify is the comment-only to_alias CODE anchor at :1257-1262, verified on disk against the applied doc anchor (10-simplify-rewrites.md:810). All finding-fix sites were verified on this same disk state; review durable. |
| Soundness.v increment 1 (obs_equiv + no_ub; pre-compile draft, reviewed at Plotkin's request) | DRAFT REVIEW | 13-soundness.md §1; 04-opsem.md §8.2; 06-primitives-memory.md P.Binary.PhysEqual; Opsem.v event/behavior deltas; simplify_set_of_closures.ml + simplify_apply_expr.ml (function-slot claim) | The observation apparatus delivers the joint W-1 design: addr_bij is a real partial bijection identity-on-symbols; `pins H0 b` in every beh_sim constructor; heap_sim + root seeding = reachable-substructure-up-to-renaming (minimal-b argument in the ENCODING NOTE is right: b existential, larger b only constrains more); ONE b per behavior pair over all events + terminal observation. All three committed defaults CONCUR: trace values via b; closures as leaves (Osim_closures) — and the Vsim_clos literal-function-slot premise's RESOLVED claim is code-verified (Function_slot.rename only at simplify_set_of_closures.ml:687 for a lifted-closure symbol NAME; fresh slot only at simplify_apply_expr.ml:468, a NEW partial-app wrapper; grep over simplify/ shows exactly these two sites); pins+heap_sim observing all initial-heap final contents is sound (context retains H0 pointers). no_ub = no Beh_undef correctly excludes undef∨stuck (Opsem.v:1193-1196). Bsim_exn observing the v_exn-reachable heap concurred (ch. 20 compares exception values through ≈). ONE finding: KF-015 — event_sim's per-event b_ev omits values retained from earlier events, under-observing cross-event externally-visible mutation. Re-review at compile + increment 2b (the four real statements; W-21 applies if expr_wf becomes a hypothesis). |
| MeetJoin.v (all 39 ch. 08 rules; GREEN) | GREEN REVIEW (co-read: Plotkin dispatch + sections 1-2, me sections 3-4 + code cites + ledger) | 08-meet-join.md (whole chapter; provers/expand_head/reify blocks line-by-line); 07-types-domain.md:287-318 (the Relational pin); provers.ml (gen_value_to_proof/meet :78-101, prove_equals_to_simple_of_kind :102-124, prove_is_int :128-154, prove_get_tag :183-213); type_grammar.ml (Relation.of_const :106-114, is_int_for_scrutinee :4287-4301, all_tags :3616-3620); meet_and_join.ml (reducer :771-824); aliases.ml (demotion choice :721-730, name_defined_earlier :411-456); reify.ml (:99-337); typing_env.ml (get_canonical_simple_exn :959-990); CORRESPONDENCE ENVELOPE-QED wording (:47-55) | Plotkin's slice CLEAN at medium+ (his 5 low items logged below). My slice: provers faithful — T.Prove.Sound/MeetShortcut transcribe the corrected intersection reading exactly; gen_value_to_meet's is_null discard is structurally encoded (METI_imm isnull wildcard = provers.ml:95's `is_null = _`), gen_value_to_proof's Not_null gate pinned in PII (:81-84); prove_equals_to_simple_of_kind exact incl. the hardcoded Name_mode.normal floor (:121). Both my-slice ENVELOPE-QED rules verified as genuine code-witnessed envelopes, NOT weakened masquerades: NullPremise's Qed exhibits the doc's meet_test.ml witness verbatim (Maybe_null + Known_result {1}); SimpleModeBoundary's Qed genuinely derives the boundary property from PES_alias's mode-floor disjunction (stated on the one modeled Simple-returning prover, disclosed); comment format per CORRESPONDENCE:47-55 in both. T.Expand.Head faithful (constant-canonical exclusion disclosed; one-step/path-compression NOTES transcribed). T.Prove.GetTag's Ob_bottom = all_tags' Known condition exactly (:3618-3620). Plotkin's watch items both settled by cite: (a) earlier-binding-time-wins CONFIRMED (aliases.ml:721-730 demotes the non-earlier element; :411-456 compares binding times) — his descent-relation reading stands; (b') rel_of_const cite REFUTED the comment → KF-028. Findings: KF-028 (medium — rel_of_const inverts of_const's is_int on null; normative doc pin), KF-029 (low-medium — RF_alias misses reify.ml's Name_mode.normal floor). Plotkin's low items (logged for the ledger, with Scott): (P1) MH_boxed kept-left alloc mode lacks the site note its siblings have; (P2) M_alias_alias without s1<>s2 silently diverges from the equal-canonicals Both_inputs path (sound direction; wants premise or note); (P3) T_Meet_Store_CoercionErasure Qed-able by fupd reflexivity (entry-34 precedent) yet Admitted; (P4) T_Join_ConstAgreement's exists-a-live-branch premise is a correct addition — CANDIDATE DOC CLARIFICATION; (P5) meet_rlfb unrelated-corners inventory omits the differing-single-tag pair. My low notes: (K1) T_Reify_Sound's statement drops the Lift clause (disclosed in comments; vacuous today, but nothing forces a future R_lift derivation to satisfy it — LiftLocalGuard was kept as interface, the Sound-Lift clause wasn't); (K2) PGT_known's isnull/imms wildcards are sound block-restricted slack vs gen_value_to_proof's Not_null gate — disclosed, but the superset `In t ts` vs the code's exact tag set could use one phrase; (K3) NullPremise's envelope shows Maybe_null+Known_result coexistence; the refutation of the naive reading additionally leans on gamma ∋ Null (T.Gamma.Value.Nullability), not part of the statement — a free-strengthening candidate, optional. FIX VERIFY (2026-07-18, pre-green): KF-028/029 fixes + K1/K2 verified on disk (details in the finding entries); K3 declined by Scott (Qed'd-envelope stability), accepted. RE-STAMPED GREEN (2026-07-18): Hopper's 02:34 sweep compiled the fixed source (02:31:34; both fixes grep-verified :1148/:1711), two green rebuilds since, Dijkstra ch. 08 content-delta re-stamp done (30 headers / 12 Admitted unchanged). Row closed. DELTA 2026-07-18 (alias-channel re-sweep, see its own row): three Admitted statements from this file found REFUTABLE — KF-051 (T_Meet_Sound, sat_ext vs symbol fallback; upgrades P2 to refutation grade — the :626-630 note's "sound direction" claim is false), KF-052 (T_Expand_Head, fixed-rho iff vs the doc's set-form equality; MY ERRATUM #4 — my slice called it faithful), KF-053 (T_Meet_GreatestLowerBound, Admitted despite its own comment disclosing the MutableBlockMissedBottom refutation; Plotkin's slice, the comment's admission was in nobody's frame as a finding). The row's per-rule fidelity verdicts otherwise stand; the re-sweep found the remaining six gamma-quantified Admitted statements clean through the repaired channel. DELTA 2026-07-18 (later): KF-051 and KF-052 RESOLVED at Hopper's green (fixes verified on disk, twice-repaired T_Expand_Head statement blessed for a polish-wave Qed); KF-053 HELD for main's shape ruling — the file's one remaining known-false Admitted. |
| Soundness.v (all 12 ch. 13 ids; GREEN) | GREEN REVIEW (full file; supersedes the row-47 increment-1 draft review, whose verdicts carry over) | 13-soundness.md (whole chapter: §1 prose, §2 :103-141, §3 :157-533 line-by-line, §4 for item-8 interaction); Simplify.v (union/closure/simplify_unit, just reviewed); MeetJoin.v tenv_descends + the KF-029 floor; WellFormed.v code0_wf/expr_wf seams; CORRESPONDENCE traceability-exception + HYBRID wording | Census exact: 12 chapter ids = 12 headers, 4 Admitted (Preserves, Local, NameMode.Coherent, AliasesMonotoneDown's stated half), 8 anchors under the sanctioned exception with true STATUS preserved. All eight anchor summaries verified faithful line-by-line against §3 (EffectfulDeletionInventory's two shapes + scope carve-outs; RegionPairAtomic's dichotomy + coinductive cut; RequiredNamesSound's (a)/(b) split + FIVE-introducer inventory + two fatals; DeadCodeBodyLocal's two directions + exactly-three channels; LiftedConstGranularity's two granularities + version-chain-only bite; DeadValueSlotCoherence's FIVE consumers + accidental SURVIVAL⇒RECORDED alignment + Cinvalid consequence; KindChecks.Gated; TrapNeutral legs (a)-(c)). Increment-1 apparatus re-confirmed on green source: KF-015's cumulative [seen] accumulator in trace_sim_from/retained; raise payloads retained; results under full b. W-23 residue CLOSED: Preserves conjoins (forall cid c0, C cid = Some c0 -> code0_wf c0) consumed BY NAME (:517), per ruling. W-21 CLOSED: expr_wf hypothesis omits WF_Prim_ArgKinds (prim_arg_kinds Section-pruned) and no 2b statement needs it — kind pathologies surface as stuck runs, which no_ub excludes. AliasesMonotoneDown HYBRID verified: stated half (same_class preservation over tenv_descends) is the doc's classes-merge-never-split core; documented half (no-removal, canonical movement, proved-ness-not-answer stability, closure-entry In_types degradation w/ typing_env.ml:1037-38 cite) matches the block + NOTES; the operational-tenv_extends non-vacuity rationale recorded. NameMode.Coherent faithful: let binders are exactly where modes live in modeled syntax; Phantom sharpening to no-free-occurrence disclosed; expr_ctx reach = every expr position (inventory mirrors Simplify.v frames one-for-one, verified). Plotkin's KF-029 tripwire DEFUSED: with RF_alias's mode floor landed, reify no longer leaks In_types Simples into rebuilt terms, so the conjecture has no model-artifact refutation through that channel. Preserves' added premises all disclosed and right (consistent E0 — without it false outright; C-in-H0 imported-code envelope, HYBRID-flavored narrowing; per-heap no_ub scoping). ONE finding: KF-030 (medium — INV_Rewrite_Local's "modeled as the rewrites union" claim silently narrows the doc's quantification: CSE.Replace/Extend and SelfTailCall are outside the union, the factoring breaks at Preserves' S_cse/simplify_code arms, and the CSE omission is load-bearing since #16 refutes that very instance; disclosure fix). Low notes: (L1) the universal kind/arity-table ENCODING NOTE (both statements) should carry the one-phrase safety argument — benign BECAUSE stuck runs are no_ub-excluded — currently it records the widening without saying why it cannot bite; (L2) TrapNeutral's anchor summary omits leg (d) (with-handler branch loopifiable, try-body self calls rejected) — one line; (L3) stale DRAFT NOTE compile-gate sentence (:19-21) — fold into any future edit, no action. DELTA 2026-07-18: KF-030 disclosure fix + L1 + L2 VERIFIED on disk (KF-030 bullet :569-588; L1 :508-510 at the Preserves note with Local deferring by reference — accepted; L2 leg (d) :874-877). KF-030 RESOLVED; review durable. |
| Simplify.v (rewrites union + congruent closure + whole-run; GREEN) | GREEN REVIEW | 13-soundness.md §2 (:103-141, the rewrite-family quantification + compositionality claim); 09-simplify-structure.md :28-96 (S.Struct.Run + ClosedResult + NoPendingConstants + SingleRound); 10-simplify-rewrites.md :427-494 (CSE.Eligible/Replace/Extend judgment forms, for the union-exclusion question); SimplifyStructure.v :124-198 + :761-801 (the three output-contract predicates); RewritesPrim.v sec. 5 seam (rw_cse/cse_extend, read for finding #16); W-24 verification (prior, this file's watch section) | CLEAN — no findings. Union arms (:65-74): exactly the four rewrite families at the frozen closed type; the deliberate NON-arms all check out — rw_code_loopify/rw_self_tail_call composed by simplify_code (W-24, both contracts verified earlier and closed), rw_cse/cse_extend composed by cse_deep. The CSE relocation out of the union is FAITHFUL: ch. 10's CSE.Replace premise reads the table from denv (not from the rewrite's E), so a union arm at the frozen five-argument type cannot carry it; the model's factoring (explicit table index, cse_extend validating each entry against the crossed binding, S_cse hard-coding fempty so every table at a CSE_here is reachability-validated) REPLACES the doc's out-of-model dacc validity with an in-model discipline — stronger than the doc asks, sound. cse_deep frame analysis: handler frames thread tbl unchanged = entries from enclosing lets only (subset of any join-point table incl. across back edges — sound side); no body-side entries leak into handlers; no code-body frame (denv rebuilt per function body); skipping extensions (CSE_let) shrinks the table — sound side; one-replacement-per-firing recovered by S_trans refiring from ancestors (recovers the pass's real lineage). stc_deep re-checked as part of the closure: frame inventory identical to cse_deep's plus the k/kret/kexn stopping discipline (W-24(a), previously verified). simplifies frame inventory COMPLETE vs Syntax.v's expr: let body, code body (both S_cong_let_code and the S_code_rebuild pipeline), nonrec handler/body, rec handler (arbitrary group element)/body; apply/apply_cont/switch/invalid have no expr subterm. Fixed-E ENCODING NOTE is the right call and correctly argued (tenv_extends is unvalidated — weaving tenv_descends in would admit unsound environments and falsify ch. 13; fixed E under-approximates on the sound side). simplify_unit vs S.Struct.Run: preserved fields (rc/ec/region/ghost/sym) match run's rebuild; side results (free_names/final_typing_env/all_code/slot_offsets) correctly declared out of unit-to-unit meaning; conjoins exactly the doc's two self-checked post-conditions plus Lift.EmptyAtEnd (the doc cross-references it from NoPendingConstants — right conjunction). Pilot lemmas: all four Qed'd, exercising nesting, rec-group two-handler transitivity, and the no-loopify pipeline. NOTES, no findings: (N1, nit for next edit) the DRAFT NOTE :31-38 (RewritesPrim.v "not yet on disk") is stale — moot since wave 5 landed; comment-only, ships as-is per the Girard-deferral precedent. (N2, booked to Soundness.v review) ch. 13 :106/:120 quantifies INV.Rewrite.Local over ALL S.Rewrite.* incl. CSE.Replace, but the union excludes rw_cse — check Soundness.v's Local statement covers or discloses the CSE carve-out (a context-free Local can't even be POSED for CSE.Replace without a table-validity hypothesis; and #16's witness refutes Local for it a fortiori — item 8 names only INV.Simplify.Preserves). (N3, booked to SimplifyStructure.v review) S_Struct_Run_ClosedResult adds a ghost-region disjunct + a free-continuations conjunct beyond the doc's rule text (justified in-comment via OS.Unit.Init; the doc may under-report the code's check) — assess there, it's that file's rule. |
| ToCmmControl.v (all 15 ch. 16 ids; GREEN) | GREEN REVIEW (the W-22 agenda) | 16-to-cmm-control.md (whole chapter); to_cmm_expr.ml (translate_raise :579-610, translate_jump_to_continuation :612-637, let_cont_not_inlined :860-905, let_cont_exn_handler :909-944, apply_expr :1033-1120, switch :1221-1330, translate_apply :499-570); trap_action.ml#Raise_kind.option_to_lambda; Cmm.v CM_Apply_Raise/CM_Raise seams (W-18); CmmMemory.v W-17 | Census exact: 15 ids = 15 headers, 14 normative (constructors/defs) + INV.ToCmm.Control conjectured => Theorem+Admitted. Structure faithful throughout: tc_realized's 3-way classify + Rec-as-Jump; LetCont.Inline/Jump/Rec premise-for-premise (NoDup+Forall-fresh transcribes "lblᵢ fresh"; rec_jump_entries positional zs++params; handler coldness false); tc_trap one-to-one through Phi; ApplyCont.Return's None/Pop disjunction with trap_return as Return_lbl Cexit (= cmm_helpers' shape); ApplyCont.Inline's length + Rec_info-skip binding; Apply.Call's four kinds with direct-call my_closure-appended-last and hook-deferred indirect/extcall images; Apply.Return's five rows with flush modes VERIFIED code-accurate (:1058/:1073/:1087 Flush_everything, :1120 Branching_point, single-Inline no flush); Invalid exact. W-22 dispositions: (1) FINDING KF-031 — option_to_lambda None = Raise_notrace, model says Raise_regular; (2) RESOLVED no finding — translate_raise pattern-ignores the Pop's exn_handler (:584), doc ellipsis and .v unconstrained k_h both code-accurate; (3) in-range-agreement claim SOUND, but FINDING KF-032 — eq-only/syntactic-retag-only shapes exclude the code's selected pre-tagged scrutinee (extra_info, :1254-1267) and the doc-named != form; the note's coverage claim for != is wrong; (4) RESOLVED no finding — asymmetry real (handler translated :868-870 BEFORE add_jump_cont/add_exn_handler; body under the k-extended env :928), immaterial under WF (nonrec handler cannot use k) — note the Jump case's doc "Θ_j ⊢ e_h" is the same harmless slack; (5) ctrl_kenv per-class linking + ctrl_traps Forall2 + R=RR match the doc's own coarse granularity, verified vs Values.v centry forms; (6) L_tau/Ctl_expr restriction disclosed with a sound presupposition argument (C-call rows heap-mutating, boundary endpoints have no source expr to relate); the call-step big-step mismatch is site-flagged with main — residual booked to ToCmmSoundness.v intake; (7) RESOLVED — doc fix landed (16-to-cmm-control.md:380 now includes OS.LetCont*); the :797-801 "raised with main" sentence is now stale (comment nit). Round-4 fl delta: forall fl harmless — control_shape excludes E_let, so no prim denotation on the matched step. KF-014 caveat: control steps do not touch R/RR; the Apply-row interaction with RR-threading rides the deferred call-step matching. W-17: ch. 16 routes all Lets through let_binding_ext — atomics are ch. 18 prim rows; RE-BOOKED to the ToCmmData.v review. W-18: SETTLED as KF-033 (medium-high, doc-rooted, escalated) — the translate_apply extras-reraise wrapper (:499-570) is owned by NO rule (ch. 16 NOTES-only; ch. 18 doc+.v silent; ToCmmControl.v misattributes it to ch. 18), so the composed model strands cross-call raises into extras-expecting handlers as CMO_undef. Also FINDING KF-034 (low-medium) — INV conclusion's unconstrained existential tr admits event-emitting "matches" for a silent step; pin tr = []. Low notes: (a) the code flushes Branching_point in the shared prologue (:865) BEFORE the exn dispatch, so real TC.LetCont.Exn has a flush the doc rule omits — doc-precision candidate, escalated softly with KF-033; (b) skip-param filtering (remove_skipped_params :898, :920-926) lives in the var_binds/simples_translate hooks, consistency under the intended instantiation — ch. 20 would surface any mismatch. DELTA (2026-07-18, fix round, pre-Hopper): KF-031 RESOLVED (:138-143), KF-034 RESOLVED (trace pinned [], :905), KF-032(a) resolved via TCSw_Two_Ne widening (:224-232) + note correction; KF-032(b) taken as disclosed UNDER-APPROXIMATION whose stated obligation is the new kernel TC_Switch_TestByValue (:252-269) — but that kernel is REFUTABLE as stated (KF-037, high: flat venv/kenv leak test binder extensions into the branches; Admitted-false = inconsistent axiom context). Strength check also exposed the same class of hole in ToCmmData.v's TC_Let_Subst via free chi' (KF-038, high, retroactive). KF-033's modeled-NOWHERE note verified (:726-736); ExnWrapper .v constructor still pending the corrected doc (nesting transposition + debug gate, KF-033 DELTA 2/3). File NOT re-greenable until KF-037 lands. |
| PrimScalar.v (all 31 ch. 05 ids; GREEN) | GREEN REVIEW (full file vs whole chapter + code spot checks) | 05-primitives-scalar.md (whole chapter, 972 lines); PrimMemoryA.v ece record + EC_pure/EC_read (:155-201); flambda_primitive.ml Box_number effects clause (:1638-1654) | Census exact: 31 chapter ids = 31 headers (sorted lists diff clean, multiplicity included); 27 normative defining clauses as denot_scalar constructors / definitions, P.Contract.NoRaiseNoControl + the three P.Effects.* as Theorem+Admitted, FolderPicksZero descriptive => documented anchor. ZERO findings — the cleanest large file yet. Verified semantics leg by leg: swap_si per-kind exact (immediates low-16-swap-zero-extend via Z.land 65535, int8 identity, int16/32/64/native byte_rev over the unsigned pattern); NumConv's uniform wrap-at-destination provably equals every doc bullet (sign-extend-then-wrap, truncation, identities); int->float32 THROUGH A DOUBLE preserved with a do-not-fix guard comment (open question 3 / double-rounding memory); float->int undef boundary table numconv_bound exactly the chapter's u(dst) — Wn for immediates (the 4.6e18 example folds, not undef), 63 host-int for int8/16 (200.0 -> -56 wrapping captured), w(dst) for 32/64/native; NaN/inf as sif_trunc None routed to the OutOfRange undef row; DivMod's Mod transcribed unwrapped as in the doc (always in range, incl. min/-1 -> 0 agreeing with the derivable DivMinIntByMinusOne constructors); shifts signed/logical/arith correct via to_unsigned, count a naked_imm with the doc's undef band (s<0 or s>=w, in_width W dropped in the undef row = the "does not fit a host int" parenthetical); IntComp Eq/Neq signedness-free, su_view for the inequalities, zcmp for compare-functions; FloatComp.Bool per the NaN table (ltb/leb false on NaN, Gt/Ge as swapped ltb/leb, Neq true on NaN), CompareFunction on the total fXX_compare (NaN below all, NaN=NaN); Box/Unbox via alloc + HO_Boxed with kind_of_boxable; ReinterpretBoxedVector's two layouts (boxed vector | tag-0 zero-value-prefix mixed block of 16/32/64 suffix bytes). Effects rules verified to the code: EC_pure/EC_read quadruples match the doc's exactly; Box_number placement CODE-VERIFIED (flambda_primitive.ml:1640-1649 — classic+Heap Delay, else Strict, incl. the heap+non-classic case the doc NOTES leave implicit; validity elided per the disclosed ENCODING NOTE, matching the doc's "..."). Low notes (to Curry): (L1) P_Contract_NoRaiseNoControl is provable by destruct r — its own note says "holds by construction" — a free Qed under the reflexivity-Qed precedent (catalog 34); (L2) int_arith_op_sem's BIA_div/BIA_mod equations are dead (total_int_op gates them out; DivMod rows inline their own formulas) — harmless, consistent values; (L3) the wrong-layout/wrong-kind undef sentences of ReinterpretBoxedVector and UnboxNumber are left STUCK per the sanctioned undef policy (catalog 23) but neither site note cites the policy — one disclosure sentence each. DELTA (2026-07-18): all three low notes applied by Curry immediately, VERIFIED on disk — (L1) P_Contract_NoRaiseNoControl is a real Qed (:362-369, destruct + two bullets; ENCODING NOTE cites the catalog-34 precedent), Admitted count 4 -> 3 (only the three P.Effects.* theorems remain, :778/:791/:813); (L2) Div/Mod unreachability note at :137-138; (L3) catalog-23 citations at both sites (:520 ReinterpretBoxedVector, :550 UnboxNumber with the BooleanNot contrast). Term-level change (the Qed) routed to Hopper for re-green; my GREEN verdict carries over — the proof body is closed content, nothing normative moved. |
| ToCmmData.v (KF-035/KF-036 fix round) | FIX VERIFICATION (targeted; full doc-vs-.v review still pending at green) | 18-to-cmm-data.md TC.Let.Static; 17-representation.md:354-375 (R.Obj boxed numbers); to_cmm_static.ml (static_boxed_number :106-123 + git -L history, static_block_updates :75-104, array emitters :228-296); to_cmm_shared.ml Update_kind + make_update (:382-523); cmm_helpers.ml (strided_field_address :1580-1583, emit_block :4227-4231, boxed-constant emitters :4237-4280) | Both fixes RESOLVED and verified offset-by-offset (details in the finding entries): sc_holes/ov_holes strides match Update_kind exactly (packed 1/2/4/4, word 8, vec 16/32/64; mixed blocks by field_size_in_words); static_update's caml_initialize/Cstore disjunction matches make_update's must_use_setfield split incl. the return_unit wrapper; soc_holes' existential judged acceptable (address-coincident under R.Obj). The verification EXPOSED KF-040: the code's boxed custom-block updates (index 0 since the Flambda 2 import) write the ops word, not the payload at word 1 — a latent COMPILER bug (dead in the production pipeline: no Var-payload producer for Boxed_int32/int64/nativeint/float32; reachable via fexpr), which ov_hole transcribes faithfully while its comment asserts the wrong layout; escalated to main. |
| ToCmmSoundness.v inc 2 (sunk closure + license; EARLY W-26 CHECK at Milner's request — not the file's review) | TARGETED CHECK (pre-green) | catalog items 64/65; 18-to-cmm-data.md:126-142; Cmm.v machine ground truth (CM_Head_Let :911-914, CM_Catch_NonRec :1017-1024, CM_Exit :1182-1190) | W-26 checks: (a) definition matches item 64 — PASS; (b) license = the quadruple's content, stated positively (totality + mention-agreement), vacuity trap avoided — PASS, and the license survives both KF-037/KF-038 recipes as a license; (c) FAIL at the move: Sink_intro's guards leave the binder-extension leak open in three quadrants (d's internal binders read by sl; sl-frame-internal binders read by d; the entire label/chi channel — no cmm_mentions_label exists). Finding KF-041 (high) with constructive counterexample; symmetric mention-disjointness + label twin proposed. Item 65's "W-26 discharged" needs a catalog correction. Full-file review (W-25, KF-033 seams, INV call-step residual, catalog-62 closure statement, TC.Let.Static layout handoff — now incl. KF-040's SC_boxed_* Var holes) at green. |
| PrimMemoryB.v (all 33 of its ch. 06 ids; GREEN modulo KF-039) | GREEN REVIEW | 06-primitives-memory.md:797-979 + :1082-1651 (B's split of record with PrimMemoryA; the five remaining ch. 06 ids verified housed elsewhere: four P.Effects axis rules in Syntax.v:111-122, P.Static.MixedBlock in Opsem.v:293 — chapter total 69 = 31 A + 33 B + 5); Values.v value grammar/value_kind (:52-93) | Census exact: 33 ids = 33 headers; 19 normative + 4 conjectured (GetHeader/IsBoxedFloat/ReadOffset/WriteOffset) as defining clauses of denot_mem_b — the conjectured-as-clauses treatment carries cited coordinator rulings (kind-constrained nondeterminism, no raw addressing) — plus ControlBarriers (effects Definition + 3 data clauses) and 9 documented anchors (FrontendInsertsChecks normative-as-anchor under the out-of-scope-component ruling, ENCODING NOTE present). ONE finding: KF-039 (PhysEqual drops the kind-Value contract; catch-all false arm relates out-of-kind pairs with defined wrong answers, against the file's own stuck policy). Verified semantics leg by leg: IsInt pair (pointer_shaped widening = the NOTES' "ANY pointer", null included); IsNull; GetTag via block_runtime_tag (Block/MixedBlock tags, FloatBlock = 254 = Double_array_tag; immediate/other-object args stuck per policy); GetHeader's t <= 251 + nondeterministic nativeint per ruling; ArrayLength (unarized |elems|, akl_consistent exact|dynamic); StringLength (length immutable even for Bytes); BigarrayLength (1 <= d via nth_error d-1; mutable-descriptor coeffect note matches); Project{Function,Value}Slot with the disclosed V_clos-for-ptr ENCODING NOTE (slot-identification argument sound); IsBoxedFloat 4 clauses incl. Is_flat_float_array (FloatBlock + naked-floats arrays = Double_array_tag per R.Obj.Array); ArrayLoad (+product cell kind via j mod width per NOTES; nth_error carries the upper bound; out-of-range undef companion); ArrayLoad.Vector (packed little-endian first-element-least-significant = doc; Values/Gc_ignorable excluded via array_elem_byte_width None; product restriction split correctly between forallb-vec128 width and the 2-or-4 ok-predicate; no alignment requirement = NOTES; m-in-range undef companion); StringOrBigstringLoad (LE assembly/decode_saw per taxonomy incl. signed wraps and f32_of_bits; String tag NOT immutable — any mu, matching the doc's CSE caveat and my string-load memory; misaligned-aligned-variant undef NONDETERMINISTIC, disclosed — model has no byte addresses); PhysEqual (same-form arms correct incl. V_clos slot+location; cross-form ptr/clos disclosed to ch. 17; out-of-kind arm = KF-039); BigarrayLoad (+complex pair-read allocating an Immutable FloatBlock, halves at 2j/2j+1 per catalog 9; decode/encode taxonomy verified both directions incl. f16 store-time rounding ENCODING NOTE and float32 vs float32_t); BigarrayGetAlignment (power-of-two premise; (data_ptr+j) land (n-1) exact; data_ptr Parameter sanctioned); ReadOffset/WriteOffset per ruling (kind-constrained ok + overlapping undef; wok_base_ok = wk table); ArraySet (Mutable + splice + unit; out-of-range and immutable undef companions, immutable shared with .Vector per its NOTES); ArraySet.Vector (forward-packing existential ws — unrepresentable b relates to nothing, disclosed; product excluded entirely, no 2/4 exception = doc); BytesOrBigstringSet (encode_saw kinds follow widths; Bytes requires Mutable; misaligned nondeterministic as loads); BigarraySet (+complex store dual); ControlBarriers effects table row-for-row vs doc :1528-1531 (Optimised_out = EC_pure, Dls/Tls/Domain = EC_read matching StateAccessors' classification) with the effects_of_nullary_agrees anti-drift Qed against Curry's total effects_of — verified proved; data clauses Poll/CpuRelax unit + Invalid undef (Cpu_relax's unit is doc-silent, comment discloses the sibling reading; Probe/Enter/Optimised_out stuck per "no data denotation"); all 9 anchors quote their rules faithfully (Poke/Peek pinned quadruples, atomic variants incl. compare_exchange's two field kinds, WideAccess's shrunken bound + bigstring-only alignment check, FrontendInsertsChecks' wrapper shape + check_zero_division). Low notes (to Girard): (N1) width-invariant caveat — value payloads are raw Z (Values.v grammar), so GetHeader's hdr and ReadOffset's payload admit out-of-width values, and ArraySet.Vector's forward-packing pre-image is unique only up to width; suggest in_width bounds on the two nondeterministic results; systemic cousin of KF-005's dropped conformance premise. (N2) BytesOrBigstringSet's immutable-bytes explicit undef clause vs the policy's stuck treatment — behaviorally identical in the composed model (both CMO_undef via OS.Unit.Final), consistency nit only. (N3) the Bigarray.Indexing anchor trims two doc sentences (caml_ba_float32_get_N/_set_N naming; box/tag-per-element_kind around the primitive) — descriptive-anchor summarization, one sentence each restores full citation. |
| Cmm.v mention predicates + ToCmmControl.v KF-037/ExnWrapper + ToCmmData.v KF-038 + PrimMemoryB.v KF-039/N1-N3 (the fix-landing round) | FIX VERIFICATION (targeted; files ride Hopper's Cmm.v-batch cascade) | Cmm.v:294-382 (cmm_mentions/cmm_mentions_label + helpers); ToCmmControl.v:243-283 (kernel premises), :491-555 (tc_exn_wrapper), :762-766 (TC_Apply threading); ToCmmData.v:1119-1152 (TC_Let_Subst); 16-to-cmm-control.md:308-341 (re-applied doc rule); to_cmm_expr.ml:499-570/:579-607; cmm_helpers.ml raise_prim :4064-4067, trywith :4691-4700; trap_apply Cmm.v:624-634; CmmMemory.v:164-221 (region rules); PrimMemoryB.v:807-831/:583/:908 | KF-037 RESOLVED-pending-compile (premises exact; counterexample blocked). KF-038's chi channel RESOLVED-pending-compile — but re-checking the FULL statement found the TT'/RR' generalization still refutable (Pop is a top-matching read; CM_Region_End pattern-matches RR; Begin can re-pick a popped handle): NEW KF-043 (high, Admitted-false family; MY ERRATUM — I had certified those channels safe). KF-033 RESOLVED-pending-compile (DELTA 4: both doc corrections applied; constructor faithful line-by-line incl. gate-inside-raise_prim and is_cold; x_exn guard verdict: right, and the only rule-minted local hazard). KF-039 + N1(ruled (c))/N2/N3 CLOSED, compiled green. NEW KF-042 (medium): simples_translate's skip row reaches both raise-operand sites where nothing re-fixes arity (my ToCmmControl green-review erratum); length-premise fix proposed. NEW W-27: ch. 20's freshness hypothesis must also cover env-derived image mutual binder-disjointness (systemic, not ExnWrapper-local). |
| ToCmmData.v KF-040 disclosure + Syntax.v static_consts_in + KF-041 respin design (the KF-040 close-out round) | FIX VERIFICATION (KF-040) + PRE-LANDING DESIGN REVIEW (KF-041) | ToCmmData.v:1270-1288 (ov_hole disclosure); Syntax.v:1145-1162 (sanctioned-Parameter section); CORRESPONDENCE.md Binders paragraph (main's amendment); Milner's respin design message | KF-040 model side RESOLVED per main's option-(b) ruling: disclosure verified complete (ops-word clobber for the custom four, +8 payload per R.Obj, latent/fexpr-reachable, flip-on-fix local); carve-out device = static_consts_in Parameter (Church, Syntax.v:1162, interface-only, verified); consumption in the ch. 20 Simulates header still to verify at the ToCmmSoundness.v review. Compiler-bug finding stays open upstream. KF-041 respin design CONCURRED (symmetric mention disjointness + sl_mentions_label twin close all three quadrants; M/TT/RR pinned by license per my check (b)) with two construction flags: sl_mentions_label and sl_mentions/sl_binds must be built by folding Cmm.v's cmm_mentions/_label over frame-internal expressions — hand-rolled subsets would miss trap-action label mentions (quadrant iii) and frame-internal binders (quadrant ii). Church's emission-pass split coordinated (my dispositioned-flag list sent; tc_prim bodies all his). |
| Machine.v (instantiation file, wave 4; GREEN) | GREEN REVIEW | 04-opsem.md §3 (:259-282, the Pure/Effect classification premises); Opsem.v Section Variables; PrimScalar/PrimMemoryA/PrimMemoryB denotation ranges (operator-head census + effects_of cross-check); Soundness.v:499-529 (table quantification); CORRESPONDENCE.md entries 20/21 + :393-395 | CLEAN — no findings, W-20 RESOLVED (see entry). Header's "NO rule ids" claim verified (grep: none; file only instantiates — correct, since every rule is transcribed at its defining site). denot_prim union: disjoint-operator-range claim verified by operator-constructor census — scalar (arith/comp/shift/conv/box-unbox/tag-untag/bool-not/reinterpret) vs mem_a (duplicate/lazy/opaque/int_as_pointer/obj_dup/make_array/make_block) vs mem_b (loads/stores/projections/tests/lengths/phys_equal/read-write_offset/nullaries) share no operator head; PrimMemoryA's string-load hits are inside effects_of (classification covers ALL ops there by design), not denotation constructors. pure_effects matches 04-opsem.md:261 exactly (No_effects | Only_generative_effects → Pure; Arbitrary → Effect), fl_pure/fl_effectful complementary by construction (Opsem.v needs no exhaustiveness axiom), and the site comment correctly repeats the doc's NOTES nuance (classification not "heap unchanged"; allocators on the Pure side; region-closers Arbitrary). cextern Parameter = catalog-20 sanction (KF-002's value-keying disclosed there). fl_unit_behavior = initial ∘ has_behavior with rho_pre/H0 ambient — exactly ch. 13's quantification shape (Soundness.v:520). Eff_flags threading via Section MACHINE matches effects_of's signature. Routed to main: one catalog-wording residue (:393-395 "instantiated later" overpromises for the WF tables — they are universally quantified at ch. 13, never instantiated; W-20's resolution text has the suggested rewording). |
| ToCmmSoundness.v KF-041/KF-040-carve-out + ToCmmControl.v KF-042 + Cmm.v/ToCmmData.v KF-043 + Church's CF-2/CF-6 doc lane (the second fix-landing round) | FIX VERIFICATION + THREE-WAY DOC ARBITRATION | ToCmmSoundness.v:258-342 (sl predicates + Sink_intro + witness comment), :464-578 (carve-out + Simulates); ToCmmControl.v:528-559/:748-768 (length premises); Cmm.v:384-429 (cmm_touches_stacks), :1155-1197 (catch rules), :1218-1275 (apply rules), :1349-1372 (CM_Raise), :1408-1427 (cm_returns/cm_escapes); ToCmmData.v:1119-1197; 18-to-cmm-data.md:281-313/:408-429; to_cmm_primitive.ml array_load :323-355, block_set :175-202; cmm_helpers.ml assignment_kind :4120-4131, setfield :4133-4154, setfield_computed :4214-4221 | KF-041 RESOLVED, green (all quadrants closed; witness transcription faithful; construction flags satisfied; no residual — W-26 CLOSED). KF-040 carve-out CONSUMED and verified (custom_boxed_var_payload + static_consts_in premise on Simulates) — model side fully closed. KF-042 RESOLVED pending compile (length premises at both raise sites + ENCODING NOTE clause, exactly as proposed). KF-043's guard blocks both FILED witnesses (transcriptions faithful incl. label-free-RR note) BUT the full TT/RR read inventory shows the guard under-inclusive: NEW KF-044 (high, Admitted-false family, MY ERRATUM #2 — the raise exemption was my own proposal's wording): direct Craise reads TT top + chi at an unmentioned label; Capply tunnels both channels (cm_returns hands the callee the caller's RR and demands it back; CM_Apply_Raise re-plugs a raise at the call site); fix = flag Craise + Capply, still LPE-vacuous. W-27 design verdict on cmm_unique_binders: closes all name-based channels IFF the spec pins both Barendregt halves in both namespaces; non-name channels correctly out of scope. Church's CF-2 → NEW KF-045 (DOC finding: TC.Prim.ArrayAccess's normative `mut` vs array_load's unconditional Mutable; .v faithful to divergent doc); CF-6 → writer-side, NOT doc (doc's flagless "plain Cstore(Word_int)" matches setfield_computed exactly; .v's cmm_init ia over-refines; block_set routes static fields through setfield_computed so the drop is unconditional). |
| Cmm.v cmm_unique_binders + ToCmmData.v CF-1 landings (the W-27 landing round; crossed-traffic reconcile with Milner/Church) | LANDING VERIFICATION | Cmm.v:431-443 (Parameter + spec comment); ToCmmSoundness.v:553-591 (Simulates incl. :575 premise); ToCmmData.v:399-435 (block_set_image + CF-1/CF-5 note), :620-649 (array_set_image) | W-27 CLOSED: both spec conditions satisfied at the landing (both Barendregt halves, both namespaces, hosted beside the mention family; premise live on fd_body fd at :575); Milner's sink-preserves-binder-multiset covering note CONCURRED with the scope-narrowing half made explicit. CF-1 fix verified at both image sites (three-argument caml_modify_local: block, untagged word index, value; array path untags the tagged index — matches addr_array_set_local). CF-10-as-note CONCURRED (rule text states no arity premise; an es <> [] premise would over-refine vs the doc; the nonempty assert is a code-reachability question, not a fidelity one — if empty Make_block is doc-permitted and code-fatal that is a separate DOC/code question, currently below threshold since flambda2 lowers zero-field variants as immediates). KF-044 NOT yet landed (Cop row still Cendregion-only at Cmm.v:421; wrong raise-exemption sentence still at :395-397) — message crossed with Milner's; re-flagged. CF-6 ruling relayed to Milner directly (held row ToCmmData.v:423 `cmm_init ia` → constant Assignment). |
| ToCmmSoundness.v s1/s2 (partial green review — the file's s3 increment is not yet written) | GREEN REVIEW (first half of task #25's agenda) | 20-to-cmm-soundness.md:1-78 (s1 + INV.ToCmm.Simulates); ToCmmSoundness.v:1-462 (header obligations, event correspondence, sunk closure, tocmm_beh_rel); Cmm.v:951-979 (cm_return_config/cm_uncaught_config), :1443-1447 (cm_unit_final_config), :1517-1578 (cm_outcome + cm_unit_behaves) | s1 correspondence FAITHFUL: rep_event/rep_trace_from/rep_stream_from are the heterogeneous image of Soundness.v's KF-015 cumulative-seed shape (retained_pairs = args+results; raise row totality-only, disclosed — no raising-external correspondence, narrowing inherited from ch. 15's CM.Extcall, candid); callee_name value-to-name hook quantified with coordinator flag, same treatment as ch. 16's hooks. Closure section delivers the W-26 discharge note verbatim as booked (validity structural in sl_ctx; drop = LPE_drop not re-represented; flush placement = the closure; sunk = refl-trans congruence closure over sink_step with per-node congruence rules, handler bodies included, labels/params fixed). tc_expr_data_sunk = exists e_c0, bare tc THEN sunk — header obligation 1 delivered. tocmm_beh_rel: all five constructors match the doc's five bullets (see W-25 RESOLVED for the uncaught/trace analysis; TBR_normal's rep_heap+rep_observe pairing is the Representation.v-anticipated design, L existential and L-domain-indexed so no over-claim; TBR_exhaust's b <> Beh_undef matches "diverges or terminates"). W-25 RESOLVED, no finding. Census: header's "all 12 rules" matches the doc (Simulates + EndToEnd + InvalidUnreached + nine discharge rules); file currently ends at Simulates/Admitted (:597) — s3's 11 rules are a PENDING INCREMENT (task #25 in_progress), re-review fires when they land. Remaining agenda for the s3 round: KF-033 wrapper seams, INV.ToCmm.Control call-step residual, item 68 statement check. |
| ToCmmSoundness.v s3 (inc 4: EndToEnd + InvalidUnreached + nine discharging invariants; completes task #25's review agenda) | GREEN REVIEW (second half of task #25; census now 12/12) | 20-to-cmm-soundness.md:86-529 (EndToEnd :86-111, InvalidUnreached :113-143, the nine discharge rules :156-529); ToCmmSoundness.v:599-1119; Soundness.v:512-529 (Preserves premise block), :327-385 (beh_sim/obs_equiv/no_ub); Machine.v:101-113 (fl_unit_behavior unfolding); Simplify.v:389-401 (SU_intro); ToCmmData.v:1289-1299 (machtype_of_kind_data); Cmm.v:93 (MC_addr); CORRESPONDENCE item 68 | CLEAN at the theorem level. INV_ToCmm_EndToEnd: premise-SET inheritance verified VERBATIM — Preserves' six premises on U0 (expr_wf + code0_wf + simplify_unit + consistent + heap-code tie + no_ub) union Simulates' ten on U' (cp_funs..loc_map_inj incl. cmm_unique_binders and the KF-040 carve-out); Simulates' per-behavior premises correctly re-emerge as conclusion existentials (initial U' + fl_has_behavior = fl_unit_behavior unfolded, c0' exposed to seed RR — verified against Machine.v:109); beh_sim (fu_module_symbol U0) b0 b' is exactly obs_equiv's first-half instance; the two-module-symbol seam (beh_sim at sym U0, tocmm_beh_rel at sym U') is CLOSED BY CONSTRUCTION — SU_intro pins all non-body fields, so fu_module_symbol U' = fu_module_symbol U0 by inversion. FALSE-statement transcription (doc's own int→float32 NOTES) accepted under the Simulates s5.6 precedent: doc-rooted, disclosed at the site, and grep confirms NO file consumes any ch. 20 theorem (leaf; Admitted never used as an axiom). Ruling on Milner's press-question: the poison-rule concern is about statements refutable by hostile derivations the doc believes true (KF-037 class); transcribing a doc-acknowledged counterexample verbatim with the reading instruction quoted is a different category — no extra demand. INV_ToCmm_InvalidUnreached: no_ub = doc premise (1) (whole-set form, matching the doc's unit-level phrasing); slot-liveness premise absent + disclosed, and the disclosure's ground claim VERIFIED (zero `Dead` hits in ToCmmData.v — site-classes (c)/(d) genuinely unmodeled); Simulates setup block inherited exactly (premises 2-11 = Simulates 1-10); the CMO_undef-union conclusion is a disclosed honest STRENGTHENING (bigger refutation surface; the non-Cinvalid half is what Simulates' outcome list already presupposes) — accepted; carrying cmm_unique_binders + the carve-out on a NEGATIVE Admitted statement is right (without them the KF-037 flat-env class refutes it). Eight anchors + AddrConfined hybrid: catalog-37 decision rule correctly applied (all eight quantify over pass internals — checked each against its doc NOTES); sharp edges preserved (CallConvCoherent leg-(2) UNDISCHARGED flag, SymbolInitPlacement's recursive-handler drop soft spot, StaticUpdateBarrier's barrier-invisible-to-layout-validation warning, SlotLiveness's P1-accidental + merge face). Envelope lemma machtype_of_kind_data_no_addr: subject verified (no MC_addr in any arm of ToCmmData.v:1289-1299; four-constructor kind destruct sound), non-vacuous (MC_addr exists, Cmm.v:93), Qed. Census 12/12 header claim now true. Agenda items CLOSED: KF-033 wrapper seams — no statement-level exposure (wrapper interior to tc derivations; InvalidUnreached's negative conclusion is now the sentinel that would catch any residual extras-stranding); INV.ToCmm.Control call-step residual — absorbed by the whole-run statement shape (the ch. 16 big-step mismatch is interior to the Admitted obligation; nothing at ch. 20's statement re-exposes it); item 68 — matches disk. Findings: KF-047 (low, comment-level) — two anchor sharp-edge omissions. Process note to Milner/main: CORRESPONDENCE ends at item 69; inc 4's deviations (premise-set inheritance, the CMO_undef strengthening + site-class disclosure, the envelope-Qed hybrid pattern) need catalog entries. |
| SimplifyStructure.v (all 37 ch. 09 ids; GREEN — .glob on disk) | GREEN REVIEW (from the idle draft queue) | 09-simplify-structure.md (whole chapter, 37-rule census list :1150-1160); SimplifyStructure.v (whole file, 884 lines); Simplify.v:376-401 (SU_intro consumption), :216; simplify.ml#run :30-84 (code ground truth for ClosedResult/NoPendingConstants); CORRESPONDENCE items 37/38, :723-724 | Census EXACT: 37 headers = 37 doc ids, doc order, every STATUS transcribed correctly (incl. Loopify's VERIFIED line). The sanctioned status-mapping deviation (5 real artifacts, 32 anchors, catalog item 37) is executed cleanly: every anchor keeps its true STATUS in the header and states its model-side residue or its "algorithm-only" rationale; spot-checked anchors (TypesMonotoneDown's split, EnvRefineOnly's (a)/(b) halves, Switch.ArmIsolation, CSE.JoinPhi, Flow.DeadLoopParam's six blockers, Flow.ExnFirstParam's inversion + the RewritesControl [exn = true -> j <> 0] premise cross-ref, LiftCont.Gate, SpeculativeSandbox) all transcribe their doc content faithfully including caveats. all_statics_placed verified as a correct implementation of catalog 38's residue: lambdas = SCC_code bodies (the only body-bearing form), nested static groups count as hits, the mutual fixpoint covers every binder form. S_Struct_JoinParams' column machinery coherent (row-length premise makes column j total for valid j). Findings: KF-046 (low, three parts): (a) join_params/rec_invariant_entry claim consumers that do not exist anywhere in the corpus — question to writer whether chs. 10/12 were supposed to lean on them; (b) ClosedResult widens the code's my_region-only check to include the ghost region with an underdisclosing comment (code verified: simplify.ml:55 fatal-errors on the ghost region); (c) EmptyAtEnd's comment claims "toplevel positions only" where the predicate delivers "not under a lambda". No soundness exposure; file otherwise faithful. |
| Cmm.v/ToCmmData.v KF-044 + CF-6 fix batch (Hopper-greened) + SimplifyStructure.v KF-046 comment fixes (the third fix-landing round) | FIX VERIFICATION (KF-044, CF-6, KF-046) + COMPLETENESS RE-SWEEP | Cmm.v:388-440 (guard + corrected comment), :1298-1304 (CM_Extcall); ToCmmData.v:1168-1218 (ENCODING NOTE + rule header + TC_Let_Subst statement), :399-440 (block_set_image CF-6); CmmMemory.v:107-280 (alloc/region/GC rule inventory — the exemption re-sweep); SimplifyStructure.v :14-15/:155-158/:419/:505-539/:522-523/:788-813; RewritesControl.v (join-consumption grep) | KF-044 RESOLVED: guard flags Cendregion/Craise/Capply; all three witness channels hit a flagged head; both erratum comments struck and rewritten correctly; Cextcall-silence answer VERIFIED (CM_Extcall emits CME_extern unconditionally — trace-silence excludes extern steps). TC_Let_Subst's Admitted-false status CLEARED (the KF-037/038/041/043/044 family's last open member on this rule). BUT the writer's completeness sweep mis-stated one exemption: CM_Alloc_Local pattern-matches RR = iota :: RR0 — a silent RR-read passing the guard — so "ch. 19 heads touch only M" is false; the statement stays safe via M-pinning + allocation monotonicity (verified: mem_alloc strictly extends M with fresh addresses; the ONLY M-shrinking op is mem_reclaim behind the flagged Cendregion; CM_Alloc_GC contributes no provable steps since gc_reloc is an unconstrained Parameter). Filed KF-048 (low): add Calloc to the guard comment's exemption inventory with the true argument, and add a tripwire sentence at gc_reloc's "constrainable later" note — a future unreachable-drop axiom re-opens the channel via the replica witness (W-31 tracks). CF-6 fix VERIFIED (immediate-field store = constant Assignment at ToCmmData.v:429; ENCODING NOTE's CF-6 clause states the int_const/setfield_computed/int_array_set justification — matches my ruling exactly). KF-046 RESOLVED all three parts (comments on disk; ch. 12 answered by Girard — Unboxing.v quantifies T directly by design; ch. 10 closed by my grep — RewritesControl.v has no join machinery, same pattern; wave-4 expectations never cashed, rewording correct). CF-2/KF-045 still holds pending main's doc ruling. |
| ToCmmSoundness.v KF-047 comment round (Hopper targeted-compile green) | FIX VERIFICATION (KF-047) | ToCmmSoundness.v:796-819 (SlotLiveness anchor), :915-949 (EffectLinear anchor) | KF-047 RESOLVED: both sentences on disk and exact. (a) at_normal_mode qualifier at :803-807 — leg (a) totality now conditioned on "slot lookups reached only from normal-mode occurrences (the doc NOTES' at_normal_mode qualifier: INV.NameMode.Coherent plus the Phantom skip), the seam to the one mechanized invariant this rule composes with". (b) apply_expr Case-3 edge at :932-935 — the call ITSELF enters the delayed set, sound only because Arbitrary_effects is never dropped (zero occurrences still emits exactly once at the next flush). No drift in the surrounding anchor text. LAST open finding against ToCmmSoundness.v — task #25 (ch. 20) review trail fully CLEAN; writer marked the task completed. Remaining in-flight touching ch. 18-20 lanes: KF-048 comment asks (Milner), CF-2/KF-045 doc-first hold (main), inc-4 CORRESPONDENCE entries (main; re-flagged by Milner, catalog still ends at 69). |
| Cmm.v/CmmMemory.v KF-048 comment round (Hopper-greened, full -k chain) | FIX VERIFICATION (KF-048) | Cmm.v:388-412 (guard comment incl. new Calloc paragraph :404-411); CmmMemory.v:226-257 (gc_reloc ENCODING NOTE incl. new TRIPWIRE paragraph :249-257) | KF-048 RESOLVED: both asks exact. Calloc exemption carries the TRUE argument (CM.Alloc.Local reads RR; excluded by M-pinning + strict allocation monotonicity + mem_reclaim being the only M-shrinker behind flagged Cendregion + gc_reloc's Parameter status), replacing the wrong "allocs touch only M" sweep claim; TRIPWIRE at gc_reloc states the condition-(iv) provable-existence hazard, the address-recycling replica witness, the Admitted-false consequence, and the constrain-only-with-guard-revisit instruction, citing KF-048/W-31. W-31 stays OPEN as the watch (comment is its anchor). LEDGER STATE AGREED with Milner: KF-037/038/040-044/047/048 + W-25/26/27 all resolved on disk with compiles confirmed; the only open items in his column are riders on main — CF-2/KF-045 doc ruling and the CORRESPONDENCE applies (inc-4 entries + the Calloc/W-31 sentence for the KF-043+044 catalog entry; catalog still ends at 69). |
| ToCmmData.v green review, MY HALF (four inc-2 dispositions re-checked + W-17 closed; Church holds the 20 tc_prim bodies) | GREEN REVIEW (partial — split per the coordinated boundary) | ToCmmData.v:512-536 (saw_chunk + note), :596-623 (array_indexing_image + note), :924-968 (Vector rows + note), :970-998 (Bigarray rows + note), :692-701 (W-17 scope note), :704-1049 (rule-header census); 18-to-cmm-data.md:385-408 (StringLoad), :432-454 (ArrayAccess.Vector incl. NOTES :453-454), :458-487 (BigarrayAccess incl. NOTES :477-480), full-chapter atomic grep; 15-cmm.md:72/:313 (is_atomic); 16-to-cmm-control.md atomic grep; Syntax.v:481-500 (atomic prim families) | ALL FOUR inc-2 dispositions HOLD on the greened file, "Reviewer-confirmed (Knuth)" stamps accurate: (1) array_indexing generic row keeps the direct untag-then-scale form; the ENCODING NOTE's equality claim re-verified arithmetically (i = 2n+1: code's arr + (i << (k-1)) - 2^(k-1) = arr + n*2^k = direct form) and the doc indeed states only the two folds + contract; fold/generic overlap = harmless nondeterminism as noted. (2) Vector rows: unboxed-product exclusion via array_elem_width = None is an honest strictly-narrower model; the vec128-from-2/4-vec128-product exception is NOTES-level in the doc (:453-454 verbatim), above-the-line form covers only Naked_vecNs; constructors correctly use untag+lsl directly (doc :449-450, NOT array_indexing) with chunk from LOAD/SET kind and width from SOURCE kind, always unaligned. (3) Bigarray rows: ba_chunk = None excludes float16 (Sixteen_unsigned + float_of_float16) and complex kinds; complex correctly flagged MATERIALLY different (two loads + box_complex ALLOCATION, the 06 generative effect) — both NOTES-level (:477-480); rule form is the single chunked load at array_indexing over the field-1 data pointer, tagged offset, as the doc's above-the-line states. (4) StringLoad: SAW_thirty_two = Thirtytwo_signed fold of unaligned_load_32 + sign-extend, value-level equality per R.Val.NakedNumber sign extension; 16-bit signed parallel (CF-8) noted at the same site. W-17 CLOSED (see watch item — scope note accurate in all three clauses; 14 doc rules = 14 tc_prim headers 1:1, atomics genuinely unlowered doc-wide). REMAINING for the file's full green stamp: Church's half (the 20 tc_prim constructor bodies vs doc quoted shapes + code) and the quoted-emission-shape protocol boundary confirmation (asked; his reply pending). The inc-3 kernel/statics region (TC.Let.*, Subst, Sound) already carries full review coverage via the KF-035/036/038/040/042/043/044 arcs. |
| TypeGrammar.v vs ch. 07 §§1-3,5 (draft-queue green review; Concretization.v vs §4 next) | GREEN REVIEW (full file; census 10/10 rule ids) | 07-types-domain.md:1-660, :838-896; TypeGrammar.v:1-888 (whole file); MeetJoin.v head_nonempty consumption grep; CORRESPONDENCE items 13-15 + the descriptive/documented-anchor convention (:19-26) | GRAMMAR SIDE CLEAN: T.Grammar.TypeDescr exact (or_unknown_or_bottom over No_alias/Equals); Variant/RowLike.Index/blocks/closures constructor-for-constructor vs §2 (known_tags Or_unknown vs known_closures bare — the doc's asymmetry — both right; indices plain data; check_field_tys kind-fixing is NOTES-level, correctly comment-only); NakedImmediate.Relational's three constructors exact with construction/reducer behaviors correctly deferred to MeetJoin.v; NonEmptySet's head_nonempty is genuinely CONSUMED (13 premise sites in MeetJoin.v — invariant enforced where the code exercises it); RecInfoRegion.Trivial exact; Disjunction.Extensions matches §5 (No_extensions/Ext + per-case env_ext). tmentions occurrence relation checked channel-by-channel against the grammar — complete (alias simples, is_null var, is_int/get_tag vars, immediates/blocks/extensions, boxed payloads, closures entries incl. function_type rec_info, array length+contents, env_ext dom+rng; naked-number/Rec_info/Region heads correctly nameless/opaque per catalog 15). ENV SIDE: Find.Bottom/Find.SymbolDefault/Find.Canonical/Equation.Closed/Scope.Existential faithful (tenv_find's totalization onto missing-cmx defaults harmless; alias-at-constant reading disclosed); cut_as_extension Parameter is catalog-14-sanctioned; SinglePass + ConcreteOnCanonical documented anchors per the descriptive convention. ONE HIGH FINDING: KF-049 — both conjectured-rule Admitted theorems (ConstCanonicalPersists, AliasesAuthoritative) are refutable under tenv_wf as defined (2-cycle te_canonical refutes idempotence; a const canonicalized to a different const refutes persistence); tenv_wf omits path-compression and const-self-canonicality, the exact invariants CORRESPONDENCE 14 already attributes to te_canonical. Fix = two wf conjuncts, both theorems likely upgrade to Qed. LOW RIDER (comment-only): NoEqualsOnCanonical's rule comment quotes the doc's dual sentence ("non-canonical names carry Equals equations resolving toward the canonical") which the Prop deliberately does not encode — one disclosure clause wanted. DELTA 2026-07-18: KF-049 RESOLVED — fix landed exactly as proposed and Hopper-greened (12-file sweep clean); verified on disk (tenv_wf :824-831 with both conjuncts + KF-049 citation :817-823; both theorems Qed :851-863/:882-889 with statements untouched; rider disclosure :791-795; Admitted census 2 -> 0). File now FULLY GREEN with zero open findings. |
| Concretization.v vs ch. 07 §4 + §5 (draft-queue green review; completes the ch. 07 pair, census 12/12 rule ids) | GREEN REVIEW (full file) | 07-types-domain.md:646-875 (all twelve T.Gamma.* rules + §5); Concretization.v:1-737 (whole file); Values.v:357-366 (simple_eval); MeetJoin.v gamma-consumption grep (:897-1814) + code-age grep (:1017-1073); 08-meet-join.md Both_inputs grep | ELEVEN of twelve rules faithful: TopBottom (Unknown clause + constructor-absence for Bottom + gamma_bottom_empty Qed on the nose); Alias (coercion-erasure disclosed, simple_eval = the doc's rho(s)); Nullability (both arms, null_witness on BOTH as the doc requires, gamma_vnn excludes V_null from the non-null arm in every constructor); Variant (untagged-membership reading exact, is_int 1/0 per arm, get_tag on blocks only, per-arm sat_ext, is_unique gamma-inert); RowLikeBlocks (known/known-top/other trichotomy right; per-shape object matching with the float-block tag-key looseness DISCLOSED at :89-93; size admission on the stored Block_size rather than |Ts| — disclosed at :152-156, the code-shaped reading); Boxed/Array/String/Mutable_block (heap-object forms right; array length as V_tagged_imm matches the code and the doc's "in practice" hedge; Bottom element kind = empty array, disclosed choice); Closures (slot containment at-least per the doc's own phrasing — exactly-vs-at-least non-sharpening disclosed :164-169; components + extension exact); CodeAgeLoose (Qed by construction — loose reading internal to function_type_code_ok; either-direction class = preorder symmetrization, right); Naked.Set (per-kind constructors); Naked.Relational (unified hni_view pair, relation_denotes exact incl. Get_tag only on block ptrs); EnvExtension (sat_ext at fixed E — the gamma-reads-E-only-through-te_code_age argument at :273-281 VERIFIED clause-by-clause, so the doc's gamma_(E u eps) reading and the SUBSETEQ claim are genuinely definitional here). consistent + gamma_set match the §4 preamble. ONE HIGH FINDING: KF-050 — T_Gamma_Kind (Admitted) is refutable: G_equals is the sole kind-crossing channel (alias at kind K to a const/symbol/name of a different kind; witnessed with empty env + a tagged-immediate const under FT_naked_float Equals). NOT a leaf — MeetJoin.v's whole rule set quantifies over gamma. Fix: one kind-agreement premise on G_equals, theorem then Qed. RIDER (low): stale cross-file comments — Concretization.v:681-683 and TypeGrammar.v:501-504 both point the meet_code_id/Both_inputs content at MeetJoin.v, which models no meet_code_id at all (documented-unmodeled, te_code_age constant through descent). With this row the ch. 07 pair is fully reviewed; draft queue EMPTY. DELTA 2026-07-18: KF-050 RESOLVED — fix landed exactly as proposed and Hopper-greened (12-file sweep clean, no downstream edits); verified on disk (G_equals premise :310 + KF-050 NOTE :300-306; T_Gamma_Kind Qed :671-686 with the unconsumed-consistent disclosure; Admitted census 1 -> 0; both meet_code_id rider sites fixed). File now FULLY GREEN with zero open findings; the ch. 07 pair (this row + row 68) closed at zero Admitted across both files. |
| MeetJoin.v alias-channel refutability re-sweep (committed at KF-050; run against the on-disk KF-050 fix, ahead of Hopper's green since the repaired G_equals is the sweep's input) | TARGETED RE-SWEEP (9 gamma-quantified Admitted statements; not a re-review of the row-48 green) | MeetJoin.v :893-1770 (all nine statements + meet/join/prover/expand/reify constructors :55-800), can_alias :59-63, stored_type :92-96, record_on :100-105, alias_res :160-165, alias_alias_res :170-177, M_alias_alias :631-637; Concretization.v consistent :649-655 + sat_ext :623-628 + repaired G_equals :307-311; Values.v simple_eval :357-366 (symbol fallback); TypeGrammar.v name_bound_in :706-708, tenv_find :728-745 | THREE REFUTABLE ADMITTED STATEMENTS FOUND (each a new HIGH; entries below): KF-051 — T_Meet_Sound's sat_ext conjunct is refutable via the simple_eval symbol fallback vs sat_ext's strict rho-binding (self-alias meet at an unstored symbol under the EMPTY env: gamma premises hold via the fallback, the recorded eps equation demands a rho binding that consistency never forces); upgrades Plotkin's P2 from "sound direction" to refutation-grade — the :626-630 disclosure's soundness claim is false. KF-052 — T_Expand_Head states the doc's SET-form gamma equality at a FIXED rho; the backward inclusion is false (expansion discards the alias's singleton constraint; witness = alias to an unstored var, U = unknown, any value of the kind); MY ERRATUM #4 — row 48 called T.Expand.Head faithful in my slice. KF-053 — T_Meet_GreatestLowerBound is Admitted while its own comment (:923-925) discloses it refutable (MutableBlockMissedBottom, Qed at :972, is the witness; result gamma disjoint from the left input's): a knowingly-false universal kept as an axiom, the KF-037 poison class verbatim. SIX statements CLEAN through the channel — T_Meet_Bottom (consistent's stored-equation clause is exactly the guard: stored-Bottom or disjoint-stored scenarios force premise-unsatisfiability), T_Join_Sound (no extension; alias branches ride stored types, unstored default to unknown which over-approximates), T_Prove_Sound / T_Prove_MeetShortcut / T_Prove_GetTag (provers read stored types; the fallback needs an rho-unbound symbol, but proving anything needs a stored type, which forces the rho binding via consistent), T_Reify_Sound (R_simple conclusions bridge via consistent's canonical-invariance clause; symbol fallback makes simple_eval total in the right direction). Also noted: the same fallback-vs-strict-binding mismatch sits benignly in consistent's clause 3 (a stored equation on an rho-unbound symbol makes E rho-inconsistent rather than unsound) — flagged in KF-051 for the writer's fix decision. Forward half of T_Expand_Head is provable and USES the KF-050 kind premise (unstored-canonical case lands in unknown via G_unknown). CORRECTION 2026-07-18 (MY ERRATUM #5): the provability claim was FALSE as stated — Scott found a residual refutable corner (unstored SYMBOL canonical at a NON-VALUE kind: tenv_find_default's symbol arm returns any_value ignoring the requested kind); provable only after consistent gained its symbol-kind clause 5. Details in the KF-052 entry. |
| ToCmmData.v FULL GREEN STAMP (both halves complete; CODE-anchor edge closed) | GREEN REVIEW (declaration — completes the split review) | ToCmmData.v CODE anchor lines (:126-127, :705-707, :721-725, :744, :754-755, :782-784, :797-798, :811-812, :825-826, :842-844, :857-858, :890-892, :926-928, :972-974, :1022-1023, :1052-1054, :1192-1195, :1545-1546, :1557-1558, :1576, :1650-1651); 18-to-cmm-data.md CODE lines (:25-26 through :492-493); Church's boundary reply (CF-1..CF-10 all dispositioned; CF-6 fix re-verified on disk :429 with site note :413-418; CF-1 3-arg caml_modify_local sites unchanged through the respin) | ToCmmData.v review COMPLETE, file GREEN-STAMPED. Church's half done: code-vs-.v on the section-3 images and all 20 tc_prim bodies = the CF-1..CF-10 arc, all dispositioned, with the green batch's CF-6 delta re-verified on current disk (block_set_image immediate branch = Cop (Cstore Word_int Assignment), unconditional drop correctly noted). My half done previous round (four inc-2 dispositions + W-17 + census). Boundary edge Church named — RULE-header CODE anchors (doc-metadata, my lane) — CLOSED with no discrepancy: all anchors match the doc's CODE lines 1:1 VERBATIM across every rule region (14 primitive rows doc :192-493 = .v :705-1049 region; section-2 kernel rules :25-115 = .v :126-127/:1052-1054/:1192-1195; section-4 statics :75-173 = .v :1545-1651), so anchor drift is impossible without doc drift, and every function an anchor names is on Church's byte-checked list (array_indexing, box_number, setfield_computed, array_set0, bigarray_load_or_store, string_like_load, make_alloc_generic, ...). HELD OUT (both halves, by agreement): the ArrayAccess row's `mut` premise — rides main's KF-045 doc ruling (W-30 watches ordering; Church's CF-2 follow-up rides it). With #25 completed, All.v (#26) is the only task left; this stamp was the last file-level gate before the finale. DELTA (2026-07-18, crossed traffic): the stamp declaration crossed Church's final sweep result — 13/14 doc-quoted shapes clean plus ONE new finding, CF-11 (medium, writer-side): TC_Prim_ArrayAccess_Vector_Load emitted `cmm_mut mu`, but array_load_vector takes no mutability parameter and bottoms out in load_chunk's mk_load_mut = always Mutable, and the doc's .Vector rule quotes the helper without routing mu — .v-vs-both per the CF-2/CF-6 taxonomy, no doc dependency. Fix landed and Hopper-greened (chain through ToCmmSoundness.v, zero warnings); VERIFIED ON DISK by me: constant CMut_mutable at :957, CF-11 note clause at :944-949 (correctly noting the scalar row keeps cmm_mut mu pending KF-045), scalar row unchanged at :911. STAMP RE-AFFIRMED post-CF-11 with the same single hold-out (KF-045's scalar row). MY ERRATUM (#3, minor): my inc-2 disposition re-check of the Vector rows verified index shape, chunk source, and width source but not the mutability token — the mu-vs-doc-silence divergence was visible doc-vs-.v (the doc's quoted helper carries no mutability) and I missed it; Church's quoted-shapes sweep caught it. CODE-anchor closure unaffected (CF-11 is a constructor-body field, not an anchor). KF-045 entry gains a CF-11 rider: the doc fix should add a parallel NOTES sentence to .Vector ("mu does not reach the load"). |
| All.v (task #26) + finale Print Assumptions | GATE REVIEW (2026-07-22; definition-of-done items 1-4) | All.v (on disk, 26 imports); Hopper's verbatim Print Assumptions log (~/.claude/plans/flambda2-rocq-print-assumptions.log, both closures read in full); theories/ grep sweeps run independently (naked-Axiom grep, RULE-token grep on All.v, unique id count, unique id-status pair split, chapter-side id extraction, comm set-difference); CORRESPONDENCE.md sanctioned-Parameter cites (:263 cextern_c, :288 cextern_rel entry 20, :391 f16_round, :429/:836 prim_arg_kinds + prim_transfer, :657-664 the layout oracles); Representation.v Parameter inventory | PASS ON ALL GATES. (1) INV_Simplify_Preserves closure: static_consts_in and cmm_unique_binders both ABSENT as required; members are exactly the sanctioned flambda-side set — Unbox?/Inline?/abstract-transfer oracles (unbox_dec, inline_dec, prim_transfer, prim_arg_kinds), alpha predicates (free_vars/free_conts/bound_vars), the Base.v float32/float64 type+op family, f16_round, data_ptr, Machine.cextern (catalog entry 20), as_pointer; nothing else, no foreign Admitted (closures are statement-type-driven; both invariants are themselves Admitted, correctly the only theorem-constants present). (2) INV_ToCmm_Simulates closure: static_consts_in and cmm_unique_binders both PRESENT as required; plus the EIGHT layout oracles (all eight individually sanctioned at catalog :657-664 — the final report's "7 layout oracles" is a miscount to fix), header_is_local, gc_reloc (still an unconstrained Parameter — W-32 intact), cextern_c, Machine.cextern, floats/f16_round/data_ptr/as_pointer, free_vars; nothing unsanctioned. Statement shapes spot-verified against the catalog: Preserves = the entry-70-quoted premise set; Simulates carries tc_expr_data_sunk, the cmm_unique_binders premise (W-28), the KF-040 custom_boxed_var_payload carve-out, symaddr_agree/rep_heap/loc_map_inj, no-undef premise, exists-outcome + tocmm_beh_rel conclusion. (3) NO NAKED AXIOMS: grep over theories/ finds zero Axiom declarations. (4) All.v: imports all 26 content files, contains zero RULE tokens (no phantom ids; prose uses lowercase "rule"). (5) Coverage independently re-verified: 453 unique ids in theories/; unique id-status pairs exactly 303 normative / 66 descriptive / 84 conjectured; chapter-side extraction 453; symmetric set difference EMPTY (exact set equality). Counting caveat recorded: raw STATUS-occurrence grep reads 88 conjectured + noise ("STATUS preserved"/"STATUS marks" prose fragments, duplicate quotings) — unique-pair extraction is the correct method. FINAL FIDELITY STATEMENT (definition-of-done item 4): NO OPEN DISCREPANCY REPORTS. KF-001..KF-054 all resolved, accepted-as-cataloged, or closed by doc ruling; the Admitted-false family (KF-037/038/041/043/044/049/050/051/052/053) fully resolved on disk; KF-040's compiler-bug half and Finding #16's design conflict remain open UPSTREAM/USER-side only (final-report decision items, not mechanization discrepancies). |
| Final report Part 1 (findings narrative) + last two comment residues | REPORT PASS + LANDING VERIFICATION (2026-07-22, at main's request — the project's last unverified deltas) | ~/.claude/plans/flambda2-rocq-final-report.md (whole file); 04-opsem.md §1.4 diff; Opsem.v:677-697 + .vo mtime; per-file `^Admitted.` counts across all 13 Admitted-bearing files; TypeGrammar.v:916 (cut_as_extension); non-anchored RULE-token grep over all 20 chapters; ToCmmControl.v:209/:259-283 (TestByValue) | Residues BOTH VERIFIED: ch. 04 §1.4 parenthetical ("unless rebound by OS.Let.Static — see that rule's NOTES") is exactly the recommended resync, prose-only; Opsem.v:687-690 header comment now quotes the corrected conclusion (v_j with the per-match NOTES gloss), .vo rebuilt. Report Part 1: per-file Admitted distribution EXACT (sums to 32; MeetJoin 10, Opsem/Pilot 0 among the 14 Admitted-free); Parameter inventory real (cut_as_extension confirmed a Parameter); Print Assumptions bullet matches my certification; falsity/USER-DECISION items match my entries; Part 3 errata corrected to five with my classification. TWO FACTUAL ERRORS FILED (report-side, not mechanization): (1) the Qed-under-conjectured sentence lists TC_Switch_TestByValue among "six doc conjectures now machine-checked theorems" — ON DISK it is an ADMITTED kernel with NO rule id (ToCmmControl.v:259, Admitted :283; catalog-62 family; one of that file's two Admitted, already correctly counted in the report's own 32) — an inherited ledger slip (Dijkstra's 07-18 class listing predates the report's "machine-checked" rendering); true inventory is five members, and the doc-conjecture-theorems count is 4 rule upgrades + 3 envelope-Qeds. (2) The coverage bullet's "naive counting reads 454 on the doc side: a prose line in ch. 18's NOTES" does not reproduce: non-anchored unique-id grep over the twenty chapters reads 453, and no chapter contains a non-line-initial `RULE ` token — the phantom either predates a doc edit or used a different pattern; the sentence needs its method stated or removal. Plus one wording nit: Simulates' closure additions are not "exactly" the listed apparatus + two conditions — f32_of_Z is a 14th addition (float-family, reachable only Cmm-side). CLOSURE (2026-07-22, same session): all three fixes applied by main and RE-VERIFIED against disk — (1) the Qed sentence now headlines SEVEN doc conjectures discharged (4 rule upgrades + 3 envelope-Qeds), states the support inventory as five, and carries an explicit reconciliation note that TC_Switch_TestByValue is an Admitted kernel with no rule id; (2) the 454 parenthetical now states the reproducible method — and it REPRODUCES: 18-to-cmm-data.md:143 is a line-initial "STATUS conjectured —" prose line inside TC.Let.Subst's NOTES (the rule's real field is :111), so line-anchored STATUS extraction reads 454 while RULE-token greps read 453; (3) f32_of_Z listed as the 14th addition. Dijkstra's reconciliation (same session) confirms both filings: the class-membership slip was his pre-coma conflation of the support-theorem inventory with the Qed class (census figures unaffected — his own per-file counts had TestByValue right all along), and the 454 reproduces under his actual method (line-anchored RULE/STATUS pairing: ch. 18 has 22 ^STATUS lines against 21 rule fences, :143 pairing a second time with :110's RULE line). FINAL VERDICT: report read CLEAN; delivery unblocked. |
| Item-8 resolution package (revised P.Binary.PhysEqual; 13 §1 folding + refinement; Preserves/Rewrite.Local restated; CSE.Replace NOTES; 20 §2/§5.6/EffectLinear; classic_physequal_box addendum) | RULING REVIEW (2026-07-22, pre-mechanization gate at main's request; full diffs, not summaries: 06/10/13 vs the rescue snapshot, 20 + 14-validation vs HEAD) | 06-primitives-memory.md:1191-1234 diff; 13-soundness.md §1 + §2 + §4-item-8 diff (incl. the :745-754 qualification tail); 10-simplify-rewrites.md:475-479 diff + CSE.Eligible :427-457; 20-to-cmm-soundness.md diff + EndToEnd :89-115 + InvalidUnreached; 14-validation/classic_physequal_box.md diff; 17-representation.md R.Observe :462-479; 04-opsem.md OS.Let.Prim premises; 06:62 mutability grammar; equivalence-phrase sweep over all chapters; ∋-notation grep | DESIGN PASSES — the refinement architecture is the right shape and coheres: pruning direction correct ({true} ⊆ {true,false} witness reclassification verified); the each-half-insufficient argument verified in both directions; CSE.Replace correction is item 8's mandated sentence verbatim; the symbol-disequality qualification (:745-751) is sound (licensed by always-derivable-0, not 1-underivability); repeated-comparison consistency handled ("consistent resolution is itself a resolution"); the NaN caveat is inherited, not worsened (==⇒= already fails at NaN in real OCaml; the float warning survives); R.Observe needs NO change — folding is correctly localized to the flambda-side §1 relation, the bridge's literal-equality reading survives because the backward-direction abstract behavior is the one matching Cmm's identity choices; closures-in-ι DEFENSIBLE (pipeline shares/duplicates/lifts closures; (=) raises on functionals so the manual's ==⇒= is vacuous there); EffectLinear Drop-arm answer ACCEPTED on its two grounds, completed by KF-057's oracle-input rider; no RULE id or STATUS moved in any of the five diffs (baseline 303/66/84 safe, Dijkstra re-certifies). THREE FINDINGS: KF-055 (high — the ι/Immutable_unique exclusion is DECIDED at 13 §4:751-754 but absent from the normative definition sites, and its letter leaves same-pointer unique comparisons (exception dispatch) with NO derivation; plus the ∋ notation is undefined and ch. 04's =-premises don't accept relational results — three one-line fixes + one sentence); KF-056 (high — Simulates' current ∀b∃o direction is refutable under relational PhysEqual; the re-posing must flip to ∀o∃b "Cmm refines Flambda", which EndToEnd's own :101-102 gloss already names; EndToEnd's "equivalence" parenthetical stale; decomposition needs a determinacy caveat); KF-057 (medium — cextern answer-monotonicity under folding is a new implicit premise; discipline sentence + watch). LOW rider: 11-inlining.md:170 still cites "observationally equivalent … stated in chapter 13" for what is now a refinement obligation — the only stale cross-reference the sweep found. |
| Item-8 mechanization wave (fold core + re-shaped statements + event apparatus) | WAVE VERIFICATION (2026-07-22, at Hopper's green; against entries 44/75 and rulings (a)-(f)) | Values.v:185-416 (iota_object/iota_struct/iota_addr/is_iota, fold_vsim/fold_osim/fold_heap_sim, fold_eq diagonal, reflexivity family incl. fold_osim_refl's non-code premise, W-35 tripwire comment); Soundness.v:128-237 (ITEM-8 AMENDMENT header, fold_bij record, pins, event_sim), :334-364 (obs_equiv/obs_refines/no_ub), :497-514 (Preserves), :555-600 (Local + the rewritten KF-030 disclosure); ToCmmSoundness.v:578-623 (re-posed Simulates); Pilot.v:879/:908 sighted; counts by my own greps (Admitted terminators 32; unique id-status pairs 453; pre/post Print Assumptions name-set diff EMPTY) | ALL PASS. (1) iota_object implements the ruled table exactly (constructor-matched Immutable for the five μ-bearing forms; Closures/Boxed wholly ι; bigstring/bigarray/lazy/code excluded) with the doc rationale in the comment; iota_struct = ι minus closures with ruling (c)'s rationale verbatim. (2) fold_vsim/fold_osim: FV_clos = b + literal slot; FV_ptr_iota descent gated on BOTH sides iota_struct; FO_closures unconditional = catalog-44 leaf opacity (consistent with ruling (c) and the pre-item-8 certified treatment); no HO_Code clause with the reachability argument stated. (3) fold_bij: functionality waived on ι SOURCES, injectivity on ι TARGETS, fb_sym_l statics-fixed, fb_sym_r's lifting disjunct correctly ι-restricted; classification-stability note (class permanent once allocated) sound. (4) DESIGN POINT BLESSED: fold_osim_refl's explicit non-code premise is the honest shape — fold_osim is only consulted on HK_addr fetches, where HO_Code never lives (well-formed heaps install code at HK_code), so the premise discharges at every use site; the unconditional diagonal being unprovable is disclosed at the site as required. (5) obs_refines textually = obs_equiv's second conjunct, direction correct (transformed ⊆ source); Preserves/Local premise sides untouched, conclusions obs_refines. (6) Simulates re-posed exactly per KF-056 + ruling (e): certified premise set unchanged, set-level no_ub, ∀ Cmm outcome ∃ b_f b_q with fl_has_behavior + beh_sim quotient + tocmm_beh_rel UNTOUCHED. (7) Event apparatus: per-event fold_bij at call-time heaps (behavior-wide record correctly gone for the heap-less behaviors), results under full b with the fresh-ι anchoring disclosed; probed the diverging-last-event corner — fresh results from the final event before divergence are never re-read in the trace semantics, the same boundary as the certified KF-015 design, sound. (8) The KF-030 wave rider LANDED (Soundness.v:559-584 mirrors entry 55's rewrite exactly — structural ground alone, second ground dissolved, rw_cse obligation booked). (9) Counts independently confirmed: Admitted 32, census pairs 453, Print Assumptions name-sets IDENTICAL pre/post wave (empty diff of the two logs). No findings. |

## Findings

### KF-001 — arity_info dispatch arity ignores is_tupled (medium)
- Chapter: 04-opsem.md:689-693 (OS.Apply.IndirectUnknownArity.Full NOTES);
  grounded by 20-to-cmm-soundness.md:350-353 (closinfo arity = param count
  *negated for Tupled*) and 17-representation.md:395-396.
- Rocq: theories/Values.v:98-101 (`ai_arity`), ENCODING NOTE at 88-96;
  CORRESPONDENCE.md catalog entry 9. Owner: Plotkin.
- What differs: the doc says the arity compared against `|s̄|` in §6.2 is
  "the one after that adjustment" (the tupled adjustment); at the Flambda
  level a tupled callee's generic full application supplies ONE argument
  (the tuple). `ai_arity := length (unarize ai_params_arity)` ignores
  `ai_is_tupled`, giving the component count instead.
- Why it matters: if Opsem.v's §6.2 rules compare `ai_arity` directly, an
  indirect full application of a tupled n-ary function (n ≥ 2) is
  misclassified as Partial (j = 1 < n) — the wrong rule fires. Catalog
  entry 9's claim that `ai_arity` is "for the §6.2 dispatch comparison"
  overstates what the encoding delivers.
- Note: using the *unarized* length for the non-tupled case is defensible —
  Flambda apply args are unarized, and ch. 20 hides unboxed-product generic
  application behind the CM.Apply axiom (20-to-cmm-soundness.md:354).
- Status: resolved — Plotkin fixed at the definition (Values.v:125-127:
  `if ai_is_tupled ai then 1 else length (unarize (ai_params_arity ai))`,
  verified 2026-07-18); catalog entry 9 amended.

### KF-002 — cextern callee is a value; doc's Cextern takes the simple (low)
- Chapter: 04-opsem.md:796 (OS.Apply.CCall premise
  `(r̄, H′) ∈ Cextern(s_f, v̄, H)` — s_f syntactic).
- Rocq: theories/Opsem.v:51-52 (`cextern_rel : value -> ...`).
  Owner: Plotkin.
- What differs: the oracle is keyed by the callee's *value* `⟦s_f⟧ρ`, not
  the syntactic simple `s_f`.
- Why it matters: mild — value-keying is arguably the better semantics
  (ρ-independent), but it is a deviation from the literal rule and carries
  no ENCODING NOTE / catalog entry yet, violating the fidelity contract's
  "every deviation is cataloged".
- Status: resolved — accepted as a deliberate, now-cataloged deviation:
  ENCODING NOTE added at the `cextern_rel` definition, CORRESPONDENCE.md
  catalog entry 20. The fidelity contract's complaint (uncataloged
  deviation) is thereby discharged.

### KF-003 — P_Binary_BlockSet: unconstrained access kind accepts representation mismatches (medium)
- Chapter: 06-primitives-memory.md:670-692 (P.Binary.BlockSet; NOTES: "undef
  if not a block pointer, i out of range, or representation mismatch").
- Rocq: theories/PrimMemoryA.v:473-479 (`P_Binary_BlockSet`, `forall bak`).
  Owner: Curry.
- What differs: the constructor quantifies over an arbitrary `bak`, so
  `Block_set` with `BAK_naked_floats` or `BAK_mixed` applied to a plain
  values `HO_Block` *derives `PR_ok`* (the store proceeds). The doc makes
  kind/representation mismatch undef; the project's undef policy maps that
  to "no derivation" — but this constructor supplies one. Contrast
  `P_Unary_BlockLoad` (PrimMemoryA.v:437-442), which correctly pins
  `BAK_values`.
- Why it matters: the model accepts (and gives semantics to) stores the doc
  classifies as UB; also creates derivation overlap with
  `P_Binary_BlockSet_Mixed`. Fix: pin the access kind to `BAK_values`.
- Status: resolved — verified in the reconciled PrimMemoryA.v:
  `P_Binary_BlockSet`/`_undef` (539-554) pin `BAK_values bt bsz bfk`;
  Naked_floats/Mixed access on a values `HO_Block` is now underived, and
  the new `P.Binary.BlockSet.NakedFloats` artifact (556-) pins
  `BAK_naked_floats` with its own undef companion.

### KF-004 — doc gap: no Block_set rule for FloatBlock (escalated to main)
- Chapter: 06-primitives-memory.md §Block_set (only P.Binary.BlockSet on
  values Blocks, 670, and P.Binary.BlockSet.Mixed, 695). Compare Block_load,
  which has a .NakedFloats variant (632).
- What's missing: `Block_set { kind = Naked_floats; … }` on a
  `FloatBlock(μ, f̄)` — the compilation of mutable float-record field
  assignment (Lsetfloatfield; `Block_access_kind.Naked_floats` is shared by
  load and set, and `simplify_block_set` handles it). With no rule, every
  float-record store is stuck, i.e. UB under the no_ub policy — clearly not
  the code's semantics.
- Why it matters: Curry cannot add an artifact without a rule id (one
  artifact per rule); the chapter needs a P.Binary.BlockSet.NakedFloats rule
  (or an explicit generalization of the generic rule). This formalism has
  caught doc gaps before; escalating rather than asking the Rocq to invent.
- Status: resolved (by doc fix) — main confirmed against
  flambda_primitive.ml (set-side Block_access_kind has Naked_floats →
  Float_record) and added RULE P.Binary.BlockSet.NakedFloats to
  06-primitives-memory.md (mirrors the load variant; store proceeds
  regardless of μ). Project baseline is now 450 rules (ch. 06 = 69). The
  first fidelity finding to change the formalism itself. Curry now owes
  the matching artifact (35 → 36 A-side rules); re-check at PrimMemoryA
  green.

### KF-005 — P.Variadic.MakeArray: element-conformance premise dropped (low)
- Chapter: 06-primitives-memory.md:523 (premise `v̄ = v₁ … vₙ (unarized
  elements conforming to ak)`).
- Rocq: theories/PrimMemoryA.v:420-424 (no conformance premise; comment
  says frontend/kinding invariant). Owner: Curry.
- What differs: the doc's premise line carries "(conforming to ak)"; the
  NOTES disclaim only the float-array-optimisation invariant, not general
  conformance. MakeBlock.Mixed *does* keep its kind premises. As encoded,
  the model can build e.g. an Immediates array holding pointers, which
  downstream array-load/prover rules may implicitly assume impossible.
- Why it matters: low now, but concretization/soundness statements over
  arrays could silently rest on an invariant the model doesn't establish.
  Either add a `Forall2 value_has_kind` premise (needs per-ak element
  kinds) or rebut with the ill-formed-not-undef reading and a site comment.
- Status: resolved — verified: `P_Variadic_MakeArray` (PrimMemoryA.v:451-456)
  now carries `elems_conform (ak_elem_kinds ak) vs` (groupwise, kind-level,
  unboxed products flattened per catalog 4). Deliberately kind-level only:
  immediacy of AK_immediates elements and the float-array-optimisation
  invariant stay frontend-maintained per the doc NOTES' "not checked here"
  — accepted; my assessment is that downstream array rules dispatch on the
  stored `ak` at kind level, so the sub-kind strengthening is not
  load-bearing. Site comment records the split.

### KF-006 — P.MixedShape.FieldKinds: normative defining clause as Admitted theorem (low)
- Chapter: 06-primitives-memory.md:179-197 (STATUS normative; the defining
  equation of field_kinds).
- Rocq: theories/PrimMemoryA.v:95-100 (`Theorem P_MixedShape_FieldKinds …
  Admitted.`). Owner: Curry (defining function is Church's, Base.v).
- What differs: status mapping says a normative *defining clause* becomes a
  Definition/Fixpoint equation (or constructor); Admitted is for
  properties/conjectures. Here the rule id sits on an Admitted
  characterization of Base.v's `mixed_block_field_kinds`.
- Why it matters: an Admitted normative defining clause is exactly the
  STATUS-mapping violation class the audit exists to catch; if Base.v's
  definition matches the doc, this proves by reflexivity/induction and
  should be Qed (or the RULE comment should move to Base.v's definition,
  coordinated with Church).
- Status: resolved — verified: PrimMemoryA.v:69-74 is now
  `Proof. intros s. reflexivity. Qed.` against Base.v's
  `mixed_block_field_kinds`, which is literally the doc equation; site
  ENCODING NOTE records that the defining clause lives in Base and this id
  sits on the definitional restatement. (Audit note: a deliberate non-True
  Qed outside the WF.Syntax by-construction pair — sanctioned, real
  equation.)

### KF-007 — CM.Apply's nested-run encoding loses callee-escaping exceptions and callee divergence (high; doc-rooted, escalated)
- Chapter: 15-cmm.md:519-537 (CM.Apply: callee "yields result r̄ and memory
  M′ (running that function's own Cmm body to a Return_lbl exit)");
  15-cmm.md:584-604 (CM.Unit.Final: outcomes include "termination by
  uncaught exception" and "divergence"; UB = "reaching Cinvalid or a stuck
  read/store"). Contrast 04-opsem.md:641-665 (OS.Apply.Direct gives the
  callee `T_body = [k_exn]` with an `Exn` *boundary* entry chaining to the
  caller's handler, and return via a `Return` boundary — small-step).
- Rocq: theories/Cmm.v:1079-1092 (`CM_Apply` requires
  `cm_returns (fd_body fd) … M tr rvals M'`), 1220-1226 (`CM_Returns` runs
  the callee with `kenv_empty`, `TT = []`, `RR = []` to a
  `cm_return_config`), 1294-1301 (`CM_Unit_Undef`). Owner: Milner.
- What differs / consequence: `cm_returns` only holds for callee runs that
  *terminate at a return exit*. Therefore, at the call site:
  (a) a raise in the callee that escapes it is stuck (the callee's local
  `TT` starts empty and its `chi` is `kenv_empty`, so `CM_Raise` cannot
  fire and no rule surfaces the raise to the caller) — so **an ordinary
  `try … with` around a function call never catches**: the caller's
  handler label was pushed on the *caller's* TT, which the callee cannot
  see. The whole run is then classified `CMO_undef` by `CM_Unit_Undef`
  (stuck, not final, not uncaught).
  (b) a diverging callee likewise yields no step at the `Capply` redex, so
  callee divergence is *stuck-at-the-call*, classified `CMO_undef`, never
  `CMO_diverges` (whose `cm_diverges` needs an infinite top-level `cm_step`
  sequence), and the callee's event trace up to divergence is lost.
- Why it matters: the Flambda machine is small-step through calls (boundary
  entries), so uncaught exceptions and divergence inside calls are real,
  distinct behaviors there. Ch. 20's simulation would have to relate
  Flambda `uncaught`/`diverge` to Cmm `CMO_undef` — i.e. it is false or
  vacuous for any program that raises across a function boundary or loops
  inside a call.
- Root cause is the DOC: ch. 15's CM.Apply has no analogue of ch. 04's
  boundary device, and its own NOTES only cover the returning case; the
  Rocq is a faithful transcription of a rule that cannot deliver two of
  CM.Unit.Final's five outcomes. Candidate doc fix: make the nested run's
  outcome three-valued (returns v̄ / escapes with v_exn / diverges) and add
  CM.Apply.Raise (re-raise in the caller, doc-groundable in the runtime's
  global trap stack) + let divergence of the nested run count as
  divergence of the caller (or switch ch. 15 to a call-stack small-step).
- Status: resolved (by doc fix; closure check done 2026-07-18, baseline
  451). Verified against the amended 15-cmm.md: §8 intro states the
  nested-run outcome trichotomy; CM.Apply's premise is now the RETURNS
  case; new CM.Apply.Raise steps the call site to
  `Cop(Craise Raise_reraise, [v_exn])` with the CALLER's TT in force — so
  try-with around calls catches, and truly-uncaught raises chain
  re-raise-by-re-raise to the base frame (CM.Unit.Final's uncaught
  outcome, which its NOTES now note needs no special nesting clause);
  CM.Unit.Final's divergence covers nested runs "hereditarily". All five
  outcomes now deliverable through calls. Two residual notes (not
  reopening): (a) CM.Apply's premise does not require the nested run's
  trap stack to be back at its base at the Return_lbl exit (the toplevel
  analogue in CM.Unit.Final says "TT balanced to the base") — a
  well-formedness gloss worth one clause; PARKED as candidate doc
  clarification per main's ruling (2026-07-18): not load-bearing until
  proofs, no doc round now (concrete Rocq site: CM_Returns,
  Cmm.v:1240-1246, discards TT′); (b) "the callee's Cextern
  events are part of the caller's trace" is prose — for diverging nested
  runs the mechanization needs the trace-carrying outcome (exactly
  KF-008's planned mirror). Rocq rework is Milner's post-green increment;
  re-verify CM_Apply/CM_Apply_Raise/cm_outcome then.
  Residual (a) CLOSED (2026-07-18, verified): the KF-014 doc amendment
  added "its trap stack balanced back to its base" to CM.Apply's
  premise, and the synced CM_Returns (Cmm.v:1259-1264) now requires
  `cm_return_config c_f vs []` — the trap stack after the return
  exit's trap actions is the nested base. Unparked and struck; agreed
  with Milner/main that the KF-014 balance clause closes it in the
  same stroke. Residual (b) was already closed by the KF-008 mirror.

### KF-008 — CMO_diverges carries no trace; asymmetric with the Flambda behavior split (low)
- Chapter: 15-cmm.md:594-598 (divergence listed as an outcome; the doc
  attaches the Cextern-event trace only to normal termination).
- Rocq: theories/Cmm.v:1251-1263 (`cm_diverges`, `CMO_diverges` — no
  event trace), vs. Opsem.v's catalog-11 split (`Beh_diverge` carries the
  finite prefix trace; `Beh_react` a coinductive stream). Owner: Milner.
- What differs: the Cmm divergence outcome discards all observable events,
  including an infinite stream of `CME_extern` events (a reactive
  program). Literal to ch. 15's silence, but the Flambda side kept traces
  precisely so ch. 20 can state trace preservation case-by-case; on the
  Cmm side as encoded, that statement is unstatable for diverging and
  reactive programs.
- Why it matters: Milner owns both sides of the ch. 20 statement; cheaper
  to mirror the CompCert split now than to discover it at wave 6. (If
  ch. 20 deliberately weakens divergence to trace-free, that choice should
  be an ENCODING NOTE + catalog entry.)
- Status: resolved (accepted) — Milner is mirroring the catalog-11
  CompCert split on the Cmm side, folded into the same post-green
  increment as the KF-007 CM.Apply rework (both rewrite the outcome
  section). Verify the mirrored encoding when that increment lands.

### KF-009 — CM.Alloc.Local: "hdr carries caml_local" premise dropped (low)
- Chapter: 19-cmm-memory-gc.md:82-84 (premise line "hdr carries caml_local
  (R.Header)"). Rocq: theories/CmmMemory.v:164-192 (`CM_Alloc_Local`; the
  site comment defers the premise to ch. 17). Owner: Milner.
- What differs: the doc states it as a premise of THIS rule; the encoding
  drops it entirely, so a local allocation with a heap-form header steps
  fine. The site comment's justification ("R.Header's constraint, not
  re-stated") converts a premise into a downstream obligation nobody
  currently states.
- Why it matters: mild behavior-widening now, and the pattern has a better
  precedent in this very file: GC condition (v) was "not statable yet" too
  until it was factored as a constructor premise over what IS modeled.
  An opaque `Parameter header_is_local : Z -> Prop` (constrained later by
  ch. 17's R.Header) would restore the premise at zero cost, mirroring the
  gc_reloc oracle treatment.
- Status: RESOLVED-PENDING-COMPILE (2026-07-18) — main sanctioned the
  Parameter treatment ("add Parameter header_is_local next round,
  constrained by R.Header in Representation.v, ENCODING NOTE citing
  Knuth's KF-009"); Milner applied it in the round-3 batch with
  Hopper: `Parameter header_is_local : Z -> Prop` after alloc_point in
  CmmMemory.v, premise `header_is_local hdr` in CM_Alloc_Local,
  sanction-citing ENCODING NOTE. Verify on disk at the round-3 green
  (same slot as W-18/W-22); Representation.v review must check the
  R.Header constraint actually lands.
- Status: RESOLVED (2026-07-18, green-verified on disk) — the delta rode
  the code_env full-chain rebuild and is green per Hopper/Milner.
  Verified: `Parameter header_is_local : Z -> Prop` (CmmMemory.v:119)
  under the sanctioned ENCODING NOTE citing KF-009 and deferring the bit
  layout to R.Header/ch. 17 (:113-118); premise `header_is_local hdr` in
  `CM_Alloc_Local` (:189) with the doc-order premise comment naming it
  (:182-185). Residual: Representation.v carries the constraining side
  as `header_is_local_spec` (Theorem...Admitted, increment 1, per
  Milner) — check at the Representation.v review.

### KF-010 — CM.Alloc.GC pins the expression, so pending Val words are never relocated (medium; doc-rooted, escalated)
- Chapter: 19-cmm-memory-gc.md:130-153 (CM.Alloc.GC: conclusion keeps e_c
  unchanged; condition (ii) covers only ce and M′). Rocq:
  theories/CmmMemory.v:236-258 (`gc_reloc` note + `CM_Alloc_GC`: both
  configs carry the SAME `e`). Owner: Milner.
- What differs: in this plugging machine, already-evaluated operands live
  as `Cval` words inside the expression — including the Calloc redex's own
  field values `vs` and any evaluated operands in pending context frames.
  The constructor's identical-`e` makes them un-relocatable, and the .v
  ENCODING NOTE's condition (ii) parenthetical "(and in pending context
  frames, which are machine temporaries)" claims coverage the constructor
  cannot deliver — the exact claims-more-than-delivered class.
- Why it matters: the canonical make-block-of-pointers (`Calloc` whose
  fields are Val pointers) hits this on EVERY GC at that alloc point: phi
  moves a field's block, CM_Alloc_Heap then writes the stale word, and
  ch. 17's ≈-preservation (hence ch. 20's stutter simulation) is false as
  stated. In the real machine those operands are Val-typed registers, i.e.
  GC roots that ARE updated; the doc rule, written against the plugging
  presentation, missed that the plugged expression plays the register
  file's role. The doc has the same gap (e_c fixed on both sides), so this
  is doc-rooted like KF-007.
- Candidate fix (minimal): let the rule/constructor relate different
  expressions and move the new condition into the oracle where the other
  unstatable conditions already live — doc: conclusion
  `⟨e_c′, ce′, χ, M′, TT, RR⟩` with (ii) extended to "every Val word
  pending in e_c" (identifiable via the machtype discipline, same license
  as (ii)/(iii)); Rocq: `gc_reloc phi (CmCfg e …) (CmCfg e' …)` with the
  documented condition list gaining (ii′). Alternative (a Clet-discipline
  premise "no Val word pending across an alloc point") fails on the alloc's
  own field operands, so the oracle route looks forced.
- Status: resolved (by doc fix; re-read verified 2026-07-18) — the amended
  CM.Alloc.GC is exactly per the candidate fix: conclusion carries e_c′
  (19-cmm-memory-gc.md:137), condition (ii) extended to pending Val words
  including the allocation's own field operands with the e_c′/ce′ equations
  spelled out (:140-143), NOTES adds the expression-as-register-file
  rationale (:154-158). Rocq sync (unpin e in gc_reloc/CM_Alloc_GC) is
  Milner's; re-verify at his next CmmMemory.v increment.

### KF-011 — gc_write_barriers lists a non-existent extern (low)
- Chapter: 19-cmm-memory-gc.md:175-177, 188-190 (premise names
  caml_modify / caml_initialize; NOTES adds "addr_array_initialize on the
  large-block fill path" — a HELPER name). Rocq:
  theories/CmmMemory.v:285-287 (`gc_write_barriers` includes
  `"caml_addr_array_initialize"`). Owner: Milner. Code ground truth:
  backend/cmm_helpers.ml:1841-1854 — the `addr_array_initialize` helper
  emits `Cextcall { func = "caml_initialize" }` over an `array_indexing`
  address; no extern named caml_addr_array_initialize exists in backend/
  or to_cmm/ (grep). (`caml_modify_local` takes the block + index, no
  Addr, so it needs no whitelist entry; both real barriers are
  alloc=false as the encoding assumes — cmm_helpers.ml:1791, 1850.)
- Why it matters: the third entry never matches, so no soundness impact —
  but a whitelist that names a phantom extern misleads exactly the reader
  the ENCODING NOTE addresses, and the correct two-element list makes the
  under-approximation claim precise.
- Status: RESOLVED-VERIFIED (2026-07-18) — applied verbatim:
  `gc_write_barriers = [caml_modify; caml_initialize]`
  (CmmMemory.v:304-305) with the helper-not-extern comment for
  addr_array_initialize (:294-297).

### KF-012 — expr_addr_ok rejects the nested-Cadda addresses array_indexing emits (medium)
- Chapter: 19-cmm-memory-gc.md:173-177 ("Each field_address (Cadda) …
  recomputed inline from a Val base … and consumed at once — as the
  address operand of the enclosing Cload/Cstore (… ArrayAccess) …").
  Rocq: theories/CmmMemory.v:331-371 (`expr_addr_ok`: the
  Cload/Cstore/barrier patterns match ONE Cadda layer and check its base
  `b` with plain `expr_addr_ok`, whose `Cop Cadda _ => false` clause then
  rejects a Cadda base). Owner: Milner. Code ground truth:
  backend/cmm_helpers.ml:1672-1689 — `array_indexing`'s dynamic-index
  cases emit `Cadda [Cadda [ptr; scaled]; Cconst …]`, the address form of
  ordinary array loads/stores (also the caml_modify path via
  addr_array_set_heap, 1816-1817).
- Why it matters: the predicate is sound but rejects the main
  array-access pattern, so when ch. 16/18's formal translation mirrors
  array_indexing, translated code fails `expr_addr_ok` and
  CM_Addr_NoSurvive (plus any ch. 20 use of the discipline) becomes
  vacuous for array programs. The doc's discipline permits the chain — the
  inner Cadda is itself consumed at once; the chain's root is the Val
  base. Fix: check the address operand with a chain-aware helper
  (`addr_chain_ok a := match a with Cop Cadda [b; o] =>
  addr_chain_ok b && expr_addr_ok o && negb (expr_allocates o) | _ =>
  expr_addr_ok a end` — the o-legs alloc-free since the earlier Cadda is
  pending while they evaluate), keeping the bare-Cadda rejection
  everywhere else.
- Status: RESOLVED-VERIFIED (2026-07-18) — applied with a sound
  encoding change from my sketch (whose fallthrough called a sibling
  on a non-subterm, rejected by the guard checker): a flag,
  `expr_addr_ok' (addr_pos : bool)` (CmmMemory.v:364-413), where the
  Cadda arm requires `addr_pos` and recurses base-with-true /
  offset-with-false + `negb (expr_allocates o)`; the three consuming
  forms (Cload :396, Cstore :398, barrier Cextcall :402) pass true to
  their address operand; `expr_addr_ok := expr_addr_ok' false`.
  Semantically my chain: chains accepted exactly under consumers,
  bare Cadda rejected elsewhere. Site-noted (:354-) that the uniform
  per-layer non-allocating-offset condition is marginally STRONGER
  than the discipline needs for the innermost offset (no Addr live
  while it evaluates) — acceptable: array_indexing's offsets are
  scaled-index/const shapes, none allocating; would only matter if a
  ch. 16/18 emitted shape had an allocating innermost offset, which
  none does.

### KF-013 — CM.Alloc.GC admits infinite silent self-stuttering, polluting the divergence outcome (medium; doc-rooted, escalated)
- Chapter: 19-cmm-memory-gc.md:130-153 (CM.Alloc.GC — "the machine MAY take
  a collection step" at an allocation point; conditions (i)-(v) place no
  lower bound on progress); 15-cmm.md CM.Unit.Final (divergence = an
  infinite step sequence). Rocq: theories/CmmMemory.v:251-258
  (`CM_Alloc_GC`, silent, fires at any `alloc_point`), Cmm.v:1311-1321
  (`cm_diverges` counts silent steps), 1378-1382 (`CM_Unit_Diverges`).
  Owner: Milner (rule); the hole is the DOC's.
- What differs / consequence: the identity relocation (phi = ∅ — nothing
  moves, nothing dropped) satisfies (i)-(v) vacuously, so any faithful
  gc_reloc instantiation admits a GC step that maps an alloc-point config
  to itself. Chaining it gives an infinite silent step sequence, so
  `CMO_diverges` becomes derivable for EVERY program that reaches an
  allocation — including ones that terminate normally. (In the .v as-is
  the Parameter has no axioms, so nothing is provable either way; the
  pollution appears the moment gc_reloc is instantiated or axiomatized
  faithfully, which Representation.v must do.)
- Why it matters: outcome-set statements (ch. 20's simulation, any ch. 13
  style "same termination outcome" lifted through ch. 20) are broken in
  the target-diverges ⟹ source-diverges direction: the target's
  divergence set contains junk the source (whose OS machine has no GC
  rule) cannot match. This is the classic stutter-divergence problem;
  CompCert-style stutter simulations need exactly this excluded.
- Candidate fixes (doc-level, either suffices): (a) fuse collection into
  the allocation step (CM.Alloc.Heap nondeterministically GCs then
  allocates, one step — kills self-loops structurally); or (b) keep the
  standalone rule but define the divergence outcome over runs with
  infinitely many non-GC steps (a fairness proviso in CM.Unit.Final's
  divergence clause + a NOTES sentence).
- Status: escalated; RULED by main (2026-07-18) — option (a), fusion
  into the allocation step: kills the stutter by construction, matches
  where collectors actually run (the rule's own "at an allocation
  point"), avoids infinite-run fairness machinery, and composes with
  the applied KF-010 fix (one phi over ce + pending words + M, then
  allocate). RESOLVED-BY-DOC-FIX (re-read 2026-07-18): the applied
  amendment (19-cmm-memory-gc.md:136-151) makes CM.Alloc.GC the fused
  rule — collection is an atomic prefix of the allocating step,
  conclusion ⟨e_c″, ce′, χ, M″, TT, RR⟩ with the collected config
  performing CM.Alloc.Heap/Local "with no further collection", and
  "There is no standalone collection transition"; the NOTES name the
  zero-progress pathology and conclude every infinite run has
  infinitely many non-GC steps. KF-010's clause (ii) is preserved
  verbatim inside the fused rule, so the two fixes compose.
  Rocq sync VERIFIED (2026-07-18): cmem_gc has a single constructor
  CM_Alloc_GC (CmmMemory.v:260-271) whose premises are alloc_point,
  gc_reloc to a collected config that is a plug of an all-Cval Calloc
  redex (the KF-010 unpin: the collected expression e_c′ is free, tied
  to e only through gc_reloc's clause (ii), stated in the oracle's
  ENCODING NOTE :226-231), condition (v) as a constructor premise
  (mem_local ⟹ phi undefined), and a MANDATORY cmem_head allocation in
  the collected memory — no standalone collection exists, so the
  identity-phi self-loop is dead by construction (every cmem_gc step
  replaces the redex with the allocation's result); closure check for
  main PASSES.

### KF-014 — nested runs start with RR = [], stranding callee allocations into the caller's region (medium/high; escalated as doc-clarification)
- Chapter: 15-cmm.md:527-573 (amended CM.Apply/CM.Apply.Raise: "fresh
  variable and catch environments, trap stack at its own base" — SILENT on
  the nested run's region stack; conclusions keep the caller's RR);
  19-cmm-memory-gc.md:75-93 (CM.Alloc.Local requires the target region to
  be the top of the config's own RR). Rocq: theories/Cmm.v:1240-1257
  (`CM_Returns`/`CM_Escapes` start the nested config with `RR = []`),
  1284-1286 (`cm_call_frame` likewise); CmmMemory.v:177-192
  (`CM_Alloc_Local` needs `RR = iota :: RR0`). Owner: Milner.
- What differs / consequence: a callee that allocates into its CALLER's
  region — the exclave / local-returning-function pattern, the flagship
  ch. 19 feature — executes `Calloc CAM_local` with (in the model) an
  empty or already-popped region stack, so no rule fires: the nested run
  is stuck, `cm_returns`/`cm_escapes` are underivable, and the whole run
  is classified `CMO_undef`. In the runtime the local stack pointer is
  global state threaded through calls (like M, whose threading the rules
  DO model), so these programs are perfectly defined.
- Why it matters: every `local_`-returning call is misclassified UB;
  ch. 20's simulation is vacuous/false for stack-allocation programs,
  which is precisely the workload ch. 19 exists to describe.
- Candidate fix (mirrors M's threading): nested runs start with the
  caller's RR (doc: add "region stack of the caller" to the nested-run
  parenthetical; Rocq: `cm_call_frame`/`CM_Returns`/`CM_Escapes` carry
  RR_caller instead of []), and CM.Apply's RETURNS premise requires the
  return config's RR to equal RR_caller (balance — the analogue of the
  trap-stack-at-base discipline; CM.Region.Begin/End inside the callee
  then compose correctly, and mem_local/mem_reclaim already thread via
  M′). CM.Apply.Raise keeps the caller's RR in the conclusion as now
  (with a NOTES caveat that unwinding-time region reclamation is not
  modeled — same license as the current trap-unwind treatment). Caveat
  for the fix review: confirm at ch. 16 (TC.* / exclave lowering) that
  callee-side Calloc-local into the caller's region is indeed the emitted
  form (expected: End_region of own region then allocate into enclosing).
- Status: escalated; ACCEPTED by main (2026-07-18) — Milner drafts the
  RR-inheritance + balance amendment. Main's open sub-question for the
  draft (to be resolved against the runtime): whether an ESCAPING raise
  restores RR to the caller's, or region closes may be skipped on
  unwind — the draft must state which. RESOLVED-BY-DOC-FIX (re-read
  2026-07-18): the applied amendment (15-cmm.md:516-584) delivers the
  RR-inheritance + balance design — §8 intro and both CM.Apply rules
  now read "region stack starting at the caller's RR", CM.Apply's
  RETURNS premise adds "its region stack back at RR" (balance, paired
  with the trap clause), and NOTES state the to_cmm discipline backing
  it. Main's sub-question is answered in the STRONG direction, with
  runtime evidence: CM.Apply.Raise's nested run ends "with memory M′
  and region stack RR′" and the conclusion carries RR′ — an escaping
  raise does NOT implicitly close callee-opened regions (caml_raise_exn
  / RESTORE_EXN_HANDLER_OCAML restores %rsp and exn_handler only;
  caml_local_sp untouched); they are reclaimed by the catching
  handler's explicit End_region via CM.Region.End's pop-down-to-ι
  shape. Faithful and better than my proposed caveat-only treatment.
  Rocq sync VERIFIED (2026-07-18): cm_returns (Cmm.v:1256-1264) and
  cm_escapes (:1275-1284) start the nested run at
  `CmCfg body ce0 kenv_empty M [] RR` — traps at own base, caller's RR
  inherited, so CM_Alloc_Local is satisfiable inside callees and the
  exclave/local-returning pattern runs (closure check for main
  PASSES); CM_Returns requires BOTH balances (`cm_return_config c_f
  vs []` for traps — also closing KF-007 residual (a) — and
  `cc_regions c_f = RR`); CM_Escapes reads RR′ off the escape config
  with no region balance, and CM_Apply_Raise's conclusion carries RR′
  (Cmm.v:1124-1125), matching the runtime evidence in the doc
  (caml_raise_exn restores %rsp/exn_handler only). cm_call_frame's
  nested start config carries `cc_regions c` (:1311-1313), so the
  hereditary-divergence descent device is consistent with the new
  threading — its soundness argument (return configs stepless) is
  RR-independent and carries over. The ch. 16 exclave-form caveat
  stays booked at the ToCmmControl.v review (W-22).

### KF-015 — event_sim under-observes cross-event retention (medium; draft-stage)
- Chapter: 04-opsem.md §8.2 / 13-soundness.md §1 (observables include
  C-call effects, glossed "(I/O and externally-visible mutation)").
  Rocq: theories/Soundness.v:224-247 (`event_sim`: per-event `b_ev`
  required to contain only THIS event's callee/args plus `pins H0`,
  heap-consistent at H_call). Owner: Plotkin (Soundness.v increment 1,
  pre-compile draft — reported at his request for concur/amend).
- What differs: external code retains pointers across calls — the very
  argument the file itself uses to justify ONE behavior-wide b and
  `pins` (ENCODING NOTE, Soundness.v:67-76). But at event j, b_ev_j
  need not contain the args/callee of events i < j (they are in b, not
  b_ev_j), so the call-time heap comparison at event j does not
  constrain the contents of an object passed out at event i. Two runs
  that mutate such an object differently between events i and j, then
  re-converge before the final heap, are beh_sim-related although the
  external can read the difference at event j — "externally-visible
  mutation" that the doc declares observable is not compared.
- Why it matters: exactly the class of transformation soundness ch. 13
  is supposed to police (reordering/coalescing writes to a buffer
  shared with C across calls) is invisible to obs_equiv, weakening
  INV.Simplify.Preserves below the doc's stated observable.
- Candidate fix: make the retained set cumulative — require b_ev_j to
  also relate all values carried by events 1..j-1 (both args and
  RESULTS: results are external-known too). Encoding-wise this means
  event_sim over trace prefixes (an indexed fold) rather than
  pointwise Forall2, or a monotone chain b_ev_1 ⊆ b_ev_2 ⊆ … ⊆ b with
  each seeded by the running union of event values.
- Status: RESOLVED-VERIFIED (2026-07-18) — Plotkin applied the
  cumulative-seed fix, indexed-fold variant: event_sim gains
  `seen : list (value * value)` with premise
  `Forall (fun vv => value_sim b_ev (fst vv) (snd vv)) seen`
  (Soundness.v:239/:249), so b_ev_j must relate every retained pair
  and heap_sim b_ev then closes over what is reachable from them at
  call time; `retained` (:262-269) collects callee, args, and what
  came back — results AND the raise payload (stronger than my minimum,
  correctly: external code constructed the payload and may retain a
  pointer); `trace_sim_from`/`stream_sim_from` thread
  `seen ++ retained ev ev'` per cons (:274-293) with []-seeded
  wrappers, leaving beh_sim textually unchanged. combine-truncation in
  `retained` is harmless (event_sim's Forall2 premises force equal
  lengths; mismatch fallback [] unreachable under event_sim). Site
  comments carry the KF-015 tag. Joint catalog-44 wording co-signed
  with the cumulative sentence included.

### KF-016 — inlining lets can capture caller-side names in the argument simples (medium; draft-stage)
- Chapter: 11-inlining.md S.Inline.Substitute (:79-104, NOTES :99-103)
  and S.Inline.Substitute.ExnExtraArgs (:144-159). Rocq:
  theories/Inlining.v `inlined_body` (:569-584), `bind_params`
  (:511-515), `region_rename` (:556-563); constructors
  RW_Inline_Substitute (:705-723) and
  RW_Inline_Substitute_ExnExtraArgs (:745-786). Owner: Girard
  (increment 2, pre-compile draft — audited at his request).
- What differs: the doc discharges binder/argument collisions by
  fiat — "the let-bound variables x_mydepth, x_myclos, x̄ keep their
  identity because they are freshened by α-conversion when the code
  is instantiated" (NOTES :100-102). The encoding takes `code` from C
  verbatim and replaces freshening with explicit premises
  (CORRESPONDENCE, Binders), but the premises cover only the RENAMING
  targets (kret/kexn/region/ghost vs the body). Nothing prevents a
  callee binder name from occurring in a caller-side simple that is
  nested UNDER that binder by the wrapper lets: (a) a param xᵢ
  occurring in a later argument simple sⱼ (i < j) is captured by
  `let xᵢ = sᵢ in … let xⱼ = sⱼ`; (b) c0_my_closure occurring in any
  sᵢ, or c0_my_depth occurring in s_callee or any sᵢ (possible via a
  Change_depth coercion's rec_info variables), likewise; (c) the
  caller's region/ghost variables — written INTO the body by
  region_rename — are captured by a param let when that param is
  unused in the body (region_targets_fresh checks only the pre-rename
  body, and an unused param defeats the free_vars blocker).
  Two-param exhibit for (a): code params (x, y), body uses y; apply
  args (a, x). Output binds y to a instead of the caller's x.
- Why it matters: rw_inline admits observably wrong rewrite
  instances, so INV.Simplify.Preserves (Soundness.v increment 2b)
  quantified over rw_inline becomes false as stated — same
  provability failure mode as KF-015's converse. Simplify itself
  never produces these instances (global binder uniqueness), which is
  exactly why the premises are the encoding's job.
- Candidate fix: one uniform premise — no name in
  B = {c0_my_depth; c0_my_closure} ∪ map fst (c0_params code) occurs
  in ap_callee or any of ap_args (slightly stronger than the minimal
  triangular condition; Simplify's fresh binders satisfy it), plus
  B disjoint from {region; ghost} when α = App_local. d0 needs
  nothing (bound outermost). Both constructors need it.
- Status: RESOLVED-VERIFIED (2026-07-18, same day) — Girard applied
  the uniform-freshness fix: `callee_binders` /
  `callee_binders_fresh` (Inlining.v:678-701) required by both
  constructors (:793-794, :847-848). Stronger than my sketch in the
  right ways: `simple_uses_var` (:660-673) descends into coercion
  payloads (`ri_mentions_var`), catching the Change_depth channel I
  flagged for c0_my_depth; the region/ghost disjunct is folded in
  (:697-699); the d0-needs-nothing observation and the
  uniform-over-triangular over-approximation are both site-documented
  (:675-677, :687-690). Compile pending (draft file).

### KF-017 — sequential continuation renames diverge from the doc's simultaneous θ under target/source aliasing (medium; draft-stage)
- Chapter: 11-inlining.md S.Inline.Substitute (:90-92: θ is one
  substitution built from the kret/kexn/region components). Rocq:
  theories/Inlining.v `inlined_body` (:573-581) applies them
  sequentially — region, then k_exn^c ↦ kexn_target, then
  k_ret^c ↦ kret_target. Owner: Girard.
- What differs: sequential naive renames coincide with the
  simultaneous θ only if the first rename's TARGET is not the second
  rename's SOURCE. If kexn_target = c0_return_continuation code —
  caller's exn handler name colliding with the callee's return-cont
  binder, which no premise excludes — and the body has no k_ret^c
  occurrence (a callee that always raises or loops; realistic), then
  `cont_occurs kexn_target body = false` (:716-717) is satisfied, the
  exn rename writes k_ret^c occurrences into the body, and the ret
  rename immediately rewrites them again: raises land on the RETURN
  target (κ_ret in Substitute; k_pop in ExnExtraArgs, where the
  aliasing name is k1). If the body does mention k_ret^c the premise
  blocks the rule, which is why this needs the never-returns corner.
  The mirror aliasing (kret target = k_exn^c) is harmless — the exn
  rename runs first and leaves no source occurrences; the
  variable-side analogues inside region_rename are blocked by
  region_targets_fresh (checked: x_myghost = region and
  x_myregion = ghost both contradict the freshness premises).
- Why it matters: same as KF-016 — an unsound instance inside
  rw_inline.
- Candidate fix: add
  `ec_exn_handler (ap_exn_continuation ap) <> c0_return_continuation code`
  to RW_Inline_Substitute and `k1 <> c0_return_continuation code` to
  RW_Inline_Substitute_ExnExtraArgs.
- Status: RESOLVED-VERIFIED (2026-07-18, same day) — both
  disequalities applied verbatim (Inlining.v:789-790, :836), with the
  sequential-vs-simultaneous rationale added to the Substitute RULE
  comment (:764-768). Compile pending.

### KF-018 — ExnExtraArgs: k1 unconstrained against the caller's return continuation (medium; draft-stage)
- Chapter: 11-inlining.md S.Inline.Substitute.ExnExtraArgs
  (:148-157: "k1 fresh"). Rocq: theories/Inlining.v
  RW_Inline_Substitute_ExnExtraArgs (:752-756: k1's freshness =
  non-occurrence in body + k1 ≠ k_pop/k_push/original handler).
- What differs: the k_pop wrapper's jump
  `Apply_cont k r̄ ⟨pop k1⟩` (exn_wrap_ret, :616-629) sits inside the
  new k1 binder's scope (k_pop's let is the BODY of k1's
  let_cont_exn). The doc's "k1 fresh" means fresh among all names in
  play, including the caller's κ_ret = Return k; the premises never
  compare k1 with k. If k1 = k, the return jump is captured and
  returns re-enter the re-raise wrapper. k_pop/k_push need no such
  premise (k occurs only in k_pop's handler, over which neither
  scopes — checked).
- Candidate fix: premise
  `forall k, ap_result_continuation ap = RC_return k -> k1 <> k`.
- Status: RESOLVED-VERIFIED (2026-07-18, same day) — applied verbatim
  (Inlining.v:837-838). Compile pending.

### KF-019 — OS.Unit.Final's halting shapes exclude trap-carrying jumps, misclassifying toplevel uncaught exceptions as UB (medium-high)
- Chapter: 04-opsem.md:911-930 (OS.Unit.Final: halting shapes
  "⟨Apply_cont k_ret⁰ (v̄), …⟩" / "⟨Apply_cont k_exn⁰ (v_exn :: …), …⟩"
  — SILENT about trap actions) and :550-571 (OS.ApplyCont.Raise +
  close_raise0: a raise is `Apply_cont k_h (bucket :: extras)` with
  `Pop { exn_handler = k_h }`). Rocq: theories/Opsem.v `final_config`
  (:1146-1154), `HB_return`/`HB_exn` (:1176-1185) — all three require
  `at_apply_cont rho ctl k vs None`, i.e. NO trap action. Owner:
  Plotkin.
- What differs: a raise whose innermost handler is the toplevel
  k_exn⁰ — i.e. `raise` in the unit's toplevel body outside any try,
  a common shape (`let () = raise Foo` at module level) — is
  `Apply_cont k_exn⁰ (bucket) ⟨Pop k_exn⁰⟩` with T = [k_exn⁰] (the
  OS.Unit.Init base frame). No step rule fires on it: Raise/TrapPop/
  TrapPush/plain ApplyCont all require handler_entry, which does not
  match CE_halt_exn; Return/ExnBoundary require CE_return/CE_exn. And
  final_config's None-premise excludes it from final. So it lands in
  `stuck` and HB_undef classifies the run Beh_undef — where the doc's
  halting shape (trap-action-silent) classifies it as termination by
  uncaught exception. The doc even anticipates the depth accounting:
  OS.ApplyCont.ExnBoundary NOTES :502-503, "At toplevel this pops
  k_exn⁰ and reaches Halt_exn at depth 0 by BOTH paths" — the direct
  path can only be the trap-carrying jump, since Raise cannot fire on
  a Halt entry. (The boundary path is fine in the Rocq: an uncaught
  exception inside a FUNCTION routes through CE_exn → Ctl_jump →
  AAC_jump with None → Final_exn. Only the direct toplevel raise is
  misclassified. The return side has the same latent shape —
  `Apply_cont k_ret⁰ (v) ⟨Pop h⟩` from a try whose join is k_ret⁰ —
  but the exn side is the concrete realistic case.)
- Why it matters: Beh_undef absorbs a defined outcome, so (a) no_ub
  wrongly excludes these programs from ch. 13's soundness license
  (the theorem goes vacuous exactly on programs that raise at
  toplevel), and (b) obs_equiv never compares their v_exn — a
  Simplify bug that changes which exception escapes at toplevel
  would pass. This corrupts the observation layer the KF-015 work
  just tightened.
- Candidate fix: drop the None constraint — quantify the trap action
  in final_config's two constructors and in HB_return/HB_exn
  (`at_apply_cont rho ctl k vs t` with t arbitrary), transcribing the
  doc's shapes literally. Ctl_jump entries are unaffected (AAC_jump
  always yields None), and `stuck`'s ~final conjunct then correctly
  excludes these configs from HB_undef.
- Status: resolved — fix verified on disk 2026-07-18: `t` quantified
  in both final_config constructors (Opsem.v:1154-1161) and in
  HB_return/HB_exn (:1183-1192), exactly the candidate fix; site
  comment (:1144-1152) records the rationale incl. the return-side
  latent shape. AAC_jump still yields None (:201) so Ctl_jump
  entries are unaffected; stuck's ~final conjunct (:1166) now
  excludes the toplevel-raise config from HB_undef. The literal
  transcription admits any t at a halt entry (e.g. a Push-to-halt),
  matching the doc's trap-silent halting shapes; reachability of
  exotic t's is a WF matter, not this rule's. Cmm-side mirror
  tracked as W-25.

### KF-020 — AccumBoxElim's clause (1) quantifies beyond the doc and is code-falsified at Maybe_null (medium; draft-stage)
- Chapter: 12-unboxing.md:358-399 (S.Unbox.Loopify.AccumBoxElim, clause (1):
  the recursive path gives the param **unknown_with_subkind**, "on which
  prove_is_a_boxed_<nnk> is Proved"). Rocq: theories/Unboxing.v:314-323
  (`Theorem S_Unbox_Loopify_AccumBoxElim` quantifies `h` over ANY
  `accum_boxed_head`-matching head — arbitrary payload type and alloc mode —
  and `inull` over ANY `is_null_ty`, including `Maybe_null _`). Owner: Girard.
- What differs: the doc's clause (1) is a claim about one specific type — the
  `unknown_with_subkind` of a boxed-number declared subkind (unknown payload,
  Not_null). The theorem asserts the decision fires for every type whose
  non-null head is a boxed-number form, at every nullness. Code ground truth
  falsifies the nullness generalization: `gen_value_to_proof`
  (types/provers.ml:78-85) returns **Unknown** for any
  `is_null = Maybe_null _` type, so `prove_is_a_boxed_<nnk>` never proves on
  a maybe-null param and `make_optimistic_number_decision` cannot fire —
  yet the theorem claims `Unbox (UD_number …)` there. Internal tell: the
  DS_number equation shape (Unboxing.v:448-449) is built with `Not_null`,
  so the file itself only ever equates a non-null box.
- Why it matters: a conjectured `Theorem … Admitted` carrying the rule id is
  the mechanization's assertion of the doc's conjecture; as stated it is a
  strictly stronger conjecture that is provably false of the code it cites.
  Anyone later instantiating `unbox_dec` faithfully inherits an unprovable
  (indeed refutable) obligation under this rule's name.
- Candidate fix: pin `inull = Not_null` (minimum), and preferably pin the
  head to the unknown-payload form the doc names (payload `Oub_unknown` at
  the nnk, alloc mode unknown) so the statement is exactly clause (1). The
  arbitrary-payload generalization is plausibly true (the prover ignores
  contents) but it is not the doc's claim; if Girard wants to keep it, the
  ENCODING NOTE must own the strengthening explicitly.
- Status: RESOLVED — green-verified (Unboxing.v:266-341): accum_unknown_head
  pins the payload to unknown_of_kind of the naked kind, the theorem pins
  Not_null and No_alias, and the ENCODING NOTE calls Not_null load-bearing
  (consistent with gen_value_to_proof's Maybe_null gate, provers.ml:81-84).
  The alloc mode stays universally quantified by my explicit acceptance:
  conclusion-side disclosed slack in what the relation may produce
  (config-dependent Alloc_mode.For_types.unknown, provers never consult
  it), unlike DeadBinding's rejected premise-side forall-fl. 2026-07-18.

### KF-021 — S_Unbox_Denv_Equation performs the meet in an env where the component parameters need not exist (medium; draft-stage)
- Chapter: 12-unboxing.md:504-523 (S.Unbox.Denv.Equation: premise "component
  parameters epa̅ (**defined as extra variables of their kinds in the
  denv**)", conclusion added by meeting param's type with T; cf. the same
  pattern spelled out in S.Unbox.Optimistic.Block :193-194, "E' = E extended
  with definitions of the epaᵢ; E' ⊢ param_type ⊓ …"). Rocq:
  theories/Unboxing.v:560-568 (`meet E (tenv_find E (Name_var param)
  K_value) T (Meet_ok Tm eps)` — the meet runs in **E**, while the
  first conjunct requires the ud_vars bound only in **E_handler**). Owner:
  Girard. Code: build_unboxing_denv.ml `denv_of_decision` defines the extra
  variables in the denv FIRST, then `add_equation_on_var` meets there.
- What differs: T is built from alias types of the component parameters
  (decision_shape), so the meet's right operand mentions names that E is
  never required to bind. The doc/code sequence is: extend the env with the
  component definitions, then meet in the extended env. MeetJoin.v's meet is
  env-dependent (alias resolution, record_on extensions), so meeting
  alias-bearing types in an env without those names is at best
  underconstrained and not the documented operation.
- Why it matters: this Definition IS the artifact for a normative rule, and
  it is also a premise of rw_unbox — both the equation's meaning and the
  rewrite's admissibility inherit the wrong-env meet.
- Candidate fix: introduce the intermediate env explicitly — e.g.
  `exists E_mid, (forall x, In x (ud_vars U) -> name_bound_in E_mid
  (Name_var x)) /\ meet E_mid (tenv_find E_mid (Name_var param) K_value) T
  (Meet_ok Tm eps) /\ tenv_find E_handler (Name_var param) K_value = Tm`
  with E_mid = E plus the component definitions (however Girard prefers to
  relate E_mid to E; the load-bearing point is that the meet's env binds the
  ud_vars).
- Status: RESOLVED — green-verified (Unboxing.v:595-608): exists E_mid
  binding the flattened ud_vars with param's type pinned equal to its type
  in E; the meet runs in E_mid; E_handler binds the components and gives
  param the met type Tm. ENCODING NOTE describes denv_of_decision's
  define-first order. 2026-07-18.

### KF-022 — decision_shape pins the closure/variant maps in one direction only; slack undisclosed (medium-low; draft-stage)
- Chapter: 12-unboxing.md:512-515 (shape table: "Closure_single_entry →
  closure_with_at_least_these_value_slots; Variant → variant with
  const_ctors / non_const_ctors **from the field epas**"). Rocq:
  theories/Unboxing.v:511-536 (DS_closure: `forall w fd, In (w, fd) vws ->
  vst w = Some (fd_alias_ty fd)` — vst may bind arbitrarily many OTHER
  slots; DS_variant: `known t = Some …` required only for t ∈ fbt — known
  may map extra tags, and `other` is a free variable where DS_block pins
  `Ob_bottom`). Owner: Girard.
- What differs: the code builds these types with EXACTLY the decision's
  slots/tags (the "at least" in the closure type's name is the row-like
  index, which the encoding separately carries as `Rl_at_least socc`); the
  relational encoding admits Ts with extra value slots, extra block tags,
  and a non-bottom variant `other`. The ENCODING NOTE (:488-493) discloses
  slack for alloc modes, the variant's immediates arm beyond CC_zero,
  extensions/uniqueness, and the closure entry's function/closure type
  maps — but NOT for extra vst slots, extra known tags, or `other`; it
  claims "the per-field/per-slot component aliases … is pinned", which is
  only half true (pinned where present, presence not bounded).
- Why it matters: decision_shape sits under the existential in the
  normative Denv.Equation, so any admissible T justifies the equation —
  including Ts strictly stronger (extra required slots; in the limit,
  near-bottom) or weaker (extra possible tags) than the documented one,
  and thence rw_unbox instances rewriting under equations the pass never
  installs. The DS_block/DS_variant `Ob_bottom` asymmetry reads as
  oversight rather than intent.
- Candidate fix: add the reverse bounds — DS_closure:
  `forall w ty, vst w = Some ty -> exists fd, In (w, fd) vws /\ ty =
  fd_alias_ty fd`; DS_variant: the analogous iff on `known` plus
  `other = Ob_bottom` (matching DS_block). Alternatively extend the
  ENCODING NOTE to own the one-directional pinning explicitly (weaker,
  not recommended for a normative rule's ingredient).
- Status: RESOLVED — green-verified (Unboxing.v:506-565): DS_closure's vst
  and DS_variant's known both carry the reverse inclusion (exact
  correspondence both directions); the `other` binder is gone with
  Ob_bottom pinned in DS_variant's conclusion; the 6.2 ENCODING NOTE now
  says "pinned in BOTH directions ... unlisted-tags arm is Ob_bottom" with
  the remaining slack (alloc modes, immediates arm beyond CC_zero,
  extensions/uniqueness, fts/cts) disclosed. 2026-07-18.

### KF-023 — code0_wf drops the region-distinctness conjunct on a false unstatability claim (medium)
- Chapter: the W-23 family — the doc's alpha-fiat binder discipline
  (11-inlining.md:99-103); main's W-23 ruling spec'd FOUR conjuncts
  including my_region ≠ my_ghost_region. Rocq: theories/WellFormed.v:580-602
  (`code0_wf` — three conjuncts; the comment at :591-595 claims the fourth
  "is not statable: code0 (Syntax.v) deliberately elides the region
  binders"). Owner: Church. Ground truth: Syntax.v:775-786 — `Mk_code0`
  carries `my_alloc_mode : alloc_mode_app`, and Inlining.v:556-563
  (`region_rename`) pattern-matches it as `App_local x_myregion x_myghost`:
  the region binder NAMES are in the model, threaded through the alloc-mode
  field rather than as separate record fields.
- What differs: the conjunct is statable today:
  `match c0_my_alloc_mode c with App_local r g => r <> g | _ => True end`.
  Its omission leaves W-23 channel (iii) unguarded: for a code0 with
  x_myregion = x_myghost = z and a Local alpha^c, `region_rename`'s inner
  `rename_var z region` consumes ALL z-occurrences, so the callee's ghost
  uses land on the caller's ordinary region — an unsound inlined_body that
  no rw_inline premise excludes (callee_binders has no region names;
  region_targets_fresh checks caller-side targets only, and KF-017's
  cross-rename disequalities don't cover same-source aliasing).
- Why it matters: exactly as load-bearing as the three conjuncts that were
  kept — the whole point of routing W-23 through code0_wf was that ch. 13's
  C-tying WF hypothesis delivers all the binder discipline rw_inline
  assumes. Also, the escalation to main currently argues from a false
  premise, and the comment would mislead the domain expert reading
  WellFormed.v.
- Candidate fix: add the match-form conjunct above and reduce the comment's
  caveat to "stated on the alloc-mode field, where code0 carries the region
  names". No Inlining.v change needed.
- Status: RESOLVED — Church landed the fix and I verified it on disk
  (WellFormed.v:599-608): the fourth conjunct is the match form verbatim
  (`match c0_my_alloc_mode c with App_local r g => r <> g | _ => True end`),
  and the comment (:591-596) now states it on the alloc-mode field with the
  KF-023 unsoundness scenario cited. Church independently re-verified the
  region_rename mechanism (Inlining.v:556-563, two sequential rename_var
  calls) before editing. W-23 conjunct (iii) is now guarded; W-23 closes on
  the ch. 13 conjunction check at Soundness.v increment 2b. 2026-07-18.

### KF-024 — S.Rewrite.LetCont.Shortcut drops the binder while Apply-position uses of k dangle (medium; draft-stage)
- Chapter: 10-simplify-rewrites.md:806-821 (conclusion at :814 drops the
  binder; NOTES distinguish general shortcuts from pure aliases /
  `to_alias`). Rocq: theories/RewritesControl.v:1139-1159
  (`S_Rewrite_LetCont_Shortcut`, conclusion `shortcut_apply_cont k ps k'
  aargs e` with no surrounding Let_cont); `retarget_apply_cont`
  (:225-255) rewrites expression-position apply_conts and switch arms
  only — `E_apply _ => e`. Owner: Church.
- What differs: an Apply in e with `RC_return k` (or exn-continuation k)
  is never retargeted — a general shortcut with arg substitution CANNOT
  retarget an Apply position (results are passed implicitly; only the
  ā = p̄ pure-alias case can, via UE.resolve_continuation_aliases). The
  conclusion drops the binder anyway, leaving the surviving use dangling
  — or, worse in the nominal encoding, captured by an outer continuation
  that happens to share k's name.
- Why it matters: the rewrite produces an ill-scoped term from a
  well-formed one, so INV.Simplify.Preserves and any WF-preservation
  statement over rw_control are refutable through this constructor. The
  implementation never does this: apply_continuation_shortcuts retargets
  jump sites and `rebuild_let_cont` drops the binder only at zero
  remaining occurrences.
- Note: the DOC's conclusion has the same shape (binder gone,
  substitution over apply_cont sites only) — the imprecision is
  doc-rooted; escalated to main as a doc-precision candidate alongside
  the .v fix.
- Candidate fix (doc-conclusion-preserving): add the premise
  `uses_ok k (fun _ => True) (fun _ => True) (fun rc => rc <> RC_return k)
  (fun ec => ec_exn_handler ec <> k) e` (the same Apply-exclusion shape
  UnusedParam/AliasedParam already state). Alternative: keep the binder
  in the conclusion and let DeadHandler drop it (closer to the
  implementation, further from the doc's conclusion).
- Status: RULED by main (2026-07-18): option (b) — the DOC fix keeps the
  Let_cont binder in Shortcut's conclusion (matching rebuild_let_cont's
  keep-until-zero-occurrences behavior; composition with
  LetCont.DeadHandler recovers the old conclusion exactly when it was
  valid), eliminating the dangling-k unsoundness structurally. My
  Apply-exclusion premise candidate is superseded (it would encode a
  restriction the code doesn't impose). Process: Church drafts the
  before/after rule-text lines (conclusion + a NOTES sentence citing
  rebuild_let_cont and the DeadHandler composition), I verify closure,
  main applies; Church's .v mirrors keep-binder.
  .v SIDE RESOLVED — verified on disk (2026-07-18): Church's interim
  uses_ok-premise fix was superseded and removed; S_Rewrite_LetCont_Shortcut
  (RewritesControl.v:1276-1288) keeps the binder, and a pure-alias
  companion S_Rewrite_LetCont_Shortcut_Alias (:1300-1316, rw_unbox
  two-constructors-one-id precedent) handles the to_alias case: handler
  jump args exactly params_as_args ps, conclusion composes
  retarget_apply_aliases over shortcut_apply_cont (disjoint site classes:
  apply_cont/switch sites via the substitution, Apply return/exn via the
  alias traversal). retarget_apply_aliases (:312-389) is scope-aware with
  exactly replace_apply_cont's stopping discipline (nonrec handler always
  descended, body stopped on k rebinding; rec group stops both; code0
  kret/kexn guard) and preserves exn extra args. Code cites verified
  verbatim: expr_builder.ml:558-573 (apply_continuation_aliases /
  apply_exn_continuation_aliases retarget only through to_alias;
  with_exn_handler keeps extra args), continuation_shortcut.ml:59-63
  (to_alias = Some iff args = params-as-simples). DOC half still open:
  Church's before/after rule-text draft is with main; my closure
  verification fires when the amended doc text lands.
  RESOLVED (2026-07-18): doc half verified against the applied chapter
  text. 10-simplify-rewrites.md:806-828 — conclusion :815 keeps the
  binder over the substituted body (exactly the .v's
  `shortcut_apply_cont` composition); NOTES :818-823 state the
  keep-binder rationale (substitution reaches apply_cont uses only,
  Apply return/exn untouched, rebuild_let_cont keeps until
  num_free_occurrences_of_cont_in_body = Zero, DeadHandler composes);
  NOTES :823-825 state the ā = p̄ to_alias case naming
  apply_continuation_aliases / apply_exn_continuation_aliases; fourth
  CODE anchor continuation_shortcut.ml#to_alias at :810, mirrored in
  the .v header (RewritesControl.v:1257-1262). Both halves closed.

### KF-025 — S.Rewrite.Apply.Invalid's arity disjunct licenses Invalid on every valid over/partial application (medium-high; draft-stage)
- Chapter: 10-simplify-rewrites.md:1059-1074 (third premise: "an
  argument's or result's kind/arity provably mismatches the callee").
  Rocq: theories/RewritesControl.v:1447-1469 (third disjunct:
  `~ equal_ignoring_subkinds (unarize args_arity) (unarize params_arity)
  \/ ~ equal_ignoring_subkinds (unarize return_arity) (unarize
  result_arity)`). Owner: Church. Ground truth:
  simplify_apply_expr.ml:828-848 (`arity_mismatch`) and :913-1000
  (dispatch).
- What differs, params half: the code's Invalid trigger is
  `arity_mismatch`, a PREFIX, PER-PARAMETER comparison — it walks
  `unarize_per_parameter` lists in lockstep and returns false when
  either list runs out (`| [], _ | _, [] -> false`); a mismatch is a
  parameter (within the common prefix) whose unarized component list
  differs in LENGTH or in some component KIND (K.equal after dropping
  subkinds). Mere arity inequality is NOT a mismatch: an over-application
  (args ⊃ params, prefix-compatible) and a partial application
  (args ⊂ params) both pass and dispatch to their own rewrites
  (:962-995). The .v's flat-inequality disjunct fires on all of them.
- What differs, result half: the result-arity check runs ONLY in the
  exact branch (`provided_num_args = num_params`, :931-946); the code
  comment says explicitly it "can only be performed for exact
  applications" (over-applications legitimately record a different
  return arity on the apply). The .v states it unconditionally.
- Why it matters: rw_control is a licensing relation; as stated it
  licenses rewriting semantically valid over/partial applications to
  Invalid, which changes behavior — INV.Simplify.Preserves (ch. 13)
  quantifies over rw_control derivations and becomes refutable. Also
  makes OverApplication/PartialApplication and Invalid simultaneously
  derivable on the same term with contradictory conclusions.
- Candidate fix: replace disjunct 3's mismatch with (i) an existential
  prefix witness — some j with `nth_error` defined in BOTH
  per-parameter unarizations whose component lists differ in length or
  in some kind ignoring subkinds — OR (ii)
  `num_params (ap_args_arity ap) = num_params (c0_params_arity cd)
  /\ ~ equal_ignoring_subkinds (unarize (ap_return_arity ap))
  (unarize (c0_result_arity cd))`. A small
  `arity_prefix_kind_mismatch` Definition mirroring arity_mismatch
  keeps it readable. (The flat `unarize` in (ii) is a benign
  under-approximation of the code's structural comparison; fine to
  keep.)
- Status: RESOLVED — fix verified on disk (RewritesControl.v:728-735,
  :1615-1638): arity_prefix_kind_mismatch is False on either-empty and
  compares unarize_component per parameter over the common prefix (exactly
  arity_mismatch's `[], _ | _, [] -> false` + per-parameter length/kind
  compare, simplify_apply_expr.ml:832-848, Church re-verified from disk);
  the result disjunct is now guarded by num_params equality (the code's
  provided_num_args = num_params, exact-only). Order of the two arity
  arguments is immaterial (prefix mismatch is symmetric). Comment rewritten
  correctly (plain length difference = over/partial, never Invalid).
  2026-07-18. Draft-stage verification; re-check at green with the file.

### KF-026 — PartialApplication's stub-internal binder distinctness is unstated but model-instantiable (low-medium; draft-stage)
- Chapter: 10-simplify-rewrites.md:1037-1056 (stub takes the remaining
  params, captures the supplied args, body is the full application).
  Rocq: theories/RewritesControl.v:1386-1433
  (`S_Rewrite_Apply_PartialApplication`); `bind_projections` (:640-649).
  Owner: Church.
- What differs: the ENCODING NOTE declares freshness of "the slot names
  and the stub-internal binders" prose-level "(the stub is closed)" —
  but unlike cid'/fs' (unconstrained atoms a skeptic cannot exploit),
  `xf`, `myc`, `mydep`, `map fst cs`, and `ps_rem`'s variable names are
  ordinary quantified variables of the constructor. Instantiating a
  ps_rem name equal to xf (or a cs name, or myc) makes a
  bind_projections let shadow the stub's parameter, and the full
  application passes the projection instead of the parameter — a
  wrong-value stub derivable inside the model. Likewise the value-slot
  table `(wf, f) :: combine (map snd cs) (ap_args ap)` needs
  `NoDup (wf :: map snd cs)` or colliding slots alias the wrong
  captures (checklist channel 3, association-table distinctness).
- Why it matters: checklist channel 1 (binder-vs-argument capture)
  applied to a stub the constructor itself assembles; the soundness
  obligation on this rule would be refutable via the degenerate
  instantiation even though every compiler-produced stub is fresh.
- Candidate fix: two premises,
  `NoDup (myc :: mydep :: xf :: map fst cs ++ map fst ps_rem)` and
  `NoDup (wf :: map snd cs)`; shrink the ENCODING NOTE's prose-freshness
  claim to cid'/fs'/kret'/kexn'.
- Status: RESOLVED — fix verified on disk (RewritesControl.v:1567-1569):
  both NoDup premises verbatim; ENCODING NOTE's prose-freshness claim
  shrunk to cid'/fs'/kret'/kexn' with the shadowing scenario cited
  (:1541-1545). 2026-07-18. Draft-stage; re-check at green.

### KF-027 — ValueSlotExempt's iff is refutable when the pattern binds my_closure (low; draft-stage)
- Chapter: 10-simplify-rewrites.md:1134-1161 (premise: the converted
  body binds each CAPTURED variable v — necessarily distinct from
  my_closure). Rocq: theories/RewritesControl.v:799-805
  (`S_Rewrite_Loopify_Attribute_ValueSlotExempt`, an Admitted iff over
  arbitrary pattern p). Owner: Church.
- What differs: with p a pattern binding x (my_closure itself), the
  left side holds vacuously (x's body occurrences are shadowed, so the
  erased let-term has no free x) while the right side can fail — the
  forward direction of the iff is false. The doc's premise shape rules
  this out (v is a captured variable, not my_closure).
- Why it matters: Admitted-with-refutable-statement is the KF-020
  class; the Believers-vs-Skeptics protocol treats the stated form as
  the claim.
- Candidate fix: add `~ In x (bound_pattern_vars p)` (matching the
  doc's fresh-v premise).
- Status: RESOLVED — fix verified on disk (RewritesControl.v:912-919):
  the premise is in and the comment explains the vacuous-by-shadowing
  failure mode. 2026-07-18. Draft-stage; re-check at green.

### KF-028 — rel_of_const answers is_int = 1 for Const_null, inverting the reducer the doc pins (medium)
- Chapter: 07-types-domain.md:287-318 (T.Grammar.NakedImmediate.Relational,
  NORMATIVE — pins the meet-time reducer's resolution "via Relation.of_const
  (Is_int ↦ not is_null, Is_null ↦ is_null, Get_tag ↦ Bottom)" at :305-307,
  and its NOTES at :310-312 explicitly flag that the BUILDER
  is_int_for_scrutinee (`= true` even for null) and the REDUCER
  Relation.of_const (`= false` for null) DISAGREE for a null constant).
  Rocq: theories/MeetJoin.v:1135-1144 — `rel_of_const` returns
  `Rel_is_int => Some 1` for every constant, and the comment calls the
  unconditional `= true` "the code's documented behavior". Owner: Scott
  (delegated here by TypeGrammar.v:150-169's promise that MeetJoin.v models
  the pinned behaviors). Ground truth: type_grammar.ml:106-114
  (`Relation.of_const`: `Is_int -> Ok (bool (not (RWC.is_null const)))`,
  with a code comment stating 1 iff not null); the reducer call site is
  meet_and_join.ml:771-824 (`reduce_head_of_kind_naked_immediate`, of_const
  at :786).
- What differs: for Rel_is_int applied to Const_null the model resolves to
  {1} where code and doc resolve to {0}. The comment conflates the two code
  paths: the BUILDER's unconditional `= true` is real (type_grammar.ml:
  4287-4301) and is correctly modeled by the separate `is_int_for_scrutinee`
  (:1157-1165); `rel_of_const` models the REDUCER, whose documented behavior
  is the opposite for null.
- Why it matters: {1} vs {0} is not slack in either sound direction — it
  excludes the true value (is_int(null) = 0 at runtime). Today the only
  consumer is the T.Meet.Relational documented anchor's ENCODING NOTE
  (:1186-1192), so no theorem is refutable yet; but these helpers are the
  interface ch. 10's Is_int/Switch rewrites are expected to consume
  (RewritesPrim.v, in flight), and the definition sits under a NORMATIVE
  doc pin, so the drift would propagate silently into normative territory.
  The wrong comment would also mislead any reader auditing against the doc.
- Candidate fix: `| Rel_is_int => Some (1 - rwc_is_null c)` (or the match
  form on Const_null), and reword the comment: builder anomaly stays with
  is_int_for_scrutinee; rel_of_const follows of_const (`= false` for null).
  Optional one-liner: Rel_get_tag's `None` deliberately fuses of_const's
  Bottom with the reducer's discard-Bottom step (meet_and_join.ml:789-795,
  doc :307-309) — net effect identical, worth saying so nobody reads None
  as "of_const has no answer".
- Status: RESOLVED — Scott's fix verified on disk (2026-07-18, pre-green;
  Hopper recompile pending, re-check at green): MeetJoin.v:1148 now reads
  `Rel_is_int => Some (1 - rwc_is_null c)` (reducer semantics, 0 for
  Const_null); the comment (:1135-1144) states the builder/reducer
  disagreement explicitly, keeps the builder anomaly with
  is_int_for_scrutinee (unchanged, correctly `1` for consts), and carries
  the Rel_get_tag fusion line (of_const Bottom fused with the reducer's
  discard, net identical). Credit: Plotkin flagged the comment's
  "documented behavior" claim for a line-level cite during the co-read
  split; the cite refuted it. GREEN CONFIRMED (2026-07-18): Hopper's 02:34
  full sweep compiled MeetJoin.v (source 02:31:34, fix grep-verified at
  :1148), downstream rebuilt green twice since; Dijkstra's ch. 08
  content-delta re-stamp done (30 headers / 12 Admitted unchanged).
  Fully stamped. Sequencing note relayed to Plotkin: Curry independently
  confirmed no rw_prim arm consults rel_of_const, and stored_type
  over-approximates constant canonicals to unknown_of_kind, so
  prove_is_int answers Unknown for constant scrutinees (null included) —
  the poisoned-composition path could not reach his fold arm even
  pre-fix; increment 2 was never actually blocked.

### KF-029 — RF_alias omits the mode floor reify applies to its Simple result (low-medium)
- Chapter: 08-meet-join.md:802-825 (reify "attempts to turn a type back
  into a term"; T.Reify.Sound) with the boundary discipline stated at
  :694-724 (T.Prove.SimpleModeBoundary: every Simple-returning channel
  into rebuilt terms carries a min_name_mode floor). Rocq:
  theories/MeetJoin.v:1691-1695 (`RF_alias : can_alias E T = Some s ->
  reify E T (R_simple s)`) with `can_alias` (:59-63) returning the raw
  canonical, no floor. Owner: Scott. Ground truth: reify.ml:214
  (`let min_name_mode = Name_mode.normal`), :242
  (`TE.get_alias_then_canonical_simple_exn env ~min_name_mode`), and the
  try_canonical_simple fallback in the same function — every R_simple the
  code returns has passed the Name_mode.normal floor.
- What differs: the model's reify admits returning a join-introduced
  In_types existential as R_simple for term substitution; the code's mode
  floor filters exactly that (degrading to Cannot_reify). T_Reify_Sound's
  value-equality conclusion stays provable (the equation is still true),
  which is precisely why this slack is silent — the broken invariant is
  term WF (an In_types existential leaking into rebuilt terms), the "WF
  violation, not a mere imprecision" the SimpleModeBoundary NOTES call out.
- Why it matters: reify is a term-rebuild channel the doc's boundary rule
  doesn't list (its premise names provers only), so nothing downstream
  re-imposes the floor; a ch. 10/13 consumer of `reify` inherits the leak.
  NOTE: `can_alias` itself must NOT gain the floor — its meet-side uses are
  faithful to the code, which resolves canonicals in meets via
  get_canonical_simple_ignoring_name_mode (meet_and_join.ml:782).
- Candidate fix: add the PES_alias mode-floor side condition to RF_alias
  (const, or binding time not below te_min_binding_time — the same
  disjunction at :1481-1484); alternatively a site ENCODING NOTE disclosing
  the slack and the reify.ml floor, if Scott prefers the envelope loose.
- Status: RESOLVED — Scott's fix verified on disk (2026-07-18, pre-green;
  re-check at green): RF_alias (MeetJoin.v:1711-1717) now carries exactly
  PES_alias's mode-floor disjunction (simple_is_const, or binding time not
  below te_min_binding_time), with a site comment citing reify.ml's
  ~min_name_mode and the SimpleModeBoundary leak; can_alias stayed
  floorless (meet-side code-faithful), as instructed. Companion K1 also
  landed: the reify ENCODING NOTE (:1693-1702) now states the contract
  that any future R_lift constructor must land together with the Lift
  clause in T_Reify_Sound. K2 landed (prove_get_tag comment :1417-1423
  discloses the In-superset vs exact all_tags slack with the code cite).
  K3 declined by Scott (would reopen a Qed'd envelope on a closed
  chapter); reliance on T.Gamma.Value.Nullability stays comment-disclosed
  — acceptable; routed to main's FINAL POLISH list next to the
  CoercionErasure upgrade if that list is actioned. GREEN CONFIRMED
  (2026-07-18): same 02:34 sweep as KF-028 (RF_alias floor grep-verified
  at :1711); Dijkstra ch. 08 re-stamp done. Fully stamped.

### Finding #16 verification — CSE of immutable Make_block vs deterministic Phys_equal (doc-level design conflict; CONFIRMED refutable)
- Provenance: surfaced by Curry's RewritesPrim.v transcription (FINDING #16
  comment at theories/RewritesPrim.v:363-373, the S.Rewrite.CSE.Eligible
  site); verification assigned to me by main (2026-07-18). Not a KF entry:
  both rules are faithful transcriptions — the conflict is between two
  correct doc rules and INV.Simplify.Preserves as stated.
- Refutation path CONFIRMED, end-to-end code-grounded. Witness:
  `let x = Make_block(0, Immutable, Heap)[a] in
   let y = Make_block(0, Immutable, Heap)[a] in
   let r = Phys_equal Eq x y in …r observed (e.g. in the module block)…`
  (a) Source semantics: each Make_block allocates fresh, so the two
  locations are distinct in EVERY run; P.Binary.PhysEqual
  (06-primitives-memory.md:1185-1204) is deterministic word equality, so
  r = 0. No undefined behaviour anywhere, so the "modulo UB" clause of
  INV.Simplify.Preserves (13-soundness.md:41-60) does not shield the
  rewrite. (b) No premise blocks CSE: eligibility holds
  (flambda_primitive.ml:2442-2445, Make_block(_, Immutable, Heap) with a
  var arg; Eligible_for_cse.create:2955-2961 explicitly admits
  (Only_generative_effects Immutable, No_coeffects) — comment "Allow
  constructions of immutable blocks to be shared"); Extend records x's
  application, Replace fires on y's (identical canonicalized prim, s = x
  in scope, NM_normal). Result: y := alias of x. (c) Post-rewrite, r = 1 —
  and the compiler FOLDS it at compile time: prove_physical_equality
  (provers.ml:1491-1514) proves true for identical variable aliases
  (:1507-1512), and simplify_phys_equal
  (simplify_binary_primitive.ml:972-995) rewrites to the constant. 0 → 1
  observable flip; INV.Simplify.Preserves refuted as stated.
- How the real compiler squares it: the language license. The OCaml manual
  specifies (==) on immutable values as implementation-dependent
  (guaranteeing only e1 == e2 ⟹ structural equality), so the flip is a
  permitted implementation choice, not a compiler bug. The fold direction
  is disciplined in exactly the way that keeps this coherent:
  prove_physical_equality proves DISequality only from content/kind
  incompatibility (disjoint numeric/string sets :1279-1298, tag/shape/size
  mismatch :1399-1411, kind mismatches, distinct symbols/consts
  :1493-1505) and returns Unknown for same-shape compatible blocks
  (:1399-1407 explicitly) — it NEVER derives `false` from "two distinct
  allocations". So every phys-equal fact the compiler generates is stable
  under the sharing CSE introduces; sharing only moves results from 0 to
  1, in the direction the license allows. The doc's CSE.Replace NOTES
  sentence "identity … is not observable" is the one over-claim: identity
  IS observable via phys_equal; what is true is that the change is
  language-licensed. Related identity-affecting mechanisms under the same
  license (for the entry's generality): lifting to statics, unboxing's
  re-boxing at use sites.
- Resolution is a design decision (escalated to the user via the final
  report). Candidate entry for 13-soundness.md §4 drafted with Curry
  (2026-07-18); options recorded there: (a) qualify §1 observational
  equivalence "up to physical identity of immutable values", (b) make
  ⟦Phys_equal⟧ nondeterministic when not provably same-word, (c) record
  only. Status: ENTRY APPLIED — main applied the co-signed draft to
  13-soundness.md §4 as item 8 (:647-689); I verified the applied text on
  disk (2026-07-18): my draft verbatim modulo line-wrapping, plus Curry's
  in-model-reproducibility clause (S_Rewrite_CSE_Replace installs the
  alias, S_Rewrite_Prim_PhysEqual_Equal derives the fold — composing the
  two Rocq rules exhibits the flip). All five survival asks present
  (Eligible_for_cse.create cite + comment, compile-time-fold point with
  provers.ml/simplify_binary_primitive.ml cites, disciplined-disequality
  argument, NOTES correction, three open options). Follow-up: proposed a
  one-line cross-reference amendment to main linking item 8 to its
  -Oclassic sibling 14-validation/classic_physequal_box.md — same
  over-specified P.Binary.PhysEqual refuted at the to_cmm stage in the
  OPPOSITE direction (Delay-duplication splits identity, 1→0; CSE-sharing
  merges it, 0→1), and that record's repair #1 IS item 8's option (b), so
  the resolution should be chosen jointly (20-to-cmm-soundness.md's
  INV.ToCmm.EffectLinear NOTES at :325-334 already states the shared
  license and points at §5.6). Resolution options stay with the humans.

### KF-030 — INV_Rewrite_Local's union claim silently narrows the doc's rule quantification; the omission is load-bearing (medium)
- Chapter: 13-soundness.md:106-124 — §2 and INV.Rewrite.Local quantify the
  local obligation over "Each rule of the E ⊢ e ⇝ e′ judgment (all of
  S.Rewrite.*, S.Inline.*, S.Unbox.*)". Rocq: theories/Soundness.v
  INV_Rewrite_Local (:568-585) premises `rewrites fl C E e e'`, with the
  header comment (:537-543) saying the doc's quantification is "modeled as
  the rewrites union, Simplify.v, whose constructors carry each rule's
  side conditions" — no exclusions mentioned. Owner: Plotkin.
- What differs: the union deliberately excludes S.Rewrite.CSE.Replace /
  CSE.Extend (composed by cse_deep; table is per-position state) and
  S.Rewrite.Loopify.SelfTailCall (composed by stc_deep under loopify
  ambient state) — all e ⇝ e′ rules under the doc's quantification.
  (Loopify.Body / Code.RecursiveRecompute are also outside, but they are
  code0-level rules, not of the e ⇝ e′ judgment — defensible without
  disclosure.) Simplify.v discloses the exclusions for the UNION's
  purposes; Soundness.v's Local claims doc-quantification coverage via
  that union without carrying the carve-out.
- Why it matters, twice over: (1) the factoring narrative ("the
  per-rewrite obligation the headline theorem factors through") breaks
  exactly at Preserves' S_cse and simplify_code arms — an induction over
  simplifies gets no per-arm lemma from Local for those constructors, so
  a reader trusting the comment mis-plans the proof; (2) the omission is
  load-bearing, not innocent: CSE.Replace is the one rule whose local
  obligation is FALSE as stated (finding #16 / 13-soundness.md §4 item 8:
  plug Cx = let x = Make_block(0,Immutable)[a] in [.], e = the second
  identical binding with phys_equal x y observed — obs_equiv fails with
  zero UB). Had rw_cse been a union arm, INV_Rewrite_Local would be
  refutable today; the exclusion quietly dodges that, which is exactly
  the kind of thing an ENCODING NOTE must say out loud. SelfTailCall's
  exclusion is sound-side (its obligation, posed with the ambient
  indices, is presumably true) but still inside the doc's quantifier.
- Candidate fix (disclosure, no statement change): ENCODING NOTE on
  INV_Rewrite_Local listing the excluded families and why each cannot be
  posed in this form — CSE.Replace needs a table-validity hypothesis
  (which lives in cse_deep's fempty-reachability discipline, Simplify.v),
  and its instance is refutable as stated pending the item-8 design
  decision (cite 13-soundness.md §4 item 8); SelfTailCall needs the
  loopify ambient state (k, my_closure, kret, kexn) and the scope-stopped
  traversal; Loopify.Body/RecursiveRecompute are not e ⇝ e′ rules.
  Optional strengthening (Plotkin's call): a separate CSE-local
  obligation stated over cse_deep with the table hypothesis, conclusion
  weakened per whichever item-8 option the humans pick — I'd hold off
  until that design decision lands.
- Status: resolved (2026-07-18) — disclosure fix verified on disk
  (Soundness.v:569-588): names both excluded families with their
  composition sites (cse_deep, stc_deep), states CSE.Replace's
  local-form instance is FALSE with the Make_block(0,Immutable) context
  witness, cites this entry for the item-8 decision, spells out the
  factoring consequence (no per-arm lemma at the S_cse /
  simplify_code arms), and closes with the Body/RecursiveRecompute
  no-defense-needed note. No CSE-local obligation stated, per my
  recommendation, pending item 8. Green-review low notes also verified:
  L1 at the Preserves note (:508-510, "benignly, since kind pathologies
  surface as stuck runs, which no_ub excludes") with the Local statement
  deferring there by reference — accepted, no duplication needed; L2 leg
  (d) in TrapNeutral's anchor (:874-877).
- COROLLARY DELTA (2026-07-22, item-8 resolution; ruling (d)): the
  design decision this entry held its optional strengthening on has
  LANDED, and entry 55's disclosure is REWRITTEN accordingly
  (verified, no drift): the union exclusion now rests on the
  STRUCTURAL ground alone (denv-state table, side judgments — stands,
  item-8-independent); the second ground is DISSOLVED (under
  Rewrite.Local's refinement reading the one-let witness exhibits
  {true} ⊆ {true, false}, so CSE.Replace's local obligation is
  true-shaped); and the held-off CSE-local obligation — Plotkin's
  optional strengthening, my hold-off — is now STATABLE on rw_cse
  and booked as a wave/follow-up item. WAVE RIDER filed with main:
  Soundness.v:569-588's disclosure comment (this entry's verified
  fix) still states the local-form instance "is FALSE with the
  Make_block(0,Immutable) context witness" — post-resolution that
  comment must mirror entry 55's rewrite; it is MISSING from record
  75's mechanization plan (which covers the RewritesPrim.v comment
  and the FINDING-16 pointer but not this one).

### KF-031 — tc_raise_kind_opt defaults an absent raise kind to Raise_regular; the code defaults to Raise_notrace (low)
- Chapter: 16-to-cmm-control.md:235-254 (TC.ApplyCont.Raise; the doc
  conclusion writes `raise_kind` without stating the None default).
  Rocq: theories/ToCmmControl.v:136-141 (`tc_raise_kind_opt`, None =>
  Raise_regular; Milner's own fidelity flag at :134-135). Code ground
  truth: translate_raise (to_cmm_expr.ml:582-590) maps the Pop's kind
  through `Trap_action.Raise_kind.option_to_lambda`, whose None case is
  **Raise_notrace** (trap_action.ml:39-44). Owner: Milner.
- What differs: under `debug_flag = true` and an absent source kind, the
  model emits `Craise Raise_regular` where the code emits
  `Craise Raise_notrace` — the translation relation fails to cover the
  real output for kind-less Pops compiled with -g.
- Why it matters: backtrace-recording only (the doc's own NOTES: "not
  control flow"), and only in debug builds — hence low — but a coverage
  miss in a relation ch. 20 poses simulation against is still a miss.
- Candidate fix: one constructor — `| None => Raise_notrace`.
- Status: RESOLVED — reported to Milner 2026-07-18; fix verified on
  disk 2026-07-18: tc_raise_kind_opt None => Raise_notrace
  (ToCmmControl.v:138-143) with the comment citing option_to_lambda
  via translate_raise (:134-137). The debug gate at
  TC_ApplyCont_Raise (:673-674) untouched, as predicted.

### KF-032 — tc_switch_form freezes concrete two-arm shapes the code (and the doc's own example) escape; the coverage claim in the ENCODING NOTE is wrong (medium)
- Chapter: 16-to-cmm-control.md:311-342 (TC.Switch: two-arm form given
  via SMART CONSTRUCTORS — "C.ite (C.eq (C.int d) sc)" — with the doc
  explicitly naming `(if (!= x 1) …)` as an emitted form, and the
  scrutinee premise "(untagged, or tagged when smaller)"). Rocq:
  theories/ToCmmControl.v:143-219 (tc_disc/tc_retag/tc_switch_form;
  ENCODING NOTE :192-199). Code: to_cmm_expr.ml:1251-1330. Owner:
  Milner.
- What differs, three ways: (a) the ENCODING NOTE claims the doc's
  `(if (!= x d))` form "is the same test with the branches swapped and
  is covered by the arm-choice nondeterminism" — false: branch-swapping
  under TCSw_Two_Eq only changes which discriminant is tested; every
  clause emits `Ccmpi Ceq` or the bare-scrutinee collapse, and no
  derivation produces a `Cne` node, so the doc-named form is
  unrepresentable. (b) In the must_tag case the model demands the
  literal syntactic retag `(sc<<1)+1` (tc_retag = C.tag_int's shape),
  but the code never re-tags: it SELECTS the pre-existing tagged
  expression recorded by Env.extra_info (to_cmm_expr.ml:1254-1267,
  `Some (Untag tagged_scrutinee_cmm)`) — an arbitrary smaller
  expression (typically a Cvar), so real tagged-case outputs are
  unrelated to every clause. (c) C.eq/C.ite are smart constructors
  (the doc's shape language), so further normalized shapes exist that
  the frozen ASTs exclude. Positive results kept: the in-range
  agreement argument among the PERMITTED forms is sound (verified:
  distinct discriminants; tagging injective; TCSw_Two_Zero matches the
  code's 0-collapse priority incl. the tagd-impossible-0 corner), and
  the deterministic arm choice (min_binding) is inside the disclosed
  nondeterminism.
- Why it matters: this is a translation relation ch. 20 relates REAL
  outputs through; every shape the code emits that no clause derives
  makes a real compilation unrelated, and the note affirmatively
  claims coverage it does not have (the ENCODING-NOTE-claiming-more-
  than-delivered class).
- Candidate fix (either): widen — add a Cne clause (swapped branches)
  and replace tc_retag's syntactic form with an existential
  tagged-form premise through a hook that ties value, not shape
  (extra_info's contract); or re-scope — mark the two-arm forms
  UNDER-APPROXIMATION with the catalog-62-style stated obligation
  (real outputs related through a shape-normalization closure, ch. 20
  posed against it). The note's != sentence must be corrected either
  way.
- Status: open, PARTIALLY RESOLVED — reported to Milner 2026-07-18;
  fixes verified on disk 2026-07-18: (a) RESOLVED — TCSw_Two_Ne
  constructor (ToCmmControl.v:224-232, genuine `Ccmpi Cne` node,
  branches swapped `eo et`, same tc_disc<>0 side condition mirroring
  the Eq/Zero split), and the false coverage sentence replaced by
  "NOT derivable by swapping branches under Ceq" (:194-200). (c)
  covered by (a)'s note rewording. (b) Milner took the
  UNDER-APPROXIMATION route (a tagged-form hook Variable would grow
  tc_expr's Section closure 15 -> 16 parameters and break ToCmmData's
  positional instantiation — proportionality accepted); the note
  states the extra_info selection gap precisely (:201-212,
  value-level equality, array_indexing family) and the stated
  obligation is the new kernel TC_Switch_TestByValue (:252-269,
  catalog-62 family, Admitted). HOWEVER my strength check found that
  kernel REFUTABLE as stated — KF-037. KF-032(b) cannot close until
  the kernel is repaired; remains open pending KF-037 only.
  DELTA (2026-07-22, revival re-orientation): the KF-037 residual is
  discharged — KF-037's fix landed, compile-confirmed, and its delta
  explicitly records that KF-032(b)'s stated obligation is now
  sound-shaped. Nothing of this finding remains open; KF-032 CLOSED.

### KF-033 — the extras-reraise call wrapper is owned by no rule anywhere; the composed model classifies defined cross-call exception propagation as UB (medium-high; doc-rooted, escalated)
- Chapter: 16-to-cmm-control.md:299-306 — TC.Apply.Return NOTES
  mention it in one sentence ("translate_apply additionally wraps the
  call in a Ctrywith that reraises with the extras") but no rule
  states it; 18-to-cmm-data.md — silent (grep: no translate_apply /
  reraise / extra_args). Rocq: ToCmmControl.v:676-678 defers it as
  "data-side (translate_apply; ch. 18)" — a misattribution twice over:
  ch. 18 (doc and ToCmmData.v both) never delivers it, and the emitted
  construct is PURE CONTROL (Ccatch/Cexit/Push/Pop/raise — nothing
  representational). Code ground truth: to_cmm_expr.ml:499-570 — when
  `Exn_continuation.extra_args k_exn` is nonempty, the call is wrapped
  in `C.trywith` whose extras-FREE handler (`~extra_args:[]`, :568)
  reraises `Raise_reraise (Cvar exn_var) ~extra_args` (:561), with the
  wrapper's Push/Pop as explicit trap actions on Cexits (:545-548).
  Owner: Milner (both ToCmm files); doc fix: main.
- What differs: in the model, TC_Apply translates an extras-carrying
  Apply with NO wrapper, so the innermost model trap-stack entry at
  the call is the source extras-expecting Ccatch Exn_handler
  (TC_LetCont_Exn binds all params). W-18's Cmm-side analysis already
  established the consequence: CM_Apply_Raise re-raises bucket-only
  ([Cval v_exn], Cmm.v:1111) and CM_Raise requires operand/param count
  agreement (Cmm.v:1214), so a callee's uncaught exception arriving at
  that handler is STUCK => CMO_undef. The real system's outcome is
  defined (the wrapper reraises with extras). Defined outcomes
  absorbed into UB — the Cmm-side twin of KF-019, reachable for every
  try-with whose exn continuation carries extra args and whose body
  makes a call (a standard Simplify output shape).
- Why it matters: no_ub-style licensing and the ch. 20 Beh_exn
  simulation clause are corrupted on exactly these programs; and the
  ToCmmControl.v site note claims delivery by a chapter that does not
  deliver (the KF-030 disclosure class), so nothing in the
  mechanization even records the hole.
- Candidate fix: doc first — a rule in ch. 16 §5 (control-shaped;
  e.g. TC.Apply.ExnWrapper) stating the wrapper: extras nonempty =>
  e_c = Ccatch(Exn_handler, [<lbl_w, [x_exn], Craise Reraise
  (x_exn :: extras_c)>], <push lbl_w; call; pop lbl_w>) around
  TC.Apply.Return's placement, extras translated at the CALL site.
  Then a ToCmmControl.v constructor gating TC_Apply on the extras
  emptiness and adding the wrapper form. Until then, at minimum the
  :676-678 note must say the wrapper is modeled NOWHERE and the
  composed-model consequence.
- Status: open — escalated to main as doc FINDING; reported to Milner,
  2026-07-18. Supersedes W-18 (its conditional resolved: ch. 16's
  lowering DOES emit extras-expecting handlers, and the protecting
  wrapper is absent from the model). DELTA (2026-07-18): main routed
  the doc rule to Milner (draft-verify-apply) with a SEQUENCING
  constraint — the wrapper rule + encoding must land before or with
  #25's apply row, so the simulation is never stated against the gap.
  Rule-block baseline moves 452 -> 453 at regen. The Branching_point
  flush omission (W-22 low note (a)) is on main's
  deferred-clarification list.
  DELTA 2 (2026-07-18): doc rule TC.Apply.ExnWrapper LANDED
  (16-to-cmm-control.md:308-337); my fix verification found the shape
  right in every leg but TWO corrections needed before the .v
  constructor bakes them in: (1) NESTING TRANSPOSED — the rule puts
  the lbl_pop Normal catch outermost with the Exn_handler inside;
  the code builds the Exn_handler OUTERMOST (C.trywith wraps
  everything, to_cmm_expr.ml:563-569; trywith = Ccatch(Exn_handler,
  ...), cmm_helpers.ml:4691-4700), i.e. Exn_handler(lbl_w) >
  Normal(lbl_pop, binds x_res) > Normal(lbl_push) > Cexit Push.
  Semantically equivalent under the Cmm machine (Cexit is
  label-targeted, raise is trap-stack-targeted) but a normative shape
  a transcribing .v would pin — real outputs would fall outside the
  relation (the KF-032(b) miss class). (2) the reraise kind is
  DEBUG-GATED (raise_prim, cmm_helpers.ml:4064-4067): Craise
  Raise_reraise only under !Clflags.debug, else Raise_notrace — the
  rule writes Reraise unconditionally; same gate the .v already
  models at TC.ApplyCont.Raise (debug_flag hook). Verified accurate:
  extras translated at the call site (fold over C.simple :513-531),
  wrapper handler extras-FREE with params [x_exn], push/pop scaffold
  exact, x_res at return machtype, NOTES rationale. Reported to
  Milner (owns the doc fix per main's routing).
  DELTA 3 (2026-07-18): Milner's fix-round message CROSSED my DELTA-2
  corrections — it still describes the shape as "outer Normal pop
  catch binding the result" with the trywith Exn_handler inside,
  i.e. the transposition. Corrections re-flagged in my reply
  (nesting: Exn_handler outermost; reraise kind debug-gated). Also
  corrected the misattributing site note: VERIFIED on disk — the
  tc_apply_return header now says the wrapper is "modeled NOWHERE
  (KF-033: not here, not in ch. 18's doc or ToCmmData.v)" with the
  composed-model consequence named (stuck => CMO_undef, KF-019's
  Cmm-side twin) and the pending 16th-rule landing plan
  (ToCmmControl.v:726-736). The stale control_shape sentence (doc
  :380) also landed per Milner. The .v constructor is still to come.
  DELTA 4 (2026-07-18): BOTH HALVES LANDED AND VERIFIED. Doc: the
  re-applied rule (16-to-cmm-control.md:308-341) now has the
  Exn_handler OUTERMOST and the line-244 gate convention
  `(if !Clflags.debug then Raise_reraise else Raise_notrace)`; NOTES
  cite the TC.ApplyCont.Raise gate — both DELTA-2 corrections
  applied. Encoding: tc_exn_wrapper (ToCmmControl.v:525-555,
  TCWrap_None/TCWrap_Extras) verified against the code line by line:
  nesting Exn(lbl_w) > Normal(lbl_pop, x_res, Cvar x_res) >
  Normal(lbl_push, Cexit(lbl_pop,[call],[Pop lbl_w])) >
  Cexit(lbl_push,[],[Push lbl_w]) matches to_cmm_expr.ml:546-569
  (pop catch built OUTSIDE push, trywith applied last); gate matches
  raise_prim (cmm_helpers.ml:4064-4067 — the code passes
  Raise_reraise unconditionally at :561 and raise_prim itself
  gates); is_cold=false matches trywith/create_ccatch
  (cmm_helpers.ml:4691-4700, to_cmm_expr.ml:549/555), disclosed;
  extras at the call site over `map fst (ec_extra_args ...)` matches
  the fold :515-530 (kind component dropped by the code too);
  machtype drop disclosed. Threading verified: TC_Apply (:762-766)
  gains `tc_exn_wrapper th ap call call'` with Return placement
  consuming call'; the "modeled NOWHERE" paragraph is gone and the
  tc_apply_return header (:809-814) names the resolution. Milner's
  requested eye on the freshness guard set (his item 5): the x_exn
  guard over vs_extra is the RIGHT single local variable hazard —
  x_exn is bound at lbl_w handler entry and the extras evaluate
  under it; x_res has no local hazard (its only occurrence is its
  own handler's body, and call is NOT under x_res's binding — entry
  binding overwrites any persisted call-internal extension); labels
  by phi_label_fresh + pairwise distinctness per file convention;
  x_exn = x_res collision admits no in-term behavior change (bound
  on disjoint paths, outward persistence is ch. 20's global
  discharge). Two spin-offs, NOT blockers for this finding: KF-042
  (the extras ride simples_translate's skip row with no
  arity-fixing pairing — also hits TC_ApplyCont_Raise) and W-27
  (env-derived image mutual binder-disjointness is systemic, not
  ExnWrapper-local). Status: RESOLVED, compile confirmed — Hopper's
  full -k chain green on the KF-044+CF-6 batch builds ToCmmControl.v
  with the constructor in; pending-compile qualifier cleared
  2026-07-18.

### KF-034 — INV_ToCmm_Control's matching Cmm run has an unconstrained existential trace (low-medium)
- Chapter: 16-to-cmm-control.md:370-397 (INV.ToCmm.Control: a SILENT
  control step — the rule's own H ~ M unchanged reading, and the .v's
  L_tau restriction — matched by "a matching Cmm run"; as the doc's
  NOTES say, a specialization of INV.ToCmm.Simulates, whose matching
  preserves traces). Rocq: theories/ToCmmControl.v:849-851 — the
  conclusion existentially quantifies `tr` with no constraint. Owner:
  Milner.
- What differs: the conjecture admits an event-emitting Cmm run as a
  "match" for a silent Flambda step (any run re-establishing the
  relation with M' = M qualifies, e.g. one that runs through an
  event-emitting extcall that restores memory). That is weaker than
  the doc's matching and useless to ch. 20 as a simulation case: trace
  correspondence is the point of Simulates.
- Candidate fix: pin `tr = []` (use `cmem_run P _ [] _` as TC_Let_Subst
  already does). Nothing is lost: in the model, matched control runs
  are Ccatch/Cexit/ite/raise steps plus (bind-point) pure Clets — none
  emit events; any hypothetical event-emitting flush residue would
  already falsify M' = M or the catalog-62 discipline.
- Status: RESOLVED — reported to Milner 2026-07-18; fix verified on
  disk 2026-07-18: the conclusion's cmem_run trace is pinned to []
  (ToCmmControl.v:905), the existential `tr` removed from the binder
  list (:904).

### KF-035 — TC_Let_Static's update list is unconstrained beyond per-store shape; the slack contains behavior-changing "translations", so the disclosed under-determination is OVER-approximation, not narrowing (medium-high)
- Chapter: 18-to-cmm-data.md:169-185 (TC.Let.Static — the NOTES pin the
  correspondence: "Or_variable 'holes' filled from ρ become deferred
  stores"; the update list is exactly the filled holes). Rocq:
  theories/ToCmmData.v:1390-1416 (TC_Let_Static:
  `Forall (static_update th) us` with `us` otherwise free; ENCODING NOTE
  :1398-1411 "correspondence ... not determined"; static_update
  :1246-1250 leaves `sym` and the field index existential). Catalog:
  item 63 sanctions this as "UNDER-DETERMINED by design". Owner: Milner
  (self-flagged; this entry is my disposition of that flag).
- What differs: the relation admits ANY list of hole-shaped stores:
  `us = []` when scg has holes (holes never filled — the Cmm run reads
  the link-time placeholder where the source reads ρ(x)); stores
  targeting OTHER live constants' fields (`sym` unconstrained by `bst`);
  wrong variable/field pairings within the binding's own symbols;
  duplicates. The doc determines the list exactly.
- Why it matters: DIRECTION. The LPE gap (catalog 62) and the inc-2
  exclusions are UNDER-approximations — real outputs fall outside the
  relation, coverage narrows, nothing becomes false. This one widens: a
  translation relation ch. 20 universally quantifies over now contains
  concrete behavior-changing "translations" (unfilled holes; a store of
  an arbitrary in-scope variable into another live constant's field), so
  Simulates posed forall against tc_expr_data is trivially FALSE, not
  conjectured — precisely the failure mode catalog item 64 itself
  invokes to reject unguarded syntactic sinks. Reachable for any program
  whose static block has a Dynamically_computed field. The note's
  neutral "not determined" (and item 63's "by design") undersell this.
- Candidate fix, two acceptable grades: (i) pin the correspondence — a
  structural walk `holes : static_const_group -> list (symbol * Z *
  variable)` over the MODEL's static_const grammar (likely far smaller
  than the compiler's 26 constructors), with `us` its static_update
  image in the fold's order; or (ii) if the walk is deferred: constrain
  each update's symbol to bst's bound symbols (kills cross-constant
  clobbering cheaply), replace the note's wording with the
  falsifiability statement, and post a ch. 20 header obligation that
  Simulates must NOT be posed forall against TC_Let_Static instances
  (a KF-033-style handoff). Only (i) is normative in substance;
  recommended.
- Status: open — reported to Milner; escalated to main (catalog item
  63's sanction is in tension with item 64's own rejected-alternative
  rationale), 2026-07-18. DELTA: main ACCEPTED — item 63 amended to
  UNDER REVIEW, original sanction called "wrong-signed", structural
  walk marked preferred, KF-036 folded into the same entry. Awaiting
  Milner's fix.
  DELTA 2 (2026-07-18): RESOLVED at grade (i), verified on disk and
  against the code. The walk is total and offset-exact: ov_hole /
  ov_holes / simple_holes / sc_holes (ToCmmData.v:1260-1338) yield
  (byte offset, variable) pairs; strides verified against
  Update_kind (to_cmm_shared.ml:382-455): packed arrays at element
  strides int8=1/int16=2/int32=4/float32=4 (UK.naked_int8s etc.;
  static_unboxed_array_updates's index is in element units,
  to_cmm_static.ml:91-104, use sites :235-238/:277), word arrays and
  float blocks at 8, vec arrays at 16/32/64; mixed-block fields
  advance by field_size_in_words (1/2/4/8; block_field_sizes matches
  to_cmm_static.ml:380-401). Both static_update arms now take
  strided_field_address_image (BYTE offset, zero-folded,
  = cmm_helpers.strided_field_address :1580-1583). SOC value slots:
  soc_holes' existential (f, sym) is address-coincident — sym_f +
  8*(off(vs) - off(f)) = base + 8*off(vs) for every f under the
  R.Obj initial-memory obligation, so the over-approximation admits
  only semantically-equal stores (unlike the original slack); the
  honest-middle judgment ACCEPTED. Disclosed residue (SOC emission
  order; add_symbol_init hoist of Cvar-valued updates,
  to_cmm_shared.ml:515-518) verified accurate. Clauses now
  `static_group_holes bst scg hs -> Forall2 (static_update th) hs us`
  (:1586-1591), no free us. CAVEAT: the offset-exactness
  verification EXPOSED a latent COMPILER bug the walk faithfully
  transcribes — boxed custom-block payload updates target word 0
  where the payload is at word 1; spun off as KF-040 (the model is
  code-faithful there, doc/R.Obj disagree with both). KF-035 itself
  closed.

### KF-036 — static_update's store shape misses the pointer path: real pointer-hole updates are caml_initialize extcalls, not Cstore (medium)
- Chapter: 18-to-cmm-data.md:182-183 ("deferred stores (update_opt,
  sequenced before the body)"). Rocq: theories/ToCmmData.v:1246-1250
  (static_update = `Cop (Cstore ch Initialization) [field_address; e]`,
  chunk existential per the ProjectValueSlot precedent). Code ground
  truth: update_opt → C.make_update (to_cmm_shared.ml:457-483): kind
  Pointer sets must_use_setfield ("the GC must see static field
  updates") → Cmm_helpers.setfield _ Pointer Root_initialization →
  assignment_kind = Caml_initialize (cmm_helpers.ml:4128) →
  `Cop (Cextcall "caml_initialize") [field_address; v]` (:4139-4153).
  Only Immediate and Naked_* kinds take the Cstore path
  (to_cmm_shared.ml:484-499, Word_int / chunked). Owner: Milner.
- What differs: pointer-kind holes — the mainline case, any value-kind
  field of a static block — produce an extcall the shape never matches,
  so `Forall (static_update th) us` is unsatisfiable for real update
  lists containing one.
- Why it matters: under-approximation (the safe direction), but of the
  COMMON case, and undisclosed — the note and catalog item 63 present
  the Cstore shape as THE update_opt shape. Real outputs for ordinary
  static blocks with variable fields fall outside the relation, on top
  of KF-035's slack in the opposite direction.
- Candidate fix: disjunction in static_update — the existing Cstore
  shape OR the caml_initialize extcall image on the same
  field_address/value (encoded per the file's extcall-image
  conventions); note discloses which Update_kind takes which arm.
- Status: open — reported to Milner, 2026-07-18. Found in my
  disposition lane; Church's .v-vs-code pass on ToCmmData should
  confirm Update_kind coverage independently.
  DELTA (2026-07-18): RESOLVED, verified on disk and against the
  code. static_update (ToCmmData.v:1401-1415) is now the exact
  disjunction: kind-Pointer arm = return_unit_image around the
  caml_initialize extcall image (matches make_update's
  must_use_setfield -> Cmm_helpers.setfield _ Pointer
  Root_initialization -> assignment_kind Caml_initialize ->
  return_unit-wrapped extcall); every other kind = bare
  `Cop (Cstore ch Initialization)` (make_update's store path,
  to_cmm_shared.ml:484-513, has no wrapper). Arm not keyed per hole
  and chunk existential — both disclosed in the site note
  (ProjectValueSlot precedent), acceptable: R.Obj.* fixes each
  constant's layout, so the slack admits no behavior-changing image
  on in-layout programs. Closed.

### KF-037 — TC_Switch_TestByValue is refutable as stated: the machine's flat environments leak the test's binder extensions into the branches (high)
- Chapter: none directly (kernel statement; catalog-62 family; the
  stated obligation for KF-032(b)). Rocq:
  theories/ToCmmControl.v:252-269 (Theorem TC_Switch_TestByValue,
  Admitted); machine ground truth: Cmm.v:911-914 (CM_Head_Let — the
  venv extension `fupd ce x w` PERSISTS, no scope-restore frame),
  Cmm.v:988-993 (CM_If runs the chosen branch in the config's
  current, i.e. test-extended, venv), Cmm.v:1017-1024 /
  CM_Catch_NonRec (chi extensions persist likewise). Owner: Milner.
- What differs: the theorem quantifies over ALL e_t, e_t', e1, e0
  with no non-interference premise, but the machine's environments
  are flat, so a test's Clet/Ccatch extensions are visible to the
  branch. Concrete counterexample (all hypotheses constructively
  satisfiable): ce x = Some (CV_word 1); e_t = Clet x (Cconst_int 2)
  (Cconst_int 1); e_t' = Cconst_int 1; e1 = Cvar x. Both tests run
  silently to CV_word 1 restoring M/TT/RR (hyps 1-2, with ce2 =
  ce[x:=2], ce2' = ce). The ite over e_t runs to Cval (CV_word 2)
  (hyp 3: CM_If picks e1 in ce[x:=2]); the conclusion demands the ite
  over e_t' also reach CV_word 2, but its branch reads ce x = 1 and
  every run is forced (only deterministic head rules involved) — no
  such run exists. A chi analog exists via Ccatch-in-test +
  Cexit-in-branch. The comment's disclosure ("plausibility rests on
  the freshness of translation-introduced backend variables, which
  the branches never read") names exactly the right condition — but
  as a COMMENT. In Rocq an unstated premise does not weaken an
  Admitted statement; it falsifies it.
- Why it matters: the statement is Admitted, so the global axiom
  context is semantically INCONSISTENT — False is derivable from the
  counterexample plus routine determinism inversions on cm_head/
  cm_step. Worse than an unprovable conjecture: anything ch. 20
  proves USING it is vacuous, and Dijkstra's finale Print Assumptions
  check would list it as a clean-looking assumption. Also violates
  the STATUS discipline (conjectured = believed-true). And it is the
  stated obligation for the KF-032(b) under-approximation, which
  therefore remains undischarged. NOTE the analogy the comment draws
  to TC_Let_Subst FAILS: that kernel pins ce and re-runs the SAME
  expression, so no venv counterexample exists there (but see KF-038
  for its own chi' hole).
- Candidate fix: turn the disclosure into premises, statable with the
  binders already present: for every backend var mentioned free in e1
  or e0, `ce2 x = ce x /\ ce2' x = ce x`; likewise for every static
  label free in e1/e0, `chi2 l = chi l /\ chi2' l = chi l`. Needs a
  free-mention predicate (the cmm_mentions family item 64 already
  introduces for ToCmmSoundness.v — placement/sharing decision with
  main). Ch. 20 discharges the premises by freshness of
  translation-introduced vars/labels, exactly as the comment intends.
- Status: open — reported to Milner (his explicit strength-check
  ask), cc main (severe class), 2026-07-18.
  DELTA (2026-07-18): FIX LANDED AND VERIFIED. cmm_mentions /
  cmm_mentions_label hosted in Cmm.v:306-382 per the placement
  ruling (binders, handler params/labels, Cexit targets, and trap
  actions all count — the safe over-approximating direction, stated
  in the site comment :294-304). TC_Switch_TestByValue
  (ToCmmControl.v:259-283) carries exactly the two proposed
  premises (:271-277); my Clet-persistence counterexample now fails
  the `ce2 x = ce x` premise and the chi analog fails the label
  premise. The comment (:243-258) states the premises are
  load-bearing with the KF-037 cite and the ch. 20
  freshness-discharge plan. KF-032(b)'s stated obligation is now
  sound-shaped. Status: RESOLVED, compile confirmed — Hopper's full
  -k chain green on the KF-044+CF-6 batch (zero warnings) builds
  ToCmmControl.v with this fix in; pending-compile qualifier
  cleared 2026-07-18.

### KF-038 — TC_Let_Subst is refutable via the free chi': a defining expression can read the handler environment through a bare Cexit (high; retroactive on my inc-3c verification)
- Chapter: 18-to-cmm-data.md (TC.Let.Subst). Rocq:
  theories/ToCmmData.v:1131-1141 (Theorem TC_Let_Subst, Admitted; the
  conclusion re-runs e_dfn under ARBITRARY chi', TT', RR'). Machine
  ground truth: Cmm.v:1182-1190 (CM_Exit resolves a Cexit through the
  config's chi and runs the handler closure's captured body/envs).
  Catalog: item 62. Owner: Milner. This is retroactive: I verified
  the inc-3c symmetrization (conclusion pinning M/TT'/RR' — my own
  ask) without re-checking falsifiability of the full statement; the
  hole predates and survives that fix, and my W-26 delta recorded the
  kernel as sound-shaped. My miss.
- What differs: e_dfn = Cexit (Lbl l) [] [] with chi l = CHandler []
  (Cconst_int 5) ce0 chi0 d Normal satisfies the hypothesis (CM_Exit
  keeps M/TT/RR; the handler body runs to Cval (CV_word 5) silently).
  The conclusion quantifies chi' universally: with chi' l bound to a
  7-returning handler the forced run ends at CV_word 7, with chi' l
  = None it is stuck — either way no run to CV_word 5 exists. The
  venv channel is CLOSED here (same ce, same e_dfn — the guard
  TC_Switch_TestByValue lacks), and TT'/RR' appear safe (a raise
  reading the AMBIENT trap stack cannot end in Cval inside e_dfn
  while restoring TT; region reads restore M or don't terminate in
  shape), so chi' is the one open channel.
- Why it matters: same class as KF-037 — Admitted-false, so the axiom
  context is semantically inconsistent, and this kernel is what the
  LPE inline rows' catalog-62 obligation rests on. Intended
  instantiations are unaffected in spirit (delayed defining
  expressions are prim/simple images containing no Cexit at all),
  which is precisely why the fix is cheap.
- Candidate fix: one premise — e_dfn is exit-closed: every
  Cexit (Lbl l) in e_dfn occurs under an enclosing Ccatch of e_dfn
  binding l (a structural predicate; trivially true of every LPE
  defining expression). Alternatively premise chi-agreement on
  e_dfn's free labels, mirroring the KF-037 fix shape, if the
  mention predicate is shared anyway.
- Status: open — reported to Milner, cc main (severe class; catalog
  item 62 needs a re-amendment noting the guard), 2026-07-18.
  DELTA (2026-07-18): chi FIX LANDED AND VERIFIED — TC_Let_Subst
  (ToCmmData.v:1140-1152) has the chi-agreement premise
  `forall l, cmm_mentions_label l e_dfn = true -> chi' l = chi l`
  (:1147-1148), the rule-header parenthetical, and an ENCODING NOTE
  recording the finding, the refutation, the over-approximation
  direction, and the vacuous LPE discharge (:1119-1126). My
  bare-Cexit counterexample is blocked. HOWEVER: this entry's own
  "TT'/RR' appear safe" analysis was WRONG — both channels are
  refutable (Pop is a top-matching READ of TT, trap_apply
  Cmm.v:626-634; CM_Region_End pattern-matches iota into RR,
  CmmMemory.v:215-221), so the kernel remains Admitted-false. My
  erratum, spun off as KF-043; I had explicitly told Milner no
  TT'/RR' premises were needed, and he acted on it. KF-038 proper
  (the chi channel) is RESOLVED — compile confirmed via Hopper's
  full -k chain green on the KF-044+CF-6 batch (ToCmmData.v builds
  with the premise in); the kernel's Admitted-false status
  transferred to KF-043 and is now cleared through the KF-044
  resolution.

### KF-039 — PhysEqual's clauses drop the kind-Value restriction; the catch-all `false` arm gives defined WRONG answers on out-of-kind pairs (low-medium)
- Chapter: 06-primitives-memory.md:1185-1204 (P.Binary.PhysEqual —
  NOTES: "Only for values of kind Value (stated in the mli)"). Rocq:
  theories/PrimMemoryB.v:42-50 (phys_same_word, final arm
  `| _, _ => false`), :800-819 (the four clauses quantify over ALL
  v1 v2 with no kind premise). Owner: Girard.
- What differs: the doc's kind restriction is a contract premise the
  clauses drop, and unlike most dropped-contract cases the encoding
  does not go STUCK outside the contract — the catch-all arm makes
  phys_same_word total, so out-of-kind pairs get a DEFINED answer,
  and a wrong one: Phys_equal Eq [V_naked_imm 5; V_naked_imm 5]
  relates to PR_ok 0 (the machine word 5 equals itself); likewise
  identical V_region or V_rec_info pairs "differ". This contradicts
  the file's own policy note (:510-518: representation mismatch =>
  no clause, stuck) — these inputs ARE related, wrongly. The
  DISCLOSED cross-form pointer case (V_ptr vs V_clos aliasing,
  ENCODING NOTE :35-41, deferred to ch. 17) is separate and stays
  acceptable; the undisclosed out-of-kind arm is the finding.
- Why it matters: over-approximation with concrete wrong instances
  in a denotation ch. 10's CSE/PhysEqual reasoning (finding #16's
  neighborhood) and ch. 20's simulation consume. Reachability is
  gated by WF kinding (well-kinded programs never apply Phys_equal
  off kind Value), hence low-medium rather than medium — but the
  guard belongs in the relation, not in the reader's knowledge of
  the type system.
- Candidate fix: add `value_kind v1 = K_value` and
  `value_kind v2 = K_value` premises to the four clauses (or have
  phys_same_word return `option bool`, None off-kind, and match
  Some in the clauses). Out-of-kind pairs then go stuck, per policy.
- Status: RESOLVED (2026-07-18) — Girard applied the premise form
  (his stated preference over option-bool: keeps phys_same_word
  total/reusable and puts the mli contract on the primitive, where
  the doc states it); verified on disk: `value_kind v1 = K_value` /
  `value_kind v2 = K_value` on all four clauses
  (PrimMemoryB.v:807-831). Hopper compiled the delta GREEN with the
  full downstream chain (Machine through Pilot), no PhysEqual
  inversion breakage. Girard's other three PrimMemoryB items also
  closed this round: N1 ruled option (c) by main
  (accept-as-disclosed, proof-phase debt — catalog item 66; site
  disclosures at GetHeader :578-583 and ReadOffset :897-907), N2
  left as-is per my cosmetic call, N3 citation restored verbatim.

### KF-040 — boxed custom-block payload updates target word 0, where R.Obj puts the ops pointer; the model faithfully transcribes a latent COMPILER bug (medium-high for the mechanization; escalated as a compiler-bug FINDING)
- Chapter: 17-representation.md:354-360 (R.Obj boxed numbers:
  Naked_float32/int32/int64/nativeint are CUSTOM blocks — M[a] = the
  &caml_*_ops pointer, payload at M[a+8]); 18-to-cmm-data.md:177-183
  (TC.Let.Static: holes become deferred stores into the layout
  "exactly as R.Obj.* prescribes"). Rocq: theories/ToCmmData.v:1260-1264
  (`ov_hole ... => [(0, x)]`, comment "Boxed-number payload hole:
  word 0 of the object") feeding sc_holes' eight SC_boxed_* rows
  (:1314-1321). Code ground truth: static_boxed_number passes
  `~index:0` (to_cmm_static.ml:106-123, unchanged since the Flambda 2
  import — git -L history checked to 2eaac088ef) with stride-8 kinds,
  and make_update stores at
  `strided_field_address symbol ~index:0 ~stride` = the bare symbol
  (cmm_helpers.ml:1580-1583); but emit_int32/int64/nativeint/
  float32_constant lay out `ops :: payload` with the symbol at the
  OPS word (cmm_helpers.ml:4227-4256). Owner: Milner (model note) +
  main (compiler-bug escalation + policy).
- What differs: for the four custom-block boxed kinds, the deferred
  update writes the payload value OVER the custom-operations pointer
  and leaves the payload word at its emitted default (0). Boxed_float
  and the three vector kinds are fine (no ops word; payload genuinely
  at word 0; strides 8/16/32/64 verified). The model transcribes the
  buggy offset faithfully, and its comment asserts the wrong layout
  as fact.
- Why it matters: three distinct consequences. (1) COMPILER: a latent
  miscompile — currently DEAD in the production pipeline (grep of
  middle_end/flambda2: reification.ml:47 creates Const-only boxed
  static consts; reaper/rebuild.ml only rewrites existing
  or_variables; closure_conversion creates none with Var), but
  REACHABLE via the fexpr test surface (fexpr_to_flambda.ml:573-581
  parses Var payloads for all four kinds), and armed for any future
  producer — e.g. a lift-inconstant extension. Consequence at
  runtime would be a corrupted ops pointer (breaking polymorphic
  compare/hash/marshal on the value) plus a zero payload. Same
  discovery class as the int->float32 double-rounding and
  recover_comparison findings. (2) MODEL: ch. 20 refutability —
  SC_boxed_int32 (OV_var x) is expressible in the model's grammar
  (the model cannot know the pipeline never emits it), so a ch. 20
  simulation posed over all WF programs meets a program where the
  translated update writes M[a] while Representation.v's R.Obj pins
  the payload at M[a+8]: the representation conclusion is FALSE for
  it. Third member of the Admitted-false family (KF-037/KF-038)
  unless guarded or fixed. (3) DOC: none — the doc chain is correct;
  the divergence is code-vs-doc.
- Candidate fixes (policy decision for main): (a) if the compiler bug
  is confirmed and fixed upstream (payload index 1 for the four
  custom kinds — or the updates forbidden for them), the model
  follows the fix: ov_hole takes the static_const constructor and
  returns [(8, x)] for the custom four, [(0, x)] otherwise, and the
  comment states the custom/non-custom split; (b) if the model stays
  code-faithful meanwhile, the ov_hole comment must state the
  divergence explicitly (KF-019-twin style: "the code writes word 0;
  R.Obj puts these payloads at word 1; simulation for these holes is
  intentionally unprovable — latent code bug, reported") so the
  ch. 20 writer poses the statement to exclude or special-case the
  four SC_boxed_* Var holes. Either way the "word 0 of the object"
  comment sentence is wrong as written and must change.
- Status: open — escalated to main as a FINDING (compiler-bug
  candidate + model-policy decision), model note reported to Milner,
  2026-07-18. Found while verifying the KF-035 fix offsets against
  Update_kind.
  DELTA (2026-07-18): main ruled option (b) — code-faithful, with
  divergence disclosure. VERIFIED LANDED: ToCmmData.v:1270-1283 is
  the disclosure in full (index-0 kept; ops-word clobber named for
  exactly the custom four with the +8 payload cite to R.Obj /
  17-representation.md s4; latent-in-production + fexpr-reachable
  stated; flip-on-upstream-fix described as the strictly local
  constructor-keyed change of my option (a)). The wrong "word 0 of
  the object" sentence is gone. Carve-out mechanism: main sanctioned
  the Parameter route — `static_consts_in : expr -> static_const ->
  Prop` landed at Syntax.v:1162 (Church; premises-only, beside the
  binder quartet; CORRESPONDENCE.md Binders paragraph amended by
  main). MODEL side RESOLVED pending one residual: the ch. 20
  Simulates header must actually consume static_consts_in to exclude
  the four SC_boxed_* Var holes — booked to my ToCmmSoundness.v
  review checklist (already listed under the TC.Let.Static layout
  handoff item). COMPILER-BUG finding remains open upstream
  (unchanged by the model ruling).

### KF-041 — sink_step's guards do not survive the binder-extension leak: licensed definitions with internal binders (or internal Ccatch labels) commute past prefixes that read them, so `sunk` relates behavior-distinct expressions (high)
- Chapter: 18-to-cmm-data.md:126-142 (the validity side conditions
  the closure exists to carry); catalog items 64-65 (which REJECT
  unguarded sinks precisely because "that closure contains
  behavior-changing reorderings"). Rocq:
  theories/ToCmmSoundness.v:294-300 (Sink_intro's four guards),
  :276-288 (sink_license), :312-344 (sunk). Owner: Milner.
- What differs: the guards check the moved binder x against sl
  (sl_binds, sl_mentions) and sl's binders against d (sl_captures),
  but nothing checks d's OWN mentions — in particular its internal
  binders, which cmm_mentions deliberately includes — against sl.
  In the machine's FLAT venv, d's internal Clet extensions persist
  after d's run (CM_Head_Let, Cmm.v:911-914 — KF-037's ground
  truth), so moving d changes which states sl's frames and d itself
  run in. Three unguarded quadrants, one guarded:
  (i) d's internal binder read by sl — UNGUARDED. Counterexample:
  d = Clet y (Cconst_int 2) (Cconst_int 1) is LICENSED (total from
  every state, silent, M/TT/RR pinned, value always 1);
  sl = SL_let z (Cvar y) SL_hole, b = Cvar z, x fresh. All four
  guards pass (sl has no binders in d, x fresh, z <> y). From any
  ce with y ↦ 7: before the move, d runs first and leaks y:=2, so
  z binds 2 and the result is 2; after the move, Cvar y reads 7 and
  the result is 7. Both runs silent and deterministic — sunk
  relates expressions with different final values.
  (ii) sl-frame-INTERNAL binder read by d — UNGUARDED (sl_captures
  checks only SL_let binders, not binders inside frame expressions;
  the license's agreement clause does not constrain states that
  differ on d's mentions).
  (iii) chi twin, both directions — UNGUARDED ENTIRELY: no label
  analog of cmm_mentions exists. A licensed d containing a
  self-contained Ccatch (e.g. Ccatch Normal [(l,[],1,_)] 1 — total,
  silent, always 1) persistently extends chi (CM_Catch_NonRec,
  Cmm.v:1017-1024); a prefix frame with a bare Cexit (Lbl l) reads
  d's handler before the move and is stuck (or hits a different
  ambient handler) after it.
  (iv) SL_let binder read by d — GUARDED (sl_captures), correct.
  The license itself is NOT at fault: its positive totality clause
  survives both W-26(c) recipes as license conditions (a bare-Cexit
  d fails totality — some chi leaves the label unbound; an
  allocating/raising d has no silent M-pinned run). The hole is in
  the MOVE's guards, exactly where item 64 promised guards would be.
- Why it matters: tc_expr_data_sunk (:350-367) is the judgment the
  ch. 20 Simulates will be posed against. With behavior-changing
  moves inside sunk, that Simulates is refutable the moment a
  translated term instantiates shape (i)/(ii)/(iii) — and the tc
  relations choose Cmm binders/labels existentially, so nothing in
  the model forbids the colliding instances that real to_cmm's fresh
  binder/label supplies avoid. This fails item 65's "W-26 discharged
  at the definition site" on check (c) — the closure re-admits, one
  level up, the over-approximation family its own comment cites
  (KF-035, catalog 64's rejected alternative). Root cause is again
  model-machine flatness making freshness load-bearing where real
  Cmm scoping gives it for free (KF-037/KF-038's family; this is the
  first instance in a DEFINITION rather than an Admitted kernel — it
  makes a future theorem false rather than the axiom context
  inconsistent, which is why it is high, not a stop-ship).
- Candidate fix: replace sl_captures with SYMMETRIC mention
  disjointness plus a label twin, both discharged in ch. 20 by
  translation-output freshness:
  `(forall y, cmm_mentions y d = true ->
      sl_mentions sl y = false /\ sl_binds sl y = false)` —
  since cmm_mentions includes binders on both sides, this single
  premise closes (i), (ii), and subsumes (iv); keep the existing
  x guards. For (iii), a `cmm_mentions_label l e` predicate
  (Cexit targets + Ccatch/handler-bound labels, binders included,
  hosted in Cmm.v beside cmm_mentions per the coordinator's
  placement ruling) with the same symmetric disjointness between
  d and sl's frames. Alternatively (weaker but sufficient):
  syntactically forbid Clet/Ccatch/Cexit INSIDE a licensed d — but
  that under-approximates real sunk bindings (translated defining
  expressions do contain lets), so the disjointness form is
  preferred.
- Status: open — reported to Milner (with the counterexample), main
  cc'd (severe: it decides whether ToCmmSoundness.v inc 2's
  centerpiece definition needs a pre-green respin; also item 65's
  "W-26 discharged" needs a catalog correction), 2026-07-18. Found
  running W-26 check (c) against the landed definition, at Milner's
  own invitation.
  DELTA (2026-07-18): Milner's license-verification request (crossed
  in flight with my report) CONCURRED — his two by-construction
  claims are exactly my check (b) verification: totality quantifies
  over ALL chi (empty kenv kills free-exit d) and agreement over
  arbitrary chi/M-differing states conditions only on ce agreement
  over d's mentions (chi/M-dependent d unlicensed; unmentioned-var
  reads impossible since mentions over-approximate reads). The
  license needs NO change. The finding is one level up, in
  Sink_intro's guards, and stands: the counterexample's d is
  licensed. Item 65's catalog correction landed (verified —
  "discharged for parts (1)/(2) only", the quadrant summary, and
  the fix shape). Guard fix itself still to land. Also noted from
  Hopper: ToCmmSoundness.v is currently the board's one RED
  (:332 nested-Forall2 scheme issue, pre-existing, Milner fix in
  flight), so the guard respin can ride that recompile.
  DELTA 2 (2026-07-18): Milner accepted the finding (counterexample
  goes in the site comment as the load-bearing witness) and sent the
  respin DESIGN for pre-landing review (queued behind Hopper's inc-3
  verdict). Design: keep the x guards + sink_license, DROP
  sl_captures, ADD my symmetric mention disjointness
  `(forall y, cmm_mentions y d = true -> sl_mentions sl y = false /\
  sl_binds sl y = false)` plus the label channel via a new
  `sl_mentions_label` Fixpoint built over Cmm.v's cmm_mentions_label:
  `(forall l, cmm_mentions_label l d = true ->
  sl_mentions_label sl l = false)`. MY VERDICT: CONCUR — closes all
  three unguarded quadrants. (i)/(ii): cmm_mentions counts d's
  binders AND reads, so one direction of quantification covers both
  d-binder-read-by-sl (forces sl_mentions false) and
  sl-internal-binder-read-by-d (forces sl_binds false, provided
  sl_binds counts frame-INTERNAL binders — flagged below); (iv)
  subsumed. (iii) both directions from ONE premise because binders
  count as mentions on both sides: d-binds-l has
  cmm_mentions_label l d = true forcing sl silent on l;
  d-exits-to-prefix's-l likewise (Cexit targets counted), and sl's
  Ccatch-bound labels counting in sl_mentions_label makes the
  premise refuse it. Bare-Cexit d already dead by license totality
  (empty-kenv chi). Residual-channel check he requested: venv/chi
  are covered by the two premises; M/TT/RR are pinned by the license
  (my check (b): totality + agreement quantify over arbitrary
  M/TT/RR — same trap_apply/CM_Region_End ground truth as KF-043).
  TWO CONSTRUCTION FLAGS sent for the landing: (1) sl_mentions_label
  must be built per-frame by FOLDING Cmm.v's cmm_mentions_label over
  every cmm_expr the frame contains — not a hand-rolled subset —
  else Push/Pop TRAP ACTIONS inside prefix-frame Cexits escape it
  (cmm_mentions_label counts trap actions; a prefix that pushes l
  while d binds l is a real interaction). (2) Same for
  sl_mentions/sl_binds on frame-internal expressions: quadrant (ii)
  closes only if frame-internal binders land in sl_binds (or
  equivalently sl_mentions built from cmm_mentions, binders
  included). Verification against the landed text when the respin
  rides the :332 recompile. W-26 still closes only then.
- Status: RESOLVED (2026-07-18) — landed and compiled GREEN
  (Hopper, first try). Verified on disk: Sink_intro
  (ToCmmSoundness.v:334-342) has the x guards, my symmetric variable
  premise, the label premise, and sink_license; sl_captures deleted
  (subsumed). BOTH construction flags satisfied by construction:
  sl_mentions (:266-271) and sl_mentions_label (:276-281) are
  literal per-frame folds of Cmm.v's cmm_mentions/cmm_mentions_label
  over SL_seq/SL_let frame expressions (trap actions and
  frame-internal binders inherited). Witness transcription in the
  site comment (:314-332) is FAITHFUL to my original (d = Clet y 2 1
  licensed; sl = SL_let z (Cvar y); before/after divergence via
  CM.Head.Let persistence; dies at the variable premise since
  cmm_mentions counts y as d's binder while sl mentions it).
  Residual-channel sweep at verification: none found — license
  totality/agreement pin M/TT/RR over arbitrary states, d-vs-b
  relative order is unchanged by the move, x is guarded against sl
  both ways, and every venv/chi cross-read instantiation contradicts
  one of the four premises. W-26 CLOSED.

### KF-042 — the skip row reaches the raise-operand sites: TC_ApplyCont_Raise and TCWrap_Extras translate their extras via simples_translate, where nothing re-fixes the arity (medium)
- Chapter: 16-to-cmm-control.md (TC.ApplyCont.Raise,
  TC.Apply.ExnWrapper — both write the extras as the full translated
  list). Rocq: theories/ToCmmControl.v:741-755 (TC_ApplyCont_Raise,
  `simples_translate th ss_extra vs_extra`), :530-555 (TCWrap_Extras,
  same premise); the hook closes to tc_simples
  (ToCmmData.v:1167-1176) whose TCS_skip row drops ANY position
  unconstrained (the elided-analysis-input encoding). Code ground
  truth: translate_raise translates EVERY extra (C.simple_list,
  to_cmm_expr.ml:598-600); the wrapper fold likewise
  (to_cmm_expr.ml:515-530). remove_skipped_args applies to CALL
  arguments against callee params only. Owners: Milner (both sites);
  catalog item 63 wording implicated.
- What differs: the skip row's sanctioned defense — catalog item 63:
  "arity re-fixed by ch. 16's parameter pairing" — holds at the call
  sites but NOT here: vs_extra feeds the Craise operand tail
  directly, with no parameter pairing. So the relation admits
  translations that silently DROP raise extras. The consuming
  handler (TC_LetCont_Exn's image) binds x_exn plus ALL extras, and
  CM_Raise requires operand/param count agreement (Cmm.v:1214, the
  KF-033 analysis), so a dropped-extra instance is STUCK at a
  defined program point — behavior-changing slack of the KF-035
  class, no real output has it, ch. 20's Simulates refutable on it.
- Why it matters: medium, not high — it is definitional slack (a
  future-theorem problem), the intended instances are unaffected,
  and the fix is one premise. But it is also a GREEN-REVIEW ERRATUM
  of mine: TC_ApplyCont_Raise had this shape when I greened
  ToCmmControl.v; I accepted the skip row on the catalog's
  call-site rationale without checking it transfers to the raise
  site. It does not.
- Candidate fix: add `length vs_extra = length ss_extra` at both
  sites (TCWrap_Extras already pins ss_extra to the source list).
  TCS_skip only ever SHORTENS, so length equality forces the
  skip-free pointwise reading — no new hook, no tc_simples change,
  and the call sites keep their sanctioned slack. Catalog item 63's
  defense sentence should gain "at the ch. 16 CALL sites; the raise
  sites pin length" when the fix lands.
- Status: RESOLVED — compile confirmed via Hopper's full -k chain
  green on the KF-044+CF-6 batch (ToCmmControl.v builds with both
  premises in; qualifier cleared 2026-07-18). Verified on disk:
  `length vs_extra = length ss_extra`
  at TC_ApplyCont_Raise (ToCmmControl.v:758) and TCWrap_Extras
  (:538), both with correct site rationale (skips only shorten, so
  length equality forces the pointwise reading); tc_simples'
  ENCODING NOTE (ToCmmData.v:1179-1187) gained the
  call-sites-vs-raise-sites clause naming this finding. Originally:
  open — reported to Milner, 2026-07-18. Found during the KF-033
  constructor verification (Milner's item 4 judgment call is what
  surfaced it).

### KF-043 — TC_Let_Subst is still refutable through TT'/RR': Pop trap actions and Cendregion are conditional READS of the control stacks, and hypothesis-satisfying expressions can touch context frames (high; my erratum — I certified these channels safe)
- Chapter: 18-to-cmm-data.md (TC.Let.Subst — "whatever the control
  state there"). Rocq: theories/ToCmmData.v:1140-1152 (the
  conclusion re-runs e_dfn under arbitrary TT', RR'). Machine ground
  truth: Cmm.v:624-634 (trap_apply — `Pop lbl REQUIRES
  TT = lbl :: TT0`, i.e. a top-matching read), CmmMemory.v:215-221
  (CM_Region_End pattern-matches `RR = RR1 ++ iota :: RR0`),
  CmmMemory.v:164-171 (CM_Region_Begin's freshness is against the
  CURRENT RR and live tags, so a just-popped handle may be
  re-picked). Owner: Milner. MY ERRATUM: KF-038's analysis declared
  "TT'/RR' appear safe" and Milner recorded "no premises added for
  those channels" on my word. Both halves of that check were wrong;
  the chi fix is necessary but not sufficient.
- What differs, TT channel: e_dfn = Ccatch Normal
  [SHandler l2 [] (Cconst_int 5) false]
  (Cexit (Lbl l2) [] [Pop l; Push l]). Under TT = l :: TT0 the
  hypothesis holds: trap_apply pops l (top matches), re-pushes l,
  TT restored; the handler body gives Cval 5 silently with M/RR
  untouched. The chi-agreement premise is satisfiable (l and l2 are
  mentioned; choose chi' = chi). Under TT' = [] (or any TT' whose
  top is not l), trap_apply = None, CM_Exit inapplicable, the
  forced run is stuck — no run to Cval 5 exists. Conclusion false.
- What differs, RR channel: e_dfn = Csequence
  (Cop Cendregion [Cconst_natint (region_word iota)])
  (Csequence (Cop Cbeginregion []) (Cconst_int 5)), with M carrying
  no iota-or-above-tagged blocks (mem_reclaim is then the
  identity). Under RR = [iota] the hypothesis holds: End pops to
  [], M restored; Begin may nondeterministically re-pick iota (both
  freshness premises hold after the pop), restoring RR = [iota];
  silent, w = 5. This e_dfn mentions NO labels, so the chi premise
  is vacuous — and note the label-free alternative fix for KF-038
  would have caught the TT counterexample (trap actions are label
  mentions) but NOT this one. Under RR' = [] the Cendregion pattern
  is unsatisfiable — stuck, no run. Conclusion false.
- Why it matters: same class as KF-037/KF-038 — an Admitted-false
  kernel makes the axiom context semantically inconsistent (fourth
  member of the flat-environment/control-state family:
  KF-037, KF-038, KF-041, KF-043). The intended LPE instantiations
  are again unaffected (delayed defining expressions carry no
  Cexit, hence no trap actions, and no region ops — region prims
  are effectful, never delayed), which again makes the fix cheap.
- Candidate fix: a syntactic occurrence guard in the KF-038 premise
  style, vacuously discharged by every LPE expression: no Pop trap
  action and no Cendregion occurrence anywhere in e_dfn (one
  Fixpoint over the expression, host it in Cmm.v beside the mention
  predicates; naming with main — e.g. cmm_pops_or_ends_region).
  Push-only trap actions and internally-caught raises stay allowed
  (a raise pops only the raiser's own pushed frame, CM.Catch.Exn,
  and the hypothesis' TT-restoration already forces balance), and
  own-region Begin/End pairs are excluded only vacuously. NOT
  sufficient alternatives: label-freeness (misses the RR channel,
  above); pinning TT' = TT and RR' = RR (destroys the kernel's
  purpose — the LPE splice re-runs at genuinely different control
  states).
- Status: open — reported to Milner with both counterexamples, main
  cc'd (severe class: Admitted-false; also my public erratum on the
  KF-038-round advice), 2026-07-18. Found re-checking the landed
  KF-038 fix against the full statement rather than the fixed
  channel only — the lesson KF-038 itself taught.
  DELTA (2026-07-18): fix LANDED (in Hopper's intake) and verified
  for the FILED channels: cmm_touches_stacks (Cmm.v:400-429) flags
  Pop trap actions (Cexit row, existsb trap_action_pops) and
  Cendregion (Cop row), walks handler bodies, and is consumed as
  TC_Let_Subst's first premise (ToCmmData.v:1161); both witnesses
  are transcribed FAITHFULLY in the kernel ENCODING NOTE
  (:1130-1136), including the label-free-RR observation. Both
  original counterexamples are blocked. HOWEVER the rule remains
  refutable through channels my own fix proposal wrongly cleared —
  the "internally-caught raises stay allowed" justification above is
  WRONG as stated (my SECOND erratum on this rule): nothing checks
  that a raise is internally caught, and calls tunnel stack reads
  past any syntactic scan. Spun off as KF-044; the Admitted-false
  status transfers there. This entry's residue is closed by
  KF-044's fix.
  DELTA 2 (2026-07-18): RESOLVED — KF-044's respin landed and was
  Hopper-greened (see KF-044's resolution); with the transferred
  residue discharged, nothing of this finding remains open. The
  whole KF-037/038/041/043/044 flat-environment / Admitted-false
  family is now resolved on disk with compiles confirmed.

### KF-044 — cmm_touches_stacks is under-inclusive: raises and calls read the ambient stacks without a Pop trap action or Cendregion token, so TC_Let_Subst stays refutable (high; Admitted-false family; MY SECOND ERRATUM on this rule — the raise-exemption justification was mine)
- Chapter: 18-to-cmm-data.md (TC.Let.Subst). Rocq:
  theories/Cmm.v:400-429 (the guard — flags only Pop trap actions
  and Cendregion), theories/ToCmmData.v:1156-1169 (the kernel).
  Machine ground truth: Cmm.v:1358-1372 (CM_Raise — `TT = lbl_h ::
  TT'`: a raise READS the ambient trap-stack top and jumps through
  chi at lbl_h, a label the raising expression does NOT syntactically
  mention, then leaves TT popped); :1218-1245 + :1411-1416
  (CM_Apply/cm_returns — the callee runs on the CALLER's region
  stack and `cc_regions c_f = RR` requires it restored);
  :1263-1275 (CM_Apply_Raise — a raising callee becomes a
  machine-plugged re-raise AT THE CALL SITE, handled by the
  caller's TT). Owner: Milner.
- What differs: three channels pass the landed guard yet read
  TT'/RR'. (a) DIRECT RAISE: e_dfn = Cop (Craise rk) [Cconst_int 0]
  has cmm_touches_stacks = false (Craise is not Cendregion; the pop
  is performed by CM_Raise, not by a Pop trap action) and mentions
  no labels (the target comes from TT, not the syntax), so the chi
  premise is vacuous. Witness: under TT = lbl_h :: TT0 with ambient
  chi(lbl_h) an Exn_handler whose body Cexit (Lbl m) [] [Push lbl_h]
  re-pushes the popped frame and jumps to a chi(m) handler returning
  Cconst_int 5 — silent run to Cval 5 with M/TT/RR restored,
  hypothesis satisfied; under TT' = [] CM_Raise cannot fire and
  nothing else resolves a plugged Craise — no Cval run, conclusion
  FALSE. (b) CALL-TUNNELED RR: e_dfn = a Capply of a P-resident
  function whose body is Csequence (Cop Cendregion [region_word
  iota]) (Csequence (Cop Cbeginregion []) (Cexit Return_lbl
  [Cval 5] [])) — syntactically e_dfn contains NO Cendregion (the
  body lives in cp_funs, invisible to any scan of e_dfn), and
  cm_returns hands the callee the caller's RR and demands it
  restored: under RR = [iota] (M carrying no iota-tagged blocks)
  End pops, Begin may re-pick iota (CM_Region_Begin freshness is
  against the CURRENT stack), cc_regions = [iota] = RR, CM_Apply
  yields Cval 5 silently with M unchanged; under RR' = [] the
  callee's End is stuck — no cm_returns, conclusion FALSE.
  (c) CALL-TUNNELED TT: a callee that raises escapes via cm_escapes
  and CM_Apply_Raise re-plugs Cop (Craise Raise_reraise) [Cval
  v_exn] at the call site — channel (a) again, without any Craise
  token in e_dfn.
- Why it matters: the kernel is still an Admitted-false axiom
  candidate (fifth member of the family after KF-037/038/041/043's
  filed channels). The erratum: my KF-043 fix proposal explicitly
  exempted raises ("internally-caught raises stay allowed — a raise
  pops only the raiser's own pushed frame"), and Milner transcribed
  that justification faithfully into both the Cmm.v and ToCmmData.v
  comments. CM.Catch.Exn installs into chi WITHOUT pushing TT
  (Cmm.v:1183-1186), so "internally caught" requires an internal
  Push the guard never demands; a raise with no preceding internal
  Push pops an AMBIENT frame. The safe true statement is only:
  raises whose matching Push is internal to e_dfn are TT'-neutral.
- Candidate fix (same syntactic spirit, still vacuous for LPE):
  extend cmm_touches_stacks to also flag Craise and Capply operator
  heads (delayed pure bindings contain neither raises nor calls, so
  the intended instantiations still discharge the premise
  vacuously; a matching-push analysis would be needless precision).
  With those two flagged the TT/RR read inventory closes: Push-only
  cannot restore the pinned TT; Cbeginregion-only cannot restore the
  pinned RR; handler-depth checks are length-relative (base
  cancels); allocation and extern effects are excluded by the
  hypothesis' M-pinning and trace-silence. RESIDUAL to confirm at
  the fix (Milner): whether Cextern-class operators can have SILENT
  runs in the machine — if every external emits a cm_event, trace
  silence already excludes them; if some are silent, flag them too.
- Status: open — reported to Milner (urgent: KF-043's delta is in
  Hopper's intake; cheapest to respin the same Fixpoint before the
  stamp), main cc'd (severe: Admitted-false + my erratum #2 on this
  rule), 2026-07-18. Found verifying the landed KF-043 guard
  against the machine's full TT/RR read inventory instead of only
  the filed witnesses.
- RESOLVED (2026-07-18, fix verified on disk, Hopper-greened):
  guard now flags Cendregion / Craise _ / Capply _ _ _ at the Cop
  head (Cmm.v:426-432); all three witness channels hit a flagged
  head ((a) Craise; (b)/(c) Capply). Both erratum comments struck
  and rewritten — Cmm.v:388-405 states the three channels correctly
  (CM.Catch.Exn installs into chi WITHOUT pushing TT, so no raise is
  ever internally caught; Capply tunnels via cm_returns +
  CM.Apply.Raise), and the surviving "remain allowed" claims are the
  true ones (Push cannot restore a pinned TT; Cbeginregion alone
  cannot restore a pinned RR). ToCmmData.v ENCODING NOTE
  (:1168-1189) carries the KF-043 + KF-044 witness summaries and the
  rule-header paraphrase now reads "no trap-stack Pop, raise, call,
  or region end". RESIDUAL ANSWERED AND VERIFIED: Cextcall cannot
  run silently — CM_Extcall (Cmm.v:1298-1304) emits [CME_extern ...]
  unconditionally (raising externals unmodeled), so the kernel's
  empty-trace hypothesis excludes extern steps; exemption on record
  in both comments. TC_Let_Subst's Admitted-false status CLEARED:
  with the three heads flagged, the concrete-rule TT/RR read
  inventory closes (see KF-048 for the one exemption that needed a
  better argument than the writer's sweep gave — CM_Alloc_Local DOES
  read RR; it is excluded by M-pinning + allocation monotonicity,
  not by "allocs touch only M").

### KF-045 — TC.Prim.ArrayAccess's normative image claims the primitive's mutability is propagated into the Cmm load hint; the code drops it (array_load emits Mutable unconditionally) — DOC finding, escalated (low-medium)
- Chapter: 18-to-cmm-data.md:415 (`Array_load (ak, index i tagged):
  ... Cop(Cload{chunk(ak); mut}, ...)`, STATUS normative) and the
  :422 NOTES sentence about CSE eligibility. Code ground truth:
  to_cmm_primitive.ml#array_load (:323-355) takes NO mutability
  parameter — value/immediate loads go to int_array_ref /
  addr_array_ref (the always-Mutable mk_load_mut family) and the
  Naked_floats row passes a LITERAL Mutable (:341); the function's
  own CR comment (:325) records the pending refactor "in the same
  way as [block_load]" (block_load DOES thread mutability — the
  doc's TC.Prim.BlockLoad `mutability` at :281 is fine). Rocq:
  ToCmmData.v's TC_Prim_ArrayAccess_Load emits Cload with
  `cmm_mut mu` — a FAITHFUL transcription of the divergent doc
  (found by Church's emission pass, his CF-2; doc half verified by
  me per the three-way protocol).
- What differs: the code's emitted load hint is Mutable regardless
  of the primitive's immutability; the doc's normative rule (and
  hence the .v) claims `mut`.
- Why it matters: soundness-neutral in the conservative direction
  (an over-Mutable hint only suppresses backend CSE/reordering; the
  flambda-level CSE eligibility of immutable array loads — 13 §4.4
  — is decided before to_cmm and unaffected), but the rule is
  normative about emitted shape and misdescribes emission; ch. 20's
  Simulates would relate the model's Immutable-hinted load to a run
  the code never produces (benign today since the hint does not
  change CM.Load semantics, but wrong-shaped for term-level
  comparison).
- Candidate fix: doc rule line `mut` -> `Mutable`, plus a NOTES
  sentence: array_load does not propagate the primitive's
  immutability into the load hint (CR in code cites the pending
  block_load-style refactor); flip back if the refactor lands. The
  .v then follows the corrected doc (emit Mutable, drop the mu
  dependence in that row).
  RIDER (CF-11 follow-on, Church via my lane, 2026-07-18): when
  drafting the fix, add a parallel NOTES word to the .Vector rule
  too — its primitive form names mu above the line but the image
  never consumes it (array_load_vector takes no mutability;
  unaligned_load_* is unconditionally Mutable). Harmless as stated
  (the .Vector rule quotes the helper and never claims mu reaches
  the load — which is why CF-11 was writer-side, already fixed and
  greened at ToCmmData.v:957), but one sentence ("mu does not reach
  the load; the helpers are unconditionally Mutable") keeps the
  scalar and vector rules telling the same story.
- Status: open — escalated to main (doc finding, my lane per
  protocol; Church's CF-2 carried the code half, verified), Milner
  and Church informed, 2026-07-18. Church independently spot-checked
  both cites and concurs; Milner holds the ArrayAccess_Load row
  until main's chapter fix lands (a .v edit BEFORE the chapter edit
  would be out of order — Dijkstra's W-30 watches for that ordering
  violation). CF-6's writer fix (constant Assignment on
  block_set_image's immediate branch) rides KF-044's ToCmmData.v
  respin; Dijkstra's W-29 keys that delta. Companion verdict on
  Church's CF-6 recorded in the ledger row (writer-side, not doc:
  the doc's
  flagless "plain Cstore(Word_int)" matches setfield_computed's
  Simple branch exactly; the .v's `cmm_init ia` over-refines — and
  flambda2's block_set routes even STATIC fields through
  setfield_computed (to_cmm_primitive.ml:180-190), so the
  flag-preserving static setfield path (cmm_helpers.ml:4154) is
  dead from flambda2 and the drop is unconditional for immediate
  fields).
- DELTA (2026-07-22): DOC EDIT LANDED AND VERIFIED (main's ruling;
  git diff vs HEAD isolates the edit — the chapter was otherwise
  unmodified in the worktree). Emission line 18-to-cmm-data.md:415
  `mut` -> `Mutable` exactly as proposed. NOTES extension (:422-428)
  carries every element of the candidate fix: hint is Mutable
  regardless of the annotation; array_load takes no mutability
  parameter (annotation dropped at dispatch); backend-level CSE
  opportunity not taken; the CR is cited only for the
  block_load-style refactor REQUEST (Church's wording caution
  honored); flip-back instruction present. RIDER LANDED: .Vector
  NOTES (:456-458) "μ does not reach the load: array_load_vector
  takes no mutability parameter and the unaligned load helpers are
  unconditionally Mutable". Text-only — no RULE/STATUS lines in the
  diff, so the 303/66/84 baseline does not move. Doc half CLOSED.
  Residual: Milner's one-line ToCmmData.v:911 follow-up (cmm_mut mu
  -> CMut_mutable), now UNBLOCKED — W-30's doc-first ordering is
  satisfied. I verify that row when it lands; entry closes then.
- DELTA 2 (2026-07-22): .v FOLLOW-UP LANDED AND VERIFIED — RESOLVED.
  TC_Prim_ArrayAccess_Load (ToCmmData.v:910-918) now emits
  `Cop (Cload ch CMut_mutable false)`; the mu dependence is dropped
  from the image while mu stays in the primitive form (correct — the
  annotation exists, the image ignores it, exactly the corrected
  doc). Header comment keeps RULE TC.Prim.ArrayAccess / STATUS
  normative and the Load line reads Mutable; the site note
  (:903-909) records CF-2/KF-045, the dispatch-drop justification,
  and the flip-back instruction. ToCmmData.vo rebuilt with the edit
  (same-minute mtime) — compiled. W-30 can close (Dijkstra's call).
  Entry CLOSED; the emission-line story is now consistent across
  doc, .v, and catalog entry 61.

## Substitution-audit checklist (capture channels)

Named section per main's directive (2026-07-18): the taxonomy distilled from
the ch. 11 findings, as the standing checklist for auditing ANY future
substitution-based rule (rw_* rules that instantiate stored terms:
inlining, continuation inlining/shortcutting, loopify wrappers, unboxing
wrapper lets, ch. 18 static rebindings).

For each rule that splices a stored term into a use site, check:

1. **Binder-vs-argument capture** (KF-016): every binder the rewrite
   introduces around caller-supplied material (wrapper lets, freshened
   params) must be checked against every caller-side simple that ends up
   nested under it — including names reachable only through non-obvious
   payloads (coercions' rec_info variables, Change_depth). Positional
   over-approximations (all binders vs all simples) are sound and simpler
   than triangular minimal conditions; say which one the premise states.
2. **Sequential vs simultaneous renames** (KF-017): a composition of naive
   renames equals the doc's simultaneous θ only if no earlier rename's
   TARGET equals a later rename's SOURCE. Hunt the degenerate bodies where
   the later source does not occur (never-returning callees, unused
   params): there the usual "target does not occur" freshness premises are
   vacuously satisfied and the aliasing goes live.
3. **Same-source aliasing within one rename table** (KF-023 / W-23 (iii)):
   two renames from DIFFERENT sources to different targets silently assume
   the sources are distinct; if they coincide, the first consumes both.
   Source-distinctness is a well-formedness fact of the STORED item, not of
   the use site — make sure some layer states it.
4. **Fresh-binder-vs-ambient names** (KF-018): when the doc says a
   generated binder is "fresh", enumerate every name whose occurrences end
   up inside that binder's scope — especially jumps to CALLER continuations
   (return, exn, trap-pop targets) — and demand explicit disequalities for
   each; "fresh" premises stated only against the callee body miss these.
5. **Assumed well-formedness of the stored item** (W-23 → code0_wf):
   binder distinctness (ret ≠ exn, NoDup params, region ≠ ghost) and
   closed-body facts (free conts bounded) that the doc grants by
   alpha-fiat must be premised or supplied by a WF layer the headline
   theorem's hypotheses actually carry. Check the delivery path end to end
   (predicate exists → conjoined where the rule is used).
6. **Target-freshness must count binder occurrences**: "rename source ↦
   target is capture-avoiding because target is fresh" needs target to have
   NO occurrence in the term, bound occurrences included (cont_occurs-style
   counting); free-occurrence-only freshness lets the rename capture under
   an existing binder of the target's name.

## Catalog gaps (CORRESPONDENCE.md requests sent to main)

- The project-wide **undef policy** (ch. 05/06 NOTES-stated undef cases →
  no derivation/stuck; explicit `PR_undef` only for crisply-stated
  conditions; `no_ub` excludes both undef and stuck, per OS.Unit.Final's
  literal "stuck = UB") governs every primitive file but appears only in
  PrimMemoryA.v's header comment and the project log — it is not in the
  CORRESPONDENCE.md catalog a domain expert reads first. Requested as a
  catalog entry.

## Watch items (not findings; re-check when the depending file lands)

- W-1: `Beh_return` observes a single root `heap_object` at `sym_mod`
  (Opsem.v:103-104). Root objects contain raw locations, so Soundness.v's
  obs_equiv must not compare them literally across programs — check the
  ch. 13 observation-equivalence encoding when it lands.
  Update (2026-07-18, Plotkin's pre-Soundness.v question): ruled — ch. 13
  and ch. 04 §8.2 are both silent on location identity (the only declared
  non-observables are alpha-renaming and coercions, 13-soundness.md:36-38),
  and literal comparison falsifies INV.Simplify.Preserves for any program
  whose module block captures a pointer, so renaming-up-to is the only
  reading that doesn't trivialize or weaken the theorem. Concurred with
  Plotkin's option 1 (single partial bijection over the whole behavior)
  with two required strengthenings, else it over-weakens: (a) the
  bijection must be heap-consistent at the observation roots — related
  locations must have structurally-related heap objects (coinductively,
  on the reachable fragment), since a bare renaming of a shallow
  `heap_object` never inspects what the fields point at ("same module
  block value" would otherwise equate `[1;2;3]` with `[4;5;6]` behind one
  indirection); this means obs_equiv needs the final heaps (or
  Beh_return must carry the reachable heap fragment); (b) ONE bijection
  scoped over the entire behavior — all Ev_ccall event values, the
  uncaught-exception payload (same issue, one level up), AND the final
  observation — identity on symbols, immediates, and the domain of the
  shared starting heap (both runs start from the same H, and external
  code can retain/compare pointers across events). Needs a catalog entry
  + main's sign-off before Soundness.v; verify the encoding delivers (a)
  and (b) at its review.
  Update 2: main's prior concurred (CM.Unit.Final's "reachable from the
  module symbol" + ch. 19's opaque locations) and directed the payload
  change immediately; Plotkin applied it and I REVIEWED the applied
  change (2026-07-18): `behavior` now carries the final heap
  (Beh_return tr H_fin; Beh_exn tr v_exn H_fin, Opsem.v:122-132) with an
  accurate ENCODING NOTE (:111-121 — candidly defers restriction +
  renaming to obs_equiv, no over-claim), and has_behavior feeds it
  faithfully (halt config's H; v_exn from the boundary jump; sym_mod
  correctly no longer a parameter, :1165-1186). Faithful as an
  intermediate state; the W-1 obligations now all fall on Soundness.v's
  obs_equiv: (a) heap-consistent comparison at roots — sym_mod for
  return, v_exn for exn (the exn case must NOT observe the module block:
  the doc observes only the exception at exn termination); (b) one
  strict partial BIJECTION per behavior pair — bijection, not a
  merge-allowing injection, because P.Binary.PhysEqual
  (06-primitives-memory.md:1183-1201) is deterministic location
  equality, so immutable-sharing is observable in-model (see W-19);
  identity on symbols, immediates, and the shared initial heap's
  domain. Open amendment sent to Plotkin: trace events carry values
  only, but the doc's observation includes "externally-visible
  mutation"/I/O whose content is what the external reads through
  pointer args at call time — without heap-at-event data, obs_equiv
  under-observes (two runs passing related pointers to
  differing-content buffers compare trace-equal). Resolution TBD in the
  joint catalog entry.
  Update 3 (Soundness.v increment 1 review, 2026-07-18): Plotkin chose
  the heap-carrying-events lane — Ev_ccall_return/raise now carry
  H_call (Opsem.v:30-44, emitted at :1033/:1053) and Soundness.v's
  event_sim compares the externally-readable fragment at call time via
  a per-event sub-bijection b_ev ⊆ b seeded with callee/args/pins.
  Reviewed: strengthenings (a) and (b) are DELIVERED (heap_sim +
  root seeding closes over the reachable fragment; ONE b per behavior
  pair with addr_bij — a real bijection — and `pins H0 b` in every
  beh_sim constructor). His three committed default readings all
  CONCUR: trace values through b (literal equality would contradict
  ch. 19 opacity); closures as leaves (Osim_closures — structural
  comparison falsifies INV.Simplify.Preserves on its own validation
  examples; Vsim_clos's literal function-slot premise verified against
  the code: Function_slot.rename in Simplify occurs only at
  simplify_set_of_closures.ml:687 deriving a lifted-closure SYMBOL
  name, decls keys untouched; the one fresh slot,
  simplify_apply_expr.ml:468's partial_app_closure, is a new wrapper
  absorbed by the rule's existential); heap scope (pins + heap_sim
  observing final contents of ALL initial-heap addresses is sound and
  deliberate — the context retains pointers into H0, so those
  mutations are externally visible; over-observation only strengthens
  the conjecture, and no legitimate Simplify rewrite mutates
  pre-existing heap differently). no_ub's claim that Beh_undef covers
  undef-reaching OR stuck verified against Opsem.v:1193-1196.
  Remaining gap: KF-015 (event_sim's b_ev is not seeded with values
  retained from EARLIER events — cross-event readability
  under-observed). W-1 otherwise DISCHARGED into the joint catalog
  entry (Plotkin drafting; co-sign pending).
  Addendum (2026-07-18): doc grounding for the heap-carrying-events
  lane verified — 15-cmm.md:600-601 (CM.Extcall NOTES) states the
  observable event is the full application Cextern(func, v̄, M),
  "the call-time memory M is part of the trace observable named by
  CM.Unit.Final"; Milner's CME_extern (Cmm.v:571-573) carries
  (func, args, mem, rets), same field order as Opsem.v's Ev_ccall_*.
  So H_call is a transcription of the documented Cmm-side
  observation, not only the retention argument. Cited in the joint
  catalog-44 wording.
- W-2: RESOLVED (targeted check at the start of the Opsem.v green
  review, 2026-07-18) — `HE_rec` (Opsem.v:181-185) looks up the CE_rec
  entry plus `group_lookup k g = Some h` and delivers params
  `zs ++ map fst (ch_params h)` (the doc's z̄ ++ x̄ᵢ prefix,
  04-opsem.md:416/425-431), body `ch_handler h`, definition env, and
  the re-tied kenv `bind_rec_group K_def rho_def zs g d` (= K′: the
  fixed point re-established at lookup, entries only record it);
  `OS_LetCont_Rec` (:733-740) builds the knot with ρ and `length T`.
  `OS_ApplyCont` consumes handler_entry uniformly with
  `length xs = length vs` (W-3's left-fold caveat still applies).
- W-3: `env_upd_vars` is a left fold (rightmost duplicate wins) vs. the
  doc's simultaneous `ρ[x̄ ↦ v̄]` — benign iff params are distinct (WF).
  Update (WellFormed.v green review): NOT settled there — WellFormed.v
  has no distinctness premises (its own fupd_list plumbing has the same
  left-fold semantics), and ch. 03 states none. My Syntax.v review
  recorded no WF.Syntax distinctness rule either. Keep open; if no
  chapter states parameter distinctness, this becomes a candidate doc
  note when a proof first needs it (the compiler enforces it via
  Bound_parameters invariants).
- W-4: RESOLVED (Syntax.v review) — Values.v assumed `simple` puts coercions
  on names only; Syntax.v:71-73 encodes exactly that, and ch. 02's
  `pattern_match` prose ("a *variable or symbol* always has an attached
  coercion ... while a *constant* never does") confirms `s @ co` is
  name-only. No action.
- W-5: `P_Variadic_BeginRegion`'s freshness is `~ In iota R` (against the
  live stack), not the doc's bare "ι fresh" (global). Adequate locally, but
  check Opsem.v/ch. 04's region discipline never relies on retired handles
  staying distinct (e.g. dangling `V_region` values after End_region).
- W-6: RESOLVED (disk check at PrimMemoryA.v green, 2026-07-18) — Curry
  adapted to Plotkin's side: PrimMemoryA.v now uses the total function
  `value_kind : value -> kind` throughout (header note at 33-34; premises
  as `value_kind v = k` / `map value_kind grp = ks`, e.g. 617, 676, 837);
  no `value_has_kind` remains (grep clean). Residual sub-check (kind of
  V_ptr/V_clos/V_null all K_value) moves to the Values.v green review.
- W-7: `denot_mem_a`/`denot_region` are typed over the applied `prim`;
  the approved prim_op interface fix will retype them — re-check premise
  fidelity after that churn (esp. that carried payloads like `field` stay
  on the operator).
- W-8: Cmm.v:73-100 hosts TRANSITIONAL local float Parameters (marked as
  such) pending the shared-ops migration to Base.v (catalog 5, third
  frozen-interface amendment). Until deleted, ch. 18/20 commuting
  statements would relate *different* Parameters and be vacuous. Dijkstra
  tracks the triplication per-round; re-verify deletion at Cmm.v green.
  Sharpened by the Base.v green review: (a) Base's shared set is already
  on disk (Base.v §11) but under different names than Cmm.v's locals
  (f64_eqb/f64_ltb/f64_leb/f64_of_Z/f64_to_Z vs f64_eq/f64_lt/f64_le/
  f64_of_int/int_of_f64), so migration is a rename, not a delete; (b)
  worse, Cmm.v re-declares some Parameters with names IDENTICAL to
  Base's (f64_of_f32, f32_of_f64, f64_to_bits, f64_of_bits, f32_to_bits,
  f32_of_bits) — these shadow Base's inside Cmm.v, so even same-named
  ops are currently distinct axioms; a ch. 18/20 statement mentioning
  both sides would compile and be silently unprovable. (Cmm.v's
  duplicated vecN_to_bits/of_bits Definitions are benign — same bodies,
  provably equal.)
- W-9: `CM_Invalid` (Cmm.v:1324-1335) is stated under hypotheses that the
  ch. 19 hooks don't step `Cinvalid`. When CmmMemory.v closes
  head_ext/step_ext, the unconditional statement should be restated (or
  the hypotheses discharged) there; also the body looks provable outright
  (cm_plug inversion), a Pilot.v candidate.
- W-10: `CM_Head_Tuple` does NOT collapse singletons (`Ctuple [e]` →
  `CV_tuple [v]`) while `cm_bundle` (call/extcall results) does
  (`[v]` → `v`). Consistent only because to_cmm never emits singleton
  `Ctuple` nor `Ctuple_field` on 1-ary results (CM.Syntax.Fragment NOTES);
  if ch. 16/18 files ever produce one, the two forms won't confuse — but
  re-check when ToCmmControl.v/ToCmmData.v land.
- W-11: DISCHARGED (CmmMemory.v green review, mandatory check done against
  backend/cmm_helpers.ml). Outcomes: (1) whitelist — caml_modify and
  caml_initialize confirmed as the real alloc=false Addr-consuming
  barriers; the third entry names a non-existent extern → KF-011;
  (2) missed flows — Cadda-through-Clet is correctly rejected (no hole),
  but the one-layer Cadda patterns reject array_indexing's nested-Cadda
  output → KF-012; native Catomic ops also consume Cadda in real output
  but are outside the modeled fragment → W-17.
- W-16: `CM_Addr_NoSurvive`'s Admitted statement (CmmMemory.v:389-398) is
  a proxy — preservation of the syntactic discipline along runs — weaker
  than the doc's conclusion ("phi never invalidates a subsequently used
  value"), which the ENCODING NOTE argues in prose. Transparent at the
  site; recheck the gap if ch. 20 ever needs the semantic conclusion as a
  lemma rather than the discipline invariant.
- W-17: real cmm_helpers atomics (atomic_exchange/arith/cas, when
  Proc.operation_supported) pass `field_address_computed` (an Addr) into
  native `Catomic` ops — a Cadda consumer neither ch. 19's premise nor
  expr_addr_ok covers. Catomic is outside the modeled Cmm fragment (Cmm.v
  has no Catomic; CM.Load even pins is_atomic = false), so no action now —
  but if the model translates the Flambda atomic prims (they ARE in
  Syntax.v's inventory: BP_atomic_load_field, TP_atomic_*_field, QP_*),
  the output must use the extcall forms (caml_atomic_*_field take block +
  index, no Addr) or the fragment + discipline both need an atomic case.
  RE-BOOKED (2026-07-18, ToCmmControl.v green review): ch. 16 routes all
  Lets through the let_binding_ext hook — prim emission is ch. 18's
  TC.Prim.* rows. Check at the ToCmmData.v doc-vs-.v review whether the
  atomic prims get emission rows and which form they emit.
  STAYS MINE (2026-07-18, dedup with Church settled): his emission
  sweep could not cover it — ToCmmData.v's only atomic mention is
  its own W-17 scope note (:684-691), which claims tc_prim states
  NO clause for BP_atomic_load_field / TP_atomic_* / QP_atomic_*
  because the doc gives them no TC lowering (images would need
  Catomic forms outside ch. 15's fragment). The item is therefore
  purely doc-vs-.v: at the ToCmmData.v green review, check that
  claim against ch. 18 (no atomic TC rows) and that the omission is
  a disclosed fragment restriction, not a silent drop.
  CLOSED (2026-07-18, ToCmmData.v green review, my half): the scope
  note (:692-701) is accurate in every clause. (1) Doc side: ch. 18
  has ZERO atomic mentions (grep) and ch. 16 none either; the only
  TC-relevant atomic surface anywhere is ch. 15's is_atomic field
  on Cload, whose machine rule (:313) pins is_atomic = false, and
  the fragment has no Catomic — so "no stated TC lowering anywhere
  in the doc" holds. (2) .v side: Syntax.v carries the primitive
  families (BP_atomic_load_field :481, TP_atomic_* :491-493,
  QP_atomic_* :498-500) and ToCmmData.v's only atomic hits are the
  scope note itself — a DISCLOSED partiality mirroring the doc, not
  a silent drop; atomics-using programs simply have no tc
  derivation (honest domain restriction, surfaced by Simulates'
  premises, never a wrong image). (3) The "likewise" tail is also
  exact: ch. 18's per-primitive inventory is precisely 14 rules
  (TagUntag..BigarrayLength), each with a 1:1 tc_prim clause
  (ToCmmData.v:704-1049), and nothing for scalar arithmetic /
  comparisons / is_int / get_tag. Original code-side observation
  (cmm_helpers atomics passing Addr into Catomic) stays true but
  moot for the model while the fragment excludes Catomic; would
  resurface only with a fragment extension.
- W-12: RESOLVED (WellFormed.v green review) — WF_Switch_Scrutinee's
  first premise is `kinding Gamma (sw_scrutinee sw) K_naked_immediate`
  (WellFormed.v:500); together with Syntax.v's structural half the doc
  conjunct is fully delivered, split exactly as both rules' NOTES say.
- W-13: RESOLVED (RewritesControl.v draft review, 2026-07-18).
  `replace_apply_cont` (Syntax.v:863-896) rewrites exactly the trap-free
  expression-position sites targeting k, scope-aware; RewritesControl.v's
  LetCont.Inline does NOT double-filter — its premises are two counters
  (count_cont_uses = 1 AND count_inlinable_uses = 1, :1118-1126), which
  together pin the single use to exactly the site class the substitution
  rewrites. The use-kind classification matches the doc: switch-arm,
  trap-bearing, and Apply return/exn uses weigh zero in
  count_inlinable_uses (:140-154), mirroring
  simplify_common.ml#apply_cont_use_kind. Shortcut deliberately uses the
  WIDER retarget_apply_cont (site traps survive, arms rewritten), which
  is the doc's own distinction (ShortcutFlat NOTES) — but see KF-024 for
  its Apply-position gap.
- W-18: CM.Apply.Raise's bucket-only re-raise vs extras-expecting caller
  handlers. `CM_Apply_Raise` re-raises `[Cval v_exn]` (Cmm.v:1111), and
  `CM_Raise` requires `length xs_extra = length v_extra` (Cmm.v:1214) —
  so if the caller's innermost Exn_handler has extra params, the re-raise
  is STUCK → `CMO_undef`. In the real backend an exception arriving from
  a callee delivers only the bucket (Proc.loc_exn_bucket); the handler's
  extra-arg registers/mutables are populated by other paths
  (15-cmm.md:510-513 covers only the direct-raise path). Whether the
  stuckness is reachable depends on ch. 16: if the lowering of exn
  continuations with extra_args materializes extras OUTSIDE the raise
  operands (mutable cells / pre-call assignment, as to_cmm does), then
  model Ccatch Exn_handlers reachable from calls always have
  `xs_extra = []` and the model is faithful; if ch. 16 emits
  extras-expecting handlers catching across calls, this is a doc-level
  infidelity (real programs classified UB). Check at ToCmmControl.v
  review (TC.Raise / TC.Apply exn-cont extra args).
  CLOSED (2026-07-18, ToCmmControl.v green review): superseded by
  KF-033. The conditional resolved the bad way — ch. 16's lowering DOES
  emit extras-expecting Ccatch Exn_handlers (TC_LetCont_Exn binds all
  params), and the protecting mechanism (to_cmm's translate_apply
  extras-reraise wrapper, to_cmm_expr.ml:499-570) is owned by no rule
  and modeled nowhere; the stuckness is reachable in the composed
  model. See KF-033 (escalated to main as doc FINDING).
- W-14: RESOLVED — Church added the site note (Syntax.v:631-634) on
  apply_expr's arity fields, pointing to WF_Arity_ApplyFlavours
  (WellFormed.v:56-57), which states the unarizedness constraint
  directly (and is conjoined in expr_wf's apply case), verified at the
  WellFormed.v green review.
- W-20: WellFormed.v's kind/metadata tables are Section Variables
  (result_kind_table, prim_arg_kinds, code_params_arity,
  code_result_arity — WellFormed.v:140-152, header note citing the
  catalog-21 pattern). The ch. 03 rules' content is delivered only
  relative to an instantiation; verify at Machine.v review that the
  wave-4 instantiation supplies tables consistent with the ch. 05/06
  denotation rows and the unit's actual code metadata (or that they
  are deliberately left as Parameters, catalog-noted).
  RESOLVED (2026-07-18, Machine.v green review): the answer is the
  third alternative — Machine.v deliberately instantiates ONLY the
  dynamic side (Opsem.v's denot/denot_r/cext/pure_prim/
  effectful_prim); the WF tables are never instantiated anywhere.
  Instead ch. 13's statements take them as universally quantified
  arguments (Soundness.v:512-517: rkt/cpa/cra explicit foralls
  feeding expr_wf) with a disclosed ENCODING NOTE (:499-511)
  carrying the safety argument I required as L1 (kind pathologies
  surface as stuck runs, which no_ub excludes; specialize if a
  canonical table module lands). prim_arg_kinds is Section-pruned
  (W-21, closed at the Soundness.v green review); the only LIVE
  prim_arg_kinds in the development is RewritesPrim.v:59's separate
  sanctioned Parameter, so there is no two-tables consistency seam.
  Sound and disclosed at every use site. One residue, routed to
  main: CORRESPONDENCE.md:393-395 still says the tables are
  "instantiated later (entry-21 pattern)" — for the WF tables that
  never happens; the reader's guide should say "universally
  quantified at the ch. 13 statements (see Soundness.v's table
  note); only the entry-21 dynamic-side Variables are instantiated
  (Machine.v)".
- W-22: ToCmmControl.v review agenda — Milner's four self-flagged points
  (2026-07-18 message), to check against 16-to-cmm-control.md and
  to_cmm_expr.ml when the file compiles: (1) tc_raise_kind_opt defaults
  an absent Trap_pop raise_kind to Raise_regular (pre-debug-gate) —
  verify translate_raise's actual defaulting; (2) TC_ApplyCont_Raise
  leaves the Pop's exn_handler field unconstrained (doc ellipsis) —
  check whether to_cmm/WF forces it to equal the applied k; (3)
  tc_switch_form's two-arm encoding admits BOTH the eq-test and the
  disc-0 collapse where the code deterministically collapses — sanity-
  check his in-range-agreement soundness claim (out-of-range is
  OS.Switch.Undef) and the tag arithmetic (tc_retag = (sc << 1)+1,
  tc_disc = 2d+1) against C.tag_int/must_tag_discriminant; (4)
  TC.LetCont.Exn translates the handler body under the ORIGINAL Theta
  while TC.LetCont.Jump uses Theta_j for both — confirm the asymmetry
  in let_cont_exn_handler is real, not a doc typo. Alongside the
  standing W-17 (atomics) and W-18 (exn extra-args lowering) checks.
  Increment-2 additions (Milner's 2026-07-18 intake note, with Hopper):
  INV.ToCmm.Control as Theorem+Admitted; check (5) the control
  relation's class granularity (ctrl_kenv: Jump/Normal, Inline/spliced,
  Return/boundary, Exn/Exn_handler chi entry; ctrl_traps as Forall2
  through Phi; R ~ RR as region-handle list equality) against the doc's
  per-class statements; (6) the restriction to silent steps between
  Ctl_expr endpoints — verify the doc's "H ~ M unchanged" clause really
  presupposes the deferral of C-call/Ctl_jump rows to ch. 20, and that
  no ch. 16 rule is thereby left unmirrored; (7) control_shape includes
  E_let_cont_* although the doc parenthetical omits it — Milner
  escalated a doc clarification to main (conclusion cites TC.LetCont.*,
  parenthetical likely an oversight); confirm the doc fix lands or the
  encoding gets a site note before green.
  CLOSED (2026-07-18): the green review ran the full agenda — see the
  ToCmmControl.v ledger row. (1) => KF-031; (2) resolved, no finding;
  (3) => KF-032 (agreement claim sound, coverage claim wrong);
  (4) resolved, asymmetry real and WF-immaterial; (5) verified;
  (6) disclosed and sound, call-step matching residual booked to the
  ToCmmSoundness.v intake; (7) resolved — doc fix landed
  (16-to-cmm-control.md:380). W-18's leg became KF-033.
- W-21 (low): expr_wf does not conjoin WF_Prim_ArgKinds at
  Let-of-prim — WF_Let_Singleton checks only named_kinding (result
  kind). This matches the doc's factoring (§5.2's conclusion is "the
  primitive application is kind-correct", never wired into Γ ⊢ e ok),
  so not a finding — but any downstream statement wanting "expr_wf =
  fully kind-correct term" (e.g. a soundness side condition) must
  conjoin WF_Prim_ArgKinds separately. Re-check if Soundness.v uses
  expr_wf as a hypothesis.
  CLOSED (2026-07-18, Soundness.v green review): expr_wf IS the
  hypothesis of Preserves and Local, WITHOUT the arg-kinds conjunct
  (prim_arg_kinds is Section-pruned; the DRAFT NOTE records the check),
  and no ch. 13 statement needs it: rewrite soundness leans on
  E-consistency, not kind tables, and kind pathologies manifest as
  stuck runs, which no_ub (undef-or-stuck) excludes from the claimed
  envelope. The same argument makes the universally-quantified
  kind/arity tables benign (low note L1 on the ledger row: the
  ENCODING NOTE should say so in one phrase).

- W-19: phys-eq determinism vs immutable-sharing rewrites.
  P.Binary.PhysEqual (06-primitives-memory.md:1183-1201) is DETERMINISTIC
  location equality ("two distinct blocks with equal contents are NOT
  phys-equal"), so any Simplify rewrite that merges structurally-equal
  immutable statics (lifted-constant dedup / sharing) changes observable
  behavior in-model — under ANY sound obs_equiv, since the phys-eq result
  is an immediate that flows into traces/final values. Consequence for
  ch. 09-12 reviews: if a documented S.* rule dedups or shares immutable
  statics, that is a DOC-level conflict with P.Binary.PhysEqual to
  escalate (the real compiler leans on OCaml's (==) underspecification,
  which the formalism has made specified) — NOT a reason to weaken
  obs_equiv's bijection to a merge-allowing map. Check at
  SimplifyStructure.v (statics placement) and RewritesPrim.v reviews.
  Progress (2026-07-18): cleared for ch. 09 (Girard), ch. 10 control side
  (Church — Switch.Merge merges by canonical-alias equality, not
  structural equality), and ch. 12 (this review concurs: unboxing
  dissolves boxes and threads components; no rule dedups or shares
  immutable statics). Only RewritesPrim.v remains.
  CLOSED (2026-07-18): the RewritesPrim.v check is finding #16 — CSE of
  immutable Make_block is precisely the sharing mechanism this watch
  item predicted, and it was handled exactly as prescribed here:
  escalated as a DOC-level conflict (now 13-soundness.md §4 item 8,
  cross-linked to the -Oclassic sibling), obs_equiv's bijection NOT
  weakened. Residual guardrail lives in KF-030's note: any future
  attempt to state a CSE-local obligation must confront item 8 rather
  than quietly weaken the observation.

- W-23: rw_inline leans on code0 well-formedness that no WF layer
  states (found at the Inlining.v draft audit; W-21-shaped). The
  substitution encoding silently assumes: (i)
  c0_return_continuation ≠ c0_exn_continuation (if equal, the exn
  rename in `inlined_body` consumes the return occurrences too and
  the ret rename finds nothing — returns land on the exn target);
  (ii) NoDup (map fst (c0_params)) (positional `combine` + nested
  lets misbind duplicates); (iii) x_myregion ≠ x_myghost inside a
  Local α^c (the first region rename consumes ghost occurrences);
  (iv) the body's free continuations ⊆ {kret, kexn} (a stray free
  cont escapes into caller scope uninterpreted). All four hold for
  real Simplify-produced code0, but WellFormed.v has no code0
  binder-distinctness or closed-body clauses (checked: only
  WF_RecInfo_MyDepth touches code0), so ch. 13's planned WF-premise
  tying C to the unit's SCC_code bindings does not currently deliver
  them. Either rw_inline gains the disequalities as premises
  (Girard) or the WF layer gains a code0_wf that Soundness.v
  conjoins (needs main's sanction). Re-check at Soundness.v
  increment 2b and at the WF premise's landing site.
  Update (2026-07-18): RULED by main — WF-layer route, no rewrite-rule
  premises. Church's `code0_wf` landed (WellFormed.v:597-602) and is
  VERIFIED for conjuncts (i)/(ii)/(iv): kret ≠ kexn, NoDup param
  names, free_conts(body) ⊆ {kret, kexn} — each matching the ruling's
  spec, with rw_inline explicitly NOT premising it (Girard's header
  pointer sentence, per main's instruction) and ch. 13's C-tying WF
  hypothesis as the delivery path (verify the conjunction at the
  Soundness.v increment-2b review). Conjunct (iii)
  (my_region ≠ my_ghost_region) was DROPPED on an unstatability claim
  that is factually wrong → KF-023 (open, Church). W-23 closes when
  KF-023 resolves and the ch. 13 conjunction is verified on disk.
  CLOSED (2026-07-18): both conditions met — KF-023 RESOLVED (fourth
  conjunct verified at WellFormed.v:599-608), and the ch. 13
  conjunction verified at the Soundness.v green review:
  INV_Simplify_Preserves conjoins (forall cid c0, C cid = Some c0 ->
  code0_wf c0) consumed BY NAME (Soundness.v:517), exactly the ruled
  delivery path.

- W-24: Simplify.v composition contracts for the loopify judgments
  (found at the RewritesControl.v draft review; not findings against
  that file — its own comments delegate composition to Simplify.v).
  (a) k-shadowing: `rw_self_tail_call` is parameterized by the ambient
  loopify continuation k and rewrites a single E_apply node; if
  Simplify.v's congruence closure threads that k under an inner
  Let_cont REBINDING the same k inside the wrapped body, the emitted
  `apply_cont k` is captured by the inner binder. Every traversal in
  RewritesControl.v itself is scope-stopping at rebindings of k; the
  closure must stop threading the loopify state the same way (or carry
  a Barendregt-style premise). (b) recompute staging: rw_code_loopify's
  Body constructor computes RecursiveRecompute's r on the WRAP-TIME
  body (`c0_body (wrap_loopified k c)`, :875-876), where my_depth is
  still free for genuinely recursive functions — the doc's recompute
  runs on the final simplified body ("Simplify rebuilds Code c with
  simplified body e′"). Simplify.v's closure must re-run
  S_Rewrite_Code_RecursiveRecompute on the final body rather than
  treating the constructor's r as the emitted flag. Check both at the
  Simplify.v review; flagged to Scott and Church 2026-07-18.
  CLOSED (2026-07-18, verified on disk against Scott's implementation):
  (a) HOLDS — stc_deep (Simplify.v:105-142) is the scope-stopped
  congruence closure: nonrec handler descent unconditional (binder not
  in scope in its own handler), nonrec body descent gated k0 ∉ {k, kret,
  kexn}, rec groups gate BOTH handler and body descent on every group
  name, no code-body frame (per-function loopify state). The kret/kexn
  extension beyond my flagged k is right: the rewrite compares those
  names syntactically (not E-mediated), and the stop is a nominal-
  encoding shadowing guard the globally-fresh pass never triggers — not
  a refutable strengthening. The wrap's own k-binder (the one that must
  NOT stop redirects) is handled by construction: SC_loopified starts
  loopify_simplifies INSIDE the handler body e0 (:350-364), so the wrap
  binder is never traversed. (b) HOLDS — both simplify_code
  constructors discard the wrap-time r0 and derive the emitted flag by
  S_Rewrite_Code_RecursiveRecompute on the FINAL body' (:363, :369),
  making Non_recursive (hence Small_function eligibility) reachable.
  Bonus check: S_code_rebuild's quantified loopify attribute (ENCODING
  NOTE :237-246) is sound-per-instantiation slack (the wrap is
  semantics-preserving under any attribute), consistent with the
  universal-am ruling. This was the W-24 contract check only; the full
  Simplify.v green review (rewrites union, cse_deep threading,
  whole-run relation) completed CLEAN 2026-07-18 — see the ledger row.

- W-25: Cmm-side mirror of KF-019 (suggested by Plotkin, 2026-07-18).
  If Cmm.v's final/uncaught-exception classification constrains the
  nested trap stack TT or a pending trap shape more tightly than
  15-cmm.md's CM.Unit.Final states, the toplevel-raise image after
  to_cmm could be misclassified exactly as the Flambda side was before
  the KF-019 fix — and ch. 20's Beh_exn simulation clause must relate
  that image to the now-correctly-classified Flambda side. Check at
  the next Cmm.v green re-review (alongside the KF-013/014 closure
  checks) or at ToCmmSoundness.v intake, whichever fires first.
  RESOLVED (2026-07-18, ToCmmSoundness.v s1/s2 review) — no finding,
  both halves clean. (a) cm_uncaught_config (Cmm.v:974-979) is
  EXACTLY CM.Unit.Final's "a Craise reaching the base trap frame":
  fully-evaluated Craise redex under an existential context, raise
  kind and extra operands existential, chi/kenv/M/RR unconstrained;
  the only trap-shape demand is cc_traps = [] — the base frame
  itself, not a KF-019-style extra constraint. (Corollary noted, no
  action: a stuck raise under NONEMPTY TT — dangling chi label or
  KF-042-style arity mismatch — classifies CMO_undef via
  CM_Unit_Undef's catch-all, the right bucket.) (b) TBR_uncaught
  (ToCmmSoundness.v:440-446) is the doc bullet verbatim: exception
  VALUES related by rep_val (≈ᵥ), traces by rep_trace, and NO
  final-heap clause — matching both the doc's "(values related by
  ≈ᵥ)" and CMO_uncaught carrying no memory. The trace-in-every-
  outcome sentence (doc :59-62) is delivered: rep_trace/rep_stream
  in all four content constructors, TBR_exhaust alone asserting
  nothing about traces, exactly the doc's "except resource
  exhaustion". Diverge/react split pairs like-with-like; the
  cross-pairings the doc's coarser "both diverge" would admit are
  excluded, but same-trace forces the like pairing anyway —
  refinement, not drift.

- W-26: the validity half of TC.Let.Subst must land in ch. 20's
  closure definition (raised at Milner's inc-3a flag, 2026-07-18).
  ToCmmData.v mechanizes TC.Let.Subst as a KERNEL (Theorem
  TC_Let_Subst :1117-1127): silent, M/TT/RR-restoring defining
  evaluations reproduce at later points with the same ce/M. The
  hoist-direction validity clauses (Can't_move_before_any_branch,
  Control_flow_point barriers — 18-to-cmm-data.md:126-131,
  :135-142) are NOT represented, sanctioned by CORRESPONDENCE
  item 62 because the bind-point-Clet closure (let_prim_ext) never
  hoists. That vacuity argument holds ONLY inside ch. 18. Item 62's
  own UNDER-APPROXIMATION obligation poses ch. 20's simulation
  against "the TC.Let.Subst-closure of the relation" — a relation
  that DOES contain flush-direction reorderings, where the validity
  clauses are load-bearing (the doc's garbage-value/segfault hazard).
  When ToCmmSoundness.v defines that closure, its reordering relation
  must carry the validity side conditions (or disclose their absence
  in the same KF-030 sense). Also note the doc conclusion covers
  drop-of-unused-pure and flush placement, neither delivered by the
  kernel — both live in the same closure obligation. Fires at
  ToCmmSoundness.v intake (mine, per the split with Church),
  alongside W-25. Separately flagged to Milner for the inc-3 green
  review: TC_Let_Subst's hypothesis pins the final TT/RR to the
  initial ones but the conclusion leaves TT3/RR3 existential — a
  consumer splicing the re-evaluation into a context needs TT'/RR'
  restored, so the stated kernel may be too weak for its own ch. 20
  consumers; symmetrize or justify.
  DELTA (2026-07-18, inc 3c landed): both asks VERIFIED on disk —
  TC_Let_Subst's conclusion now pins M/TT'/RR' with only ce3/chi3
  existential (ToCmmData.v:1131-1141), rationale in the kernel note
  (:1113-1118: trap pushes and region opens are silent, so the
  hypothesis' pinning is balance content the kernel must transfer);
  the UNDER-APPROXIMATION note names all three closure-owed behaviors
  citing this watch item (:1066-1080); catalog item 62 AMENDED to
  match. Catalog item 64 (new) answers the design half in advance:
  the ch. 20 closure is SINK-ONLY (inline/drop already in the bare
  LPE rows), straight-line contexts that never cross a branch or
  loop header (the validity clauses made structural), plus a
  kernel-shaped SEMANTIC commuting license standing in for the
  PROVIDED quadruple. The intake check NARROWS to: (a) the .v
  definition matches item 64, and (b) the semantic license is strong
  enough to carry the quadruple's content (coeffect-vs-write,
  effectful-barrier-once) rather than mere value-preservation at the
  sink point.
  DELTA 2 (2026-07-18, KF-037/KF-038): a THIRD intake check added —
  (c) the semantic commuting license, being "kernel-shaped via
  cmem_run" like the two kernels just refuted, must not quantify
  environments/expressions freely across the machine's FLAT venv/
  kenv: check it against the KF-037 counterexample recipe (binder
  extensions leaking into the sunk expression's context) and the
  KF-038 recipe (free chi under bare Cexit) before it is Admitted.
  Any license refutable this way makes the axiom context
  inconsistent, not merely the closure weak.
  DELTA 3 (2026-07-18, inc 2 landed; checks run at Milner's
  invitation): (a) PASSES — the on-disk definition set
  (ToCmmSoundness.v:226-367: sl_ctx, sl_binds/sl_mentions/
  sl_captures, sink_license, sink_step, sunk, tc_expr_data_sunk)
  matches catalog item 64's shape (sink-only; straight-line
  contexts; moves cannot cross branch/switch/handler/catch
  boundaries; congruence closure allows wholly-inside-arm moves);
  cmm_mentions hosted in Cmm.v per the placement ruling.
  (b) PASSES — sink_license (:276-288) is the quadruple's content
  stated POSITIVELY (totality from every state, silent, M/TT/RR
  pinned + value agreement across mention-agreeing states); the
  conditional-form vacuity trap is explicitly avoided and the site
  comment says so; discard-on-early-exit argued soundly.
  (c) FAILS at the MOVE (not the license): the license survives
  both recipes as a license, but Sink_intro's guards leave three
  leak quadrants open — d's internal binders read by sl, sl-frame-
  internal binders read by d, and the whole chi/label channel.
  Finding KF-041 (high), with constructive counterexample. W-26
  closes only when KF-041's guard fix lands; parts (1) and (2) of
  the original booking are delivered as item 65 records (validity
  structural in sl_ctx; drop = LPE_drop, not re-represented; flush
  placement = the closure itself).
  CLOSED (2026-07-18): KF-041's respin landed and compiled green,
  verified (see KF-041's final status — all quadrants closed, both
  construction flags satisfied, no residual channel found).
- W-27: ch. 20's Simulates statement must carry a global freshness/
  disjointness hypothesis strong enough for ENV-DERIVED expression
  images, not just rule-minted atoms (raised 2026-07-18 at the
  KF-033 constructor verification). The file convention (stated at
  tc_exn_wrapper's note and the KF-037 comment) guards rule-minted
  variables/labels locally and defers "global freshness" to ch. 20.
  But tc_simple's variable row is `tc_V th x = Some e` with e an
  arbitrary stored expression (delayed-binding inlining), so ANY
  juxtaposition of two translated simples — every Cop argument
  list, every Craise operand tail, TCWrap_Extras' raise path where
  vs_extra evaluate after call's partial argument evaluations —
  cross-pollutes via the flat venv if one image's INTERNAL binder
  is another's mention. Real to_cmm discharges this with
  create_local's global supply. The discharge point must be the
  Simulates hypotheses (catalog item 68's statement): check at the
  ToCmmSoundness.v full green review that some hypothesis (e.g.
  binder-disjointness / well-formedness of translation outputs)
  covers image-vs-image pollution, or file the KF. Fires with #25's
  green review (mine, with W-25 and the KF-040 layout handoff).
  DESIGN REVIEW (2026-07-18, at Milner's request, proposal with
  main for sanction): `Parameter cmm_unique_binders : cmm_expr ->
  Prop` (Barendregt on the translation output, both namespaces) as
  a Simulates premise on fd_body. MY VERDICT: closes every
  NAME-BASED channel W-27 names — image-internal binder vs sibling
  image's free mention (bound-vs-free overlap), double binding
  (pairwise distinctness), and the label twins — and doubles as
  the global discharge ch. 16's file convention defers for
  x_exn/KF-037-style premises. TWO SPEC CONDITIONS for the landing:
  (1) the comment must pin BOTH Barendregt halves — binders
  pairwise distinct AND no bound atom occurring free elsewhere in
  the term — in BOTH namespaces (backend_var binders = Clet vars +
  handler params; static_label binders = Ccatch handler labels);
  pairwise distinctness alone does NOT close the bound-vs-free
  channel, and as an axiom-free Parameter its meaning lives
  entirely in that comment (a fun _ => True reading must be
  excluded by the spec text). (2) Define "binder occurrence" by the
  same traversal family as Cmm.v's mention predicates (host the
  spec comment beside them) so the two notions cannot drift.
  Channels it does NOT close are the non-name-based ones (TT/RR
  discipline, M/allocation) — those are per-rule matters already
  handled by the KF-043/044 guard and the sink license, correctly
  out of this Parameter's scope.
  CLOSED (2026-07-18): landed and verified. Cmm.v:431-443 hosts the
  Parameter beside the mention family with the spec comment pinning
  BOTH Barendregt halves ("all Clet and handler-parameter variable
  binders and all handler labels pairwise distinct, occurring
  nowhere outside their own scopes") in BOTH namespaces — spec
  condition (1) satisfied; hosting beside cmm_mentions/
  cmm_mentions_label satisfies condition (2)'s drift guard (the
  comment's "nominal-occurrence pattern of Syntax.v's binder set"
  ties occurrence to the same traversal family). Consumed as a
  Simulates premise at ToCmmSoundness.v:575, `cmm_unique_binders
  (fd_body fd)`. Milner's covering note — sink moves preserve the
  binder multiset, so uniqueness of fd_body covers the pre-sink
  e_c0 — CONCUR with the one missing half made explicit: sinking
  only NARROWS the sunk binder's scope (pre-sink Clet over
  sl_plug sl b, post-sink Clet inside sl), occurrence sets are
  unchanged by the rearrangement, and Sink_intro's disjointness
  premises already forbid sl mentioning x; so occurrences inside
  the narrow scope sit inside the wide one and both Barendregt
  halves transfer backward. Fine as-is; worth a one-line comment at
  Sink_intro only if a prover trips on it.

- W-31: gc_reloc constraining vs TC_Let_Subst's allocation exemption
  (raised 2026-07-18 with KF-048). The stack guard leaves Calloc
  unflagged although CM_Alloc_Local reads RR; the exemption holds
  because M-pinning + allocation monotonicity make allocating silent
  runs impossible — which in turn holds because gc_reloc (CmmMemory.v
  Parameter) contributes no provable steps. The oracle's ENCODING
  NOTE says "constrainable later": if any axiom ever cashes condition
  (iv) (unreachable blocks may be dropped) as provable step
  existence, re-audit TC_Let_Subst against KF-048's replica witness
  (drop known-content unreachable block, re-allocate replica at the
  freed address, restoring M around an RR-reading local alloc) before
  accepting the axiom. Fires if anyone proposes gc_reloc axioms or a
  concrete GC instantiation.

### KF-046 — SimplifyStructure.v green review: three disclosure-level drifts (low; one part needs a writer answer)
- Chapter: 09-simplify-structure.md (whole chapter); Rocq:
  theories/SimplifyStructure.v; Owner: Girard
- What differs (three parts, all low, all in one file's comments/
  disclosures — the file's structure is otherwise CLEAN, see the
  ledger row):
  (a) PHANTOM CONSUMERS. The file's decision rule (header :24 and
  catalog item 37) is "a ch. 09 rule gets a real Prop iff another
  chapter leans on it", and the header lists five leaned-on
  artifacts. But S_Struct_JoinParams/join_params (:399-411) and
  S_Struct_Rec_NoFixpoint/rec_invariant_entry (:503-509) have ZERO
  consumers outside the file: the Abbreviation comments claim
  "leaned on by chs. 10/12 and the wave-6 files" (:410) and "for
  the wave-5/6 files" (:509), yet a full-corpus grep hits only
  MeetJoin.v (definition of nway_join) and a prose mention in
  Simplify.v:216. Only the three unit-contract Props are consumed
  (Simplify.v:393-398, SU_intro), as catalog :723-724 itself says.
  QUESTION FOR THE WRITER (and the ch. 10/12 owners): were
  RewritesControl.v/Unboxing.v supposed to lean on entry-type joins
  and silently don't (a real seam), or do those files legitimately
  quantify entry types (then the two comments + the header list +
  catalog item 37's "iff" need rewording to "provided for, currently
  unconsumed")? Extra real Props are harmless; false consumer claims
  are not.
  (b) ClosedResult WIDENING UNDERDISCLOSED. The normative rule (doc
  :61-63) and the code agree: simplify.ml:52-61 fatal-errors on ANY
  free variable except toplevel_my_region — the ghost region
  included. S_Struct_Run_ClosedResult (:158-164) additionally
  permits fu_toplevel_my_ghost_region, and its comment presents this
  as matching OS.Unit.Init "on the nose" without stating that the
  code's own check is strictly tighter. Safe direction (ClosedResult
  sits conclusion-side in SU_intro, so widening only admits more
  model outputs), but the disclosure should say the .v deliberately
  widens the code's check for Init-compatibility — or tighten to
  my_region only if nothing needs the ghost case. (The added
  free-conts conjunct bounding continuations by return/exn is fine:
  an addition beyond the doc, disclosed, Init-matching.)
  (c) EmptyAtEnd COMMENT OVERCLAIM. all_statics_placed says "no
  static-const binding under a lambda" (statics inside continuation
  handlers at any depth PASS — weaker than the real at_unit_toplevel
  discipline, correctly disclosed at the PlaceAtToplevel anchor and
  catalog item 38). But S_Struct_Lift_EmptyAtEnd's comment (:797-799)
  says the output unit "carries every static binding at toplevel
  positions only" — claiming the stronger discipline the predicate
  does not deliver. One-sentence fix to match its own catalog entry.
  Also scope note: S_Struct_Rec_NoFixpoint's comment should say the
  rows are the INVARIANT-param columns only (variant params get
  unknown per the doc; the predicate misapplied to variant columns
  would be wrong — moot today given (a), load-bearing the day a
  consumer appears).
- Why it matters: (a) is the ENCODING-NOTE-claims-more-than-delivered
  class — the audit trail says these artifacts are load-bearing when
  nothing bears on them, and it may mask a real missing seam in
  chs. 10/12; (b)/(c) are disclosure accuracy on NORMATIVE rules.
  No soundness exposure anywhere (all deviations are in the safe
  direction).
- Status: open — sent to Girard (writer) with the part-(a) question;
  catalog item 37's wording routed to main, 2026-07-18.
- RESOLVED (2026-07-18, fixes verified on disk): (a) both
  Abbreviation comments (:419, :522-523) and the header (:14-15) now
  say provided-for/currently-unconsumed, with the header splitting
  the three consumed unit contracts from the two provided Props.
  Girard's ch. 12 answer: no silent seam — Unboxing.v deliberately
  quantifies the entry type T directly (unbox_dec input;
  S_Unbox_Denv_Equation meets against it), the join that produced T
  being upstream of the decision procedure. The ch. 10 half I closed
  by grep: RewritesControl.v's only join-related content is two CODE
  citation lines (join_points.ml#compute_handler_env) — same
  quantify-directly pattern, no machinery expecting join_params. So
  the wave-4 expectation comments were simply never cashed; the
  rewording is the correct resolution. (b) ClosedResult ENCODING
  NOTE added (:155-158): states the code's check is strictly tighter
  (a free ghost region trips the fatal too) and the widening is
  deliberate, conclusion-side. (c) EmptyAtEnd comment now says "no
  static binding under a lambda" with the handler caveat (:813), and
  Rec_NoFixpoint gained the invariant-columns-only scope sentence
  including why variant columns would be wrong.

### KF-047 — ToCmmSoundness.v s3 anchors: two doc sharp edges dropped from anchor comments (low; comment-only)
- Chapter: 20-to-cmm-soundness.md:156-215 (SlotLiveness), :293-335
  (EffectLinear); Rocq: theories/ToCmmSoundness.v:785-822 (SlotLiveness
  anchor), :910-947 (EffectLinear anchor); Owner: Milner
- What differs (both are True-Prop documented anchors, so there is no
  formal content at stake — this is audit-trail completeness, the
  stated purpose of the anchors):
  (a) SlotLiveness leg (a)'s NORMAL-MODE qualifier dropped. The doc's
  NOTES (:205-207) condition totality on the occurrence sets being
  *at_normal_mode*: "(a) also needs to_cmm to reach a slot lookup only
  from normal-mode occurrences (INV.NameMode.Coherent + the Phantom
  skip)". The anchor states leg (a) unconditionally. That qualifier is
  a real premise seam (it is how this rule composes with
  INV.NameMode.Coherent, which IS mechanized in Soundness.v) and the
  final report's composition map should be able to read it off the
  anchor.
  (b) EffectLinear's apply_expr Case-3 edge dropped. The doc's
  conclusion (:322-324) explicitly covers "apply_expr Case 3, where
  the CALL ITSELF enters D as a delayed binding for a single-param
  inlined return continuation, relying on Arbitrary_effects never
  being dropped" — the one place a function CALL becomes a delayed
  binding, and the doc's sharpest surprise in this rule. The anchor
  cites to_cmm_expr.ml#apply_expr in its CODE list but the comment
  never mentions the case.
- Why it matters: the anchors' whole value (per catalog 37's decision
  rule and the file's own :774-783 preamble) is preserving the doc's
  sharp edges for the final report; these two edges are
  composition-relevant (one names the mechanized invariant it leans
  on, the other is the load-bearing surprise of the rule). Two
  sentences fix both.
- Status: RESOLVED — Milner landed both sentences (comment-only round,
  Hopper targeted-compile green, zero warnings); verified on disk
  2026-07-18. (a) at_normal_mode qualifier at ToCmmSoundness.v:803-807
  — leg (a) totality now explicitly "needing also that slot lookups
  are reached only from normal-mode occurrences (the doc NOTES'
  at_normal_mode qualifier: INV.NameMode.Coherent plus the Phantom
  skip), the seam to the one mechanized invariant this rule composes
  with" — names the mechanized invariant, exactly the composition-map
  hook asked for. (b) apply_expr Case-3 edge at :932-935 — "The
  conclusion's load-bearing edge is apply_expr's Case 3: the call
  ITSELF enters the delayed set, sound only because an
  Arbitrary_effects binding is never dropped (zero occurrences still
  emits exactly once at the next flush)." Both match the asks; no
  drift introduced elsewhere in either anchor (surrounding text
  re-read). This closes the last open finding against ToCmmSoundness.v
  — task #25 (ch. 20) review trail fully CLEAN; Milner has marked the
  task completed.

### KF-048 — the stack-guard exemption inventory omits Calloc-local, which READS RR; currently safe only via M-pinning + allocation monotonicity, and gc_reloc's "constrainable later" invitation is a tripwire on exactly that argument (low; comment-level today, future-soundness guard)
- Chapter: 18-to-cmm-data.md (TC.Let.Subst); Rocq:
  theories/Cmm.v:388-405 (guard comment's exemption inventory),
  theories/CmmMemory.v:186-202 (CM_Alloc_Local), :226-250 (the
  gc_reloc oracle ENCODING NOTE), :216-221 (CM_Region_End /
  mem_reclaim); Owner: Milner.
- What differs: the KF-044 fix message's completeness sweep claimed
  the ch. 19 head-rule family "touches only M ... and preserves
  TT/RR by construction". That rationale is WRONG for
  CM_Alloc_Local, which pattern-matches `RR = iota :: RR0` — a
  local allocation is a silent RR-READ, and Cop (Calloc CAM_local
  kind) passes cmm_touches_stacks. The on-disk comments make no
  false claim (they are merely silent about Calloc), but the
  exemption inventory in the guard comment covers Push /
  Cbeginregion / Cextcall and omits the one unflagged operator that
  reads a pinned stack.
- Why TC_Let_Subst is nonetheless SAFE today (verified): the kernel
  hypothesis pins M at both run endpoints. In the concrete rule
  inventory, every allocation strictly extends M (CM_Alloc_Heap /
  CM_Alloc_Local via mem_alloc, fresh addresses) and the ONLY
  M-shrinking operation is mem_reclaim at CM_Region_End — behind
  the flagged Cendregion. CM_Alloc_GC cannot help a hostile
  derivation: its gc_reloc premise is an unconstrained Parameter
  ("permitting nothing by itself"), so no concrete refuting run can
  ever discharge it in Rocq. Hence an M-pinned silent run performs
  no allocation at all, and CM_Alloc_Local's RR-read is unreachable.
  (This is the argument my KF-044 fix note already recorded —
  "allocation and extern effects are excluded by the hypothesis'
  M-pinning and trace-silence" — the sweep should have cited it
  rather than "allocs touch only M".)
- The TRIPWIRE: the gc_reloc ENCODING NOTE advertises the oracle as
  "constrainable later". If anyone later adds a drop axiom cashing
  condition (iv) ("blocks unreachable from the Val roots may be
  dropped") as a provable EXISTENCE of collecting steps, the
  M-pinning argument breaks: the adversary picks M containing a
  known-content unreachable block a, has e_dfn perform the RR-reading
  local alloc, then at a later alloc point drops a (and the local
  block) and re-allocates a replica of a's contents at the freed
  address — an M-restoring silent run whose replay is stuck under
  RR' = []. TC_Let_Subst would flip to Admitted-false with NO change
  to the guard, the kernel, or ch. 18. Any future gc_reloc axiom
  must either exclude re-use of freed addresses within a run,
  exclude dropping region-tagged blocks AND keep allocation
  monotone, or be re-checked against this witness shape.
- Asks (both one-comment fixes): (1) Cmm.v guard comment: add
  Calloc to the exemption inventory with the true argument
  (M-pinning + allocation monotonicity; CM_Alloc_Local does read
  RR). (2) CmmMemory.v gc_reloc ENCODING NOTE: add one sentence
  flagging that TC_Let_Subst's stack-guard exemption for allocation
  depends on gc_reloc contributing no provable steps — re-audit
  against KF-048's replica witness before constraining.
- Status: RESOLVED — both asks landed (comment-only two-file round)
  and Hopper-greened (full -k chain, zero warnings, no census
  change); verified on disk 2026-07-18. (1) Calloc exemption
  paragraph at Cmm.v:404-411, inside the cmm_touches_stacks guard
  comment, with the TRUE argument verbatim in substance:
  CM.Alloc.Local READS RR (pattern-matches RR = iota :: RR0);
  exclusion via M-pinning at both endpoints + every allocation
  strictly extending M + the only M-shrinking operation being
  mem_reclaim behind the flagged Cendregion, CM.Alloc.GC
  contributing no provable steps while gc_reloc is an unconstrained
  Parameter — concluding "M-pinned silent runs perform no
  allocation at all (KF-048)", with a cross-pointer to the tripwire
  note. The writer's wrong "allocs touch only M" sweep claim is
  gone. (2) TRIPWIRE paragraph at CmmMemory.v:249-257, end of the
  gc_reloc ENCODING NOTE, citing KF-048/W-31: the exemption depends
  on gc_reloc contributing NO provable steps; an axiom cashing
  condition (iv) as provable EXISTENCE of a dropping step breaks it
  by the address-recycling replica witness and flips TC_Let_Subst
  Admitted-false with no change to the guard or ch. 18; "constrain
  this oracle only alongside a guard revisit." Both match the asks
  exactly. Watch item W-31 REMAINS OPEN by design — the tripwire
  comment is its on-disk anchor, and it fires if anyone proposes
  gc_reloc axioms or a concrete GC instantiation. Found
  re-verifying Milner's KF-044 completeness sweep against
  CmmMemory.v's rule inventory.

### KF-049 — T.Env.ConstCanonicalPersists / T.Env.AliasesAuthoritative: both Admitted theorems are REFUTABLE under tenv_wf as defined — te_canonical is unconstrained data and the wf predicate omits the two invariants the theorems need (high; Admitted-false family)
- Chapter: 07-types-domain.md:466-527 (both conjectured rules); Rocq:
  theories/TypeGrammar.v:834-841 (T_Env_ConstCanonicalPersists),
  :858-861 (T_Env_AliasesAuthoritative), :812-817 (tenv_wf),
  :779-783 (T_Env_Canonical_Least), :495-499 (canonical);
  CORRESPONDENCE items 13-14; Owner: Scott.
- What differs: the doc rules are (presumably true) properties of the
  code's aliases domain, maintained by its operations. The .v restates
  them as static Theorems over tenv_wf — but tenv_wf (Least +
  NoEqualsOnCanonical + Equation.Closed) does not constrain
  te_canonical enough to make either statement true, so both Admitted
  statements are FALSE, i.e. hostile-derivation-refutable (the
  KF-037/038/041/043/044 class: an inconsistent axiom context for any
  future consumer).
- Witness 1 (refutes AliasesAuthoritative, idempotence): E with empty
  te_types, te_is_bottom = false, and te_canonical the 2-cycle
  {var a |-> var b, var b |-> var a} (definable via variable_eqb).
  tenv_wf holds: Least is satisfied — each name's "class" under
  same-canonical is a singleton with equal default binding times
  (both vars default to bt_imported_variables = 2 via
  binding_time_of_name's None branch), so every instance is 2 <= 2;
  NoEqualsOnCanonical and Closed are vacuous on empty te_types. But
  canonical (canonical a) = a <> b = canonical a.
- Witness 2 (refutes ConstCanonicalPersists): E with empty te_types
  and te_canonical = {c |-> c'} for distinct constants c, c' (say
  Simple_const (Const_naked_immediate 0) and ...1). tenv_wf holds
  (Least: bt(c') = bt_consts = 0 <= everything; rest vacuous).
  Instantiate s := c: simple_is_const c = true, canonical c =
  canonical s trivially, so the conclusion forces canonical c = c —
  but canonical c = c', and distinct constants are provably unequal.
  Nothing in tenv_wf makes a constant its own canonical, nor forbids
  two distinct constants in a class (the doc's "moreover" clause).
- Why it matters: (a) exactly the poison class the project ruled
  must-fix on ToCmmControl/ToCmmData (KF-037 precedent) — the doc
  believes these statements true; falsity is a MODELING gap, not
  doc-rooted, and is undisclosed, so the ch. 20 s5.6 leaf-precedent
  does not apply; (b) both theorems are leaves today (grep: no
  consumer outside TypeGrammar.v), but All.v (#26) is about to import
  everything and the finale's integrity claim covers all Admitted
  statements; (c) the gap is load-bearing beyond the two theorems:
  type_simple_in_term's ENCODING NOTE (:750-758) justifies returning
  alias_type_of at a constant by "a constant is always its class's
  canonical (T.Env.ConstCanonicalPersists)" — under current tenv_wf a
  wf env can canonicalize a constant to a DIFFERENT simple, silently
  breaking that reading too. CORRESPONDENCE item 14 already
  DESCRIBES te_canonical as "the path-compressed canonical map";
  tenv_wf just fails to say so.
- Proposed fix (writer's choice of shape): add two conjuncts to
  tenv_wf, matching the ENCODING NOTE at T.Env.Canonical.Least
  ("representation invariant, not a freestanding theorem"):
  (i) idempotence/path-compression: forall s, canonical E
  (canonical E s) = canonical E s — this IS AliasesAuthoritative
  part (a), which then becomes a Qed consuming the conjunct;
  (ii) constants self-canonical: forall c, simple_is_const c =
  true -> canonical E c = c — ConstCanonicalPersists then follows in
  two rewrites (canonical s = canonical c = c) and its "moreover"
  clause (no two distinct constants per class) is immediate. Both
  theorems can keep their rule ids and likely upgrade Admitted ->
  Qed, strictly improving the census. No downstream file consumes
  tenv_wf, so strengthening it breaks nothing.
- Status: RESOLVED (2026-07-18). Scott fixed exactly as proposed and
  Hopper greened it (full 12-file sweep, zero warnings, first try);
  verified on disk: tenv_wf (TypeGrammar.v:824-831) gains both
  conjuncts in order — idempotence :830, constants-self-canonical
  :831 — with a comment (:817-823) naming them as the representation
  invariants behind AliasesAuthoritative (a) / ConstCanonicalPersists
  and citing KF-049 with both witness shapes. Both theorems now Qed
  with statements untouched (te_is_bottom premise kept as the doc
  rule's shape though now unused): AliasesAuthoritative :882-889 is
  the idempotence conjunct applied; ConstCanonicalPersists :851-863
  is one rewrite plus the constants conjunct, and I re-checked the
  proof term (Hconst c Hc : canonical E c = c closes the rewritten
  goal) and the ENCODING NOTE's claim that the doc's
  no-two-distinct-constants clause falls out by instantiating s at
  the second constant (it does: canonical c' = c and = c' forces
  c = c'). LOW RIDER also landed: NoEqualsOnCanonical's comment
  (:791-795) now discloses only the first sentence is encoded, dual =
  stored-representation half per AliasesAuthoritative (b)/(c).
  TypeGrammar.v Admitted census: 2 -> 0 (grep-confirmed). The
  type_simple_in_term justification noted in "why it matters" (c) is
  now actually licensed by tenv_wf. Dijkstra re-stamp flagged by
  Hopper directly (statement-change = tenv_wf shape + 2x
  Admitted->Qed). KF-050 (companion, Concretization.v) remains OPEN
  — T_Gamma_Kind still Admitted at :668, G_equals premise not yet
  added; the combined-round suggestion became two rounds, fine.

### KF-050 — T.Gamma.Kind: the Admitted theorem is REFUTABLE — G_equals has no kind-agreement constraint, so an alias type concretizes across kinds; and unlike KF-049 this file is NOT a leaf (MeetJoin.v's meet/join/prover soundness rules all quantify over gamma) (high; Admitted-false family)
- Chapter: 07-types-domain.md:660-689 (T.Gamma.Kind, T.Gamma.Alias);
  Rocq: theories/Concretization.v:663-668 (Theorem T_Gamma_Kind,
  Admitted), :303-306 (G_equals), :644-650 (consistent);
  Values.v:357-366 (simple_eval); Owner: Scott.
- What differs: the doc's T.Gamma.Kind says concretization never
  crosses kinds — true in the code because alias_type_of kappa s is
  only ever built at s's own kind. The model's G_equals clause drops
  that: it admits ANY v = simple_eval rho s at ANY kind's Equals
  type, and neither gamma nor [consistent] restores kind agreement.
- Witness (no environment machinery needed): T := FT_naked_float
  (Oub_ok (Equals (Simple_const (Const_tagged_immediate 3)))). For
  any E with te_is_bottom = false and EMPTY te_types/te_canonical
  and empty rho, [consistent E rho H] holds (clause 3 vacuous,
  canonical = id) and G_equals gives gamma E rho H T (V_tagged_imm 3)
  since simple_eval of a const is const_value — but kind_of_value
  (V_tagged_imm 3) = K_value <> K_naked_float = kind_of_ftype T.
  Alternate witnesses: any symbol (simple_eval's V_ptr Addr_sym
  fallback fires even under empty rho), or a variable whose stored
  type is any_value. Every OTHER gamma constructor forces the
  conclusion syntactically — G_equals is the sole kind-crossing
  channel (checked constructor-by-constructor).
- Why it matters: (a) Admitted-false, the KF-037/049 poison class —
  not doc-rooted, not disclosed; (b) NOT a leaf: MeetJoin.v states
  T.Meet.Sound/Bottom/GreatestLowerBound, the join rules, all the
  provers, T.Expand, and T.Reify as Admitted quantifications over
  this gamma (MeetJoin.v:897-1744), so the unconstrained alias
  channel sits inside the premises/conclusions of the ch. 08 rule
  set; after the fix the ch. 08 statements touching aliases deserve
  a one-pass refutability re-sweep (I did not find a concrete
  refutation of a MeetJoin statement via this channel, but I did not
  exhaust it).
- Proposed fix: add a kind-agreement premise to G_equals —
  value_has_kind v (kind_of_ftype T) — mirroring the code's
  by-construction kind homogeneity of aliases (alias_type_of takes
  the kind). The doc's own T.Gamma.Kind licenses exactly this
  premise. T_Gamma_Kind then goes through by one case analysis on
  the derivation (every constructor now forces the kind) — Admitted
  -> Qed, census improves. The pilot lemma
  gamma_singleton_naked_immediate and T_Gamma_Closures_CodeAgeLoose
  (both Qed) are untouched by an added premise on G_equals except
  the pilot's inversion gains one trivially-discharged case.
- RIDER (low, comment-only, same fix round): two stale cross-file
  claims about meet_code_id. Concretization.v:681-683 says the
  CodeAgeLoose NOTES on meet_code_id's Both_inputs abuse "concern
  ch. 08 and are recorded there" — they are NOT: MeetJoin.v has zero
  meet_code_id/Both_inputs/code-age content beyond the
  documented-unmodeled note at :1017-1018 (te_code_age is held
  constant through descent, :1073), and 08-meet-join.md's only
  Both_inputs sentence (:166) is about alias meets, not code ids.
  TypeGrammar.v:501-504 likewise says code_age_newer_eq is used by
  "meet_code_id in MeetJoin.v" — no such definition exists. Fix:
  reword both comments to say the Both_inputs finding lives in the
  ch. 07 doc rule's NOTES and ch. 08's mechanization does not model
  meet_code_id (documented-unmodeled per MeetJoin.v:1017). The
  unrecorded content is itself descriptive (sound-but-imprecise
  nondeterminism), so this is a disclosure gap only.
- Status: RESOLVED (2026-07-18). Hopper's confirming green landed —
  full 12-file sweep, first try, zero warnings, no downstream edits
  (meet_sound_naked_float and the pilot rebuilt untouched; the
  discriminate paths absorbed the premise as analyzed); T_Gamma_Kind
  verified Qed on the compiled source. Dijkstra flagged directly by
  Hopper for the re-stamp. On-disk verification (done pre-green): G_equals carries the premise
  verbatim (`value_has_kind v (kind_of_ftype T)`, Concretization.v
  :310) with the ENCODING NOTE naming the code's alias
  kind-homogeneity and citing KF-050 (:300-306); T_Gamma_Kind is Qed
  (:671-686) by the case-analysis route (reflexivity/assumption on
  the pinned formers, two-level gamma_vnn/gamma_hvnn descent for the
  non-null Value case); statement untouched, with the consistent
  premise disclosed as unconsumed and why it stays (:666-670);
  Admitted census 1 -> 0 (grep: only a comment word remains). BOTH
  rider sites fixed: Concretization.v:699-702 (Both_inputs NOTES
  live in the ch. 07 doc rule; ch. 08 does not model meet_code_id)
  and TypeGrammar.v:501-505 (phantom "meet_code_id in MeetJoin.v"
  cite replaced with the correct statement). Will flip to RESOLVED
  at Hopper's green.
- RE-SWEEP PRE-STAGED (2026-07-18, while idle): the committed
  post-fix MeetJoin.v refutability re-sweep has a fixed target list.
  MeetJoin.v has exactly 12 Admitted statements; NINE quantify over
  gamma and are in scope: T_Meet_Sound (:893), T_Meet_Bottom (:908),
  T_Meet_GreatestLowerBound (:926), T_Join_Sound (:1267),
  T_Prove_Sound (:1524), T_Prove_MeetShortcut (:1543), T_Prove_GetTag
  (:1622), T_Expand_Head (:1660), T_Reify_Sound (:1740). THREE are
  gamma-free and out of scope: T_Meet_Store_CoercionErasure (:955),
  T_Join_ConstAgreement (:1294), T_Reify_LiftLocalGuard (:1764).
  Side observation logged now: T_Join_ConstAgreement carries an
  explicit `canonical E c = c` premise (:1298) — written defensively
  against exactly the gap KF-049 closed; post-KF-049 that premise is
  derivable from tenv_wf, but the statement doesn't take tenv_wf, so
  it needs no change (the explicit premise is the more portable
  shape). Sweep method when the fix lands: for each of the nine, ask
  whether the repaired G_equals still leaves a hostile gamma
  derivation that satisfies the premises while breaking the
  conclusion, focusing on Equals-typed operands in meets/joins and
  prover inputs. SWEEP RUN 2026-07-18 (against the on-disk KF-050
  fix): results in the re-sweep ledger row and KF-051/052/053 below.

### KF-051 — T.Meet.Sound: the Admitted theorem is REFUTABLE — simple_eval's symbol fallback lets gamma admit alias values with no rho binding, while sat_ext demands one for every recorded equation, so any meet that records an equation on an rho-unbound symbol breaks the sat_ext conjunct (high; Admitted-false family)
- Chapter: 08-meet-join.md (T.Meet.Sound, normative; T.Meet.AliasAlias);
  Rocq: theories/MeetJoin.v:893-900 (T_Meet_Sound, Admitted),
  :631-637 (M_alias_alias), :170-177 (alias_alias_res), :100-105
  (record_on), :626-630 (the s1 = s2 disclosure note);
  Concretization.v:623-628 (sat_ext), :649-655 (consistent);
  Values.v:357-366 (simple_eval's symbol fallback). Owner: Scott
  (statement in MeetJoin.v; root definitions in Concretization.v).
- What differs: gamma reads a symbol through simple_eval, which
  falls back to Some (V_ptr (Addr_sym s)) when rho does not bind it
  — so gamma admits values at symbol-alias types with NO rho
  binding. sat_ext (Sat_ext_intro) instead requires a literal
  `rho n = Some v` for every equation in the extension. Nothing in
  `consistent` forces rho to bind a symbol that has no stored type,
  and M_alias_alias records equations (via record_on) on names with
  no boundness guard. The two reads disagree, and T.Meet.Sound's
  sat_ext conjunct is the casualty.
- Witness (minimal, self-alias): E entirely empty (te_is_bottom =
  false), rho and H empty — consistent holds (every clause vacuous
  or trivial). T1 = T2 = alias_type_of K_value (sym A) for any
  symbol A. Meet derivation: M_alias_alias with s1 = s2 = sym A
  (can_alias = self on the empty canonical map; bt A <= bt A; the
  recursive meet of the stored types is any_value vs any_value ->
  M_unknown_l). Result eps = record_on A (Equals A) (record_on A
  any_value fempty), i.e. eps(Name_sym A) = Some (Equals A) — the
  overwrite the :626-630 note describes. Premises: gamma T1 v and
  gamma T2 v hold at v = V_ptr (Addr_sym A) via the fallback (kind
  premise satisfied). Conclusion's sat_ext demands rho (Name_sym A)
  = Some _, but rho is empty — False. Non-degenerate variant
  (s1 <> s2): A in te_defined_symbols, B unbound anywhere, rho =
  {A |-> V_ptr (Addr_sym B)}, T1 = Equals(sym A), T2 = Equals(sym
  B), v = V_ptr (Addr_sym B): both gammas hold (A via rho, B via
  fallback), eps records B |-> Equals A, and rho CANNOT bind B
  (consistent's domain clause: B is not name_bound_in E) — sat_ext
  necessarily False. So the hole is not the s1 = s2 corner: ANY meet
  recording an equation on an E-unbound or rho-unbound symbol
  refutes the statement.
- Why it matters: (a) Admitted-false poison class (KF-037/049/050
  precedent), on the file's FLAGSHIP soundness statement; All.v
  would make False derivable. (b) It falsifies the :626-630
  disclosure's "coarser than the code but in the sound direction" —
  recording a self-alias equation is gamma-SHRINKING for sat_ext,
  not enlarging; this upgrades Plotkin's row-48 low item P2
  (premise-or-note wanted on s1 <> s2) to refutation grade. (c) The
  code never hits this: T.Env.Equation.Closed says equations only
  mention bound names, and OCaml symbols always denote statics —
  the model's meet lacks the boundness guard and its sat_ext lacks
  the symbol denotation. Doc is fine; modeling gap.
- Proposed fix (writer's choice): (i) PREFERRED — make sat_ext read
  names the way gamma does: `exists v, simple_eval rho
  (simple_of_name n) = Some v /\ gamma E rho H T v` (vars unchanged
  — simple_eval has no var fallback; symbols gain their static
  denotation). This is semantically the doc's reading (equations
  constrain the valuation; symbols have fixed denotations) and
  weakens sat_ext only at symbol equations, in gamma's own
  direction. Consider aligning consistent's clause 3 the same way
  in the same edit (today a stored equation on an rho-unbound
  symbol merely voids consistency — benign but the same mismatch).
  (ii) Alternative: boundness premises on the meet constructors +
  rho-totality on defined symbols in consistent — heavier and still
  needs (i)'s reasoning for defined-but-unstored symbols. Either
  way T_Meet_Sound stays Admitted (its truth is the real inductive
  content) but stops being refutable.
- Status: RESOLVED 2026-07-18. Hopper built the fix green (full
  downstream sweep, first try) and Scott confirms the compiled text
  is exactly the on-disk text I swept — no drift between
  verification input and build. Verification record (disk probe,
  crossed Scott's announcement): landed as proposal (i) VERBATIM:
  sat_ext's clause now
  reads `exists v, simple_eval rho (simple_of_name n) = Some v /\
  gamma E rho H T v` (Concretization.v:631-636, the simple_eval
  read at :634), with an ENCODING NOTE :623-628 explaining the
  variable/symbol split and citing KF-051. The optional companion
  edit I flagged was taken too: consistent's clause 3 aligned to
  simple_eval (:667-671), with the comment :656-659 naming it "the
  same reading as sat_ext (KF-051)". The indicted M_alias_alias
  disclosure note was repaired (MeetJoin.v:629-633): "sound
  direction" is now expressly conditioned on sat_ext's simple_eval
  reading, with the old literal-rho unsatisfiability credited to
  KF-051. I re-ran both witnesses against the new clause — both
  DEFUSED: minimal (empty everything, eps(A) = Equals A):
  simple_eval's fallback yields V_ptr (Addr_sym A), G_equals holds
  at it (kind premise fine, V_ptr : K_value); non-degenerate (rho =
  {A |-> ptr B}, eps(B) = Equals A): simple_eval rho (sym B)
  fallback = ptr B and gamma (Equals A) (ptr B) via rho's A
  binding. Also checked the general self-alias case beyond my
  witnesses: if s1 is an rho-unbound VAR the equation would be
  unsatisfiable, but T_Meet_Sound's gamma-premise on the alias type
  already forces the rho binding via G_equals, so no residual hole.
  T_Meet_Sound remains Admitted (:903) as intended — no longer
  refutable. SIDE EFFECT NOTED: consistent also gained a NEW fifth
  clause (:673-674, `rho (Name_sym sym) = Some v -> value_has_kind
  v K_value`), justified in the comment from ch. 07 §4 / OS.Let.
  Static and consumed by KF-052's defaulted-symbol case. Assessed
  FAITHFUL: symbols denote statics of kind Value in the code and
  doc; the clause strengthens a FROZEN-interface definition but
  only shrinks the consistent-rho set on symbol bindings, cannot
  manufacture vacuity (rhos ignoring symbols satisfy it), and is
  disclosed in place.

### KF-052 — T.Expand.Head: the Admitted theorem is REFUTABLE — it states the doc's SET-form concretization equality at a FIXED rho, where the backward inclusion is simply false (expansion discards the alias's singleton constraint) (high; Admitted-false family; MY ERRATUM #4)
- Chapter: 08-meet-join.md (T.Expand.Head, normative); Rocq:
  theories/MeetJoin.v:1641-1649 (expand_head), :1660-1665
  (T_Expand_Head, Admitted); Concretization.v gamma_set :660-661
  (the doc's set form); TypeGrammar.v tenv_find :739-745 (missing
  var -> unknown_of_kind). Owner: Scott.
- What differs: the doc's conclusion gamma_E(T) = gamma_E(H) is
  about the SET form (values under SOME consistent rho, ch. 07
  section 4 preamble; gamma_set in Concretization.v). The .v states
  an iff at a FIXED rho. Forward (gamma T v -> gamma U v) is true
  and provable. Backward is false: at fixed rho, gamma of an alias
  type is (at most) the singleton {rho-eval of s}, while gamma of
  the canonical's stored type is everything that type admits.
- Witness: E, rho, H all empty (consistent trivially). T =
  alias_type_of K_value (var x). EH_alias applies: canonical = var
  x (empty map), not a const; U = stored_type E K_value (var x) =
  tenv_find default = unknown_of_kind K_value. Take v =
  V_tagged_imm 0: gamma U v by G_unknown; gamma T v requires
  simple_eval rho (var x) = Some v = None — underivable. The iff's
  backward direction fails. (The failure is generic, not a
  degenerate-env corner: with x bound, stored T_x, and rho x = v0,
  backward fails at every v in gamma(T_x) other than v0.)
- Why it matters: Admitted-false poison class; and unlike KF-051
  the falsity is quantifier-placement drift — a set-form doc
  equality transcribed as a per-valuation equivalence. Note the
  forward direction actively CONSUMES the KF-050 kind premise (the
  unstored-canonical case lands in unknown via G_unknown, needing
  value_has_kind from G_equals) — evidence the repaired channel is
  what makes the true half provable.
- Proposed fix: split the statement. (i) Keep the per-rho FORWARD
  implication under the rule id (gamma T v -> gamma U v, with
  consistent) — likely Qed today: alias case bridges via
  consistent's canonical-invariance clause + clause 3 when the
  canonical is stored, G_unknown + the KF-050 premise when not.
  (ii) The equality is the set-form claim: either state gamma_set E
  T v <-> gamma_set E U v as the Admitted conjecture (matching the
  doc's quantification; the backward realizability argument is
  genuine proof content), or document the backward half as
  deferred. An ENCODING NOTE should say the fixed-rho iff was the
  drift.
- MY ERRATUM #4: row 48's green review put T.Expand.Head in my
  slice and called it "faithful (constant-canonical exclusion
  disclosed; one-step/path-compression NOTES transcribed)" — I
  verified the constructor against the doc's operational content
  but never asked at which quantifier the conclusion's equality
  lives. Same failure shape as erratum #3 (a field outside the
  frame I brought to the rule); logged for the retrospective.
- Status: RESOLVED 2026-07-18. Hopper built the fix green (full
  downstream sweep, first try; per Scott the compiled text is
  exactly the text I swept). Statement blessed for the polish-wave
  Qed attempt (see erratum #5 sub-entry for the twice-repaired
  history and corner verification). Clause 5's downstream cost is
  currently ZERO: Plotkin verified no hand-built consistent witness
  exists anywhere (Pilot.v has no occurrence of consistent;
  Soundness.v consumes it hypothesis-side only, in its two Admitted
  conjectures) — so the FROZEN-interface strengthening bit nobody.
  CROSS-REF (Plotkin's suggestion, taken): consistent clause 5 is
  the MODEL-side answer to the [[KF-054]] seam — if the humans take
  the ch. 07 section 4 preamble sentence KF-054 requests, clause 5
  becomes its direct transcription; the doc fix and the model
  clause should land as a matched pair, and whoever audits the doc
  edit (Dijkstra, doc-first) should check the sentence against
  clause 5's exact shape. Verification record (disk probe, crossed
  Scott's announcement): landed as proposal (i) with the "backward
  documented as deferred" branch of (ii): T_Expand_Head restated as
  the per-rho
  FORWARD implication only (MeetJoin.v:1673-1679 — `gamma E rho H T
  v -> gamma E rho H U v` under consistent; the false backward
  direction is GONE from the statement, so my V_tagged_imm 0
  witness no longer applies). ENCODING NOTE :1663-1672 discloses
  everything I asked for: the doc's equality is between SET forms,
  at fixed rho only forward holds, expansion discards the alias's
  singleton constraint, backward false pointwise, cites KF-052; the
  set-form equality is stated as "deferred with the proofs" (no
  separate gamma_set conjecture statement — acceptable: the rule id
  now anchors only a true claim, and the deferral is disclosed in
  place rather than axiomatized). The note also names the proof
  route I predicted: stored-canonical case via consistent's
  stored-equation + canonical-agreement clauses, defaulted-symbol
  case via the NEW consistent symbol-kind clause (see KF-051 entry)
  together with G_equals's KF-050 kind premise. Statement remains
  Admitted (:1679) rather than the Qed I predicted for the forward
  half — and Scott's fix message explains why: NOT a proof-effort
  choice but a RESIDUAL REFUTABLE CORNER he found while sketching
  the proof, which is MY ERRATUM #5 (see below). The corner:
  EH_alias with an entry-less SYMBOL canonical at a NON-VALUE kind.
  tenv_find_default sends symbols to any_value (the K_value top)
  REGARDLESS of the requested kind k, so for T = naked-float alias
  to an unstored symbol A with rho(A) = V_naked_float f, gamma T v
  held (G_equals kind premise checks against kind_of_ftype T =
  naked_float, satisfied by f) while gamma(any_value, f) is false —
  forward implication refuted pre-fix. THE FIX IS consistent's new
  clause 5 (rho symbol bindings pinned to K_value), which I had
  already assessed faithful (see KF-051 entry); with it the corner
  is closed AIRTIGHT because value_has_kind v k is kind_of_value v
  = k (Concretization.v:49-50), a function — so clause 5 makes the
  rho-bound sub-case contradict G_equals's non-value kind premise,
  and the fallback V_ptr (Addr_sym A) is K_value by construction
  (:39): the defaulted-symbol case is vacuous at non-value kinds
  and lands in any_value via G_unknown at K_value. VERIFIED all
  three corners of the intended Qed: stored-canonical via
  consistent clauses 3+4 (simple_eval determinism identifies the
  clause-3 witness with the G_equals value); defaulted-var via
  unknown_of_kind k + the KF-050 kind premise; defaulted-symbol via
  clause 5 + fallback as above. STATEMENT BLESSED for Scott's Qed
  attempt (he asked for reviewer sign-off before proving; Qed
  deferred to the polish wave with a compile loop — agreed).
- MY ERRATUM #5 (provability-claim class, not a rule-fidelity
  verdict): my re-sweep ledger row and this entry's original fix
  proposal both asserted the forward half was "provable today"
  ("likely Qed: ... G_unknown + the KF-050 premise when not
  [stored]"). FALSE as of the pre-clause-5 statement — I checked
  the unstored-VAR default (unknown_of_kind k, kind-correct) and
  generalized to the symbol default without noticing
  tenv_find_default's symbol arm IGNORES the requested kind. Scott
  found it; the corner is exactly my frame-blindness failure shape
  (erratum #3/#4): I brought the "does the default admit v" frame
  and never asked "at WHICH kind". Logged for the retrospective.

### KF-053 — T.Meet.GreatestLowerBound: a KNOWINGLY-refutable universal is kept as an Admitted axiom — its own comment discloses the refutation (T.Meet.MutableBlockMissedBottom, proved Qed twenty lines below) (high; Admitted-false family, disclosed-in-place variant)
- Chapter: 08-meet-join.md (T.Meet.GreatestLowerBound, conjectured
  design-intent with a doc-witnessed failure); Rocq:
  theories/MeetJoin.v:917-933 (comment + T_Meet_GreatestLowerBound,
  Admitted), :961-975+ (T_Meet_MutableBlockMissedBottom, Qed — the
  witness). Owner: Scott.
- What differs: the statement is the false universal itself. The
  comment at :923-925 says outright "Refutable as a universal
  claim: the sharpest witnessed failure is
  T.Meet.MutableBlockMissedBottom below, whose result gamma is
  disjoint from the left input's" — and then the statement is
  Admitted anyway. Admitted = usable axiom: any downstream file
  (All.v imports everything) can instantiate it at the
  MutableBlockMissedBottom meet (immediates-only variant vs
  Mutable_block -> non-Bottom Mutable_block result, empty
  extension), pick v = a pointer to a suitable mutable block in H
  (consistent with the empty E), and derive gamma T1 v for an
  immediates-only variant at a pointer — False.
- Why it matters: this is the KF-037 poison class in its purest
  form — not a subtle modeling gap but a disclosed refutation
  sitting as an axiom. The disclosure makes the STATEMENT honest
  prose-wise while leaving the AXIOM lethal. It predates the
  project's Admitted-false precedent being articulated (row 48's
  co-read logged the trio as clean at medium+; the meet-properties
  slice was Plotkin's, and the GLB comment's own admission was in
  nobody's frame as a finding — the sieve gap ran the other way
  from CF-11's).
- Proposed fix (writer's + main's shape call): the doc itself
  treats GLB as design intent WITH a witnessed failure, so the
  mechanization must not assert the universal. Options: (i) demote
  to a named Prop definition without asserting it (Definition
  T_Meet_GreatestLowerBound_claim : Prop := ... with the
  refutation note; zero axiom footprint, keeps the rule id
  greppable); (ii) state the true weaker version (GLB modulo the
  documented failure family) if a clean premise exists — likely
  ugly (the failure is a constructor case, not a side condition);
  (iii) documented-anchor form per the descriptive convention.
  (i) is the minimal honest shape.
- Status: OPEN (filed 2026-07-18, MeetJoin.v alias-channel
  re-sweep — found structurally, not through the alias channel;
  sent to Scott, cc main; Plotkin notified — row-48 slice
  courtesy). Disk probe 2026-07-18: KF-051/052 fixes landed but GLB
  is UNCHANGED (still the universal, Admitted at :936, refutation
  comment :926-928 intact, MutableBlockMissedBottom Qed :975-989)
  — consistent with the shape call waiting on main's STATUS-mapping
  ruling. Scott's fix message confirms: HELD, shape question sent
  to main with the named-Prop demotion (my option (i)) as his
  recommendation; the negation-Qed alternative (deriving False from
  the universal via MutableBlockMissedBottom) judged stronger but
  needing a compile loop to build the full-meet-layer refutation
  term blind. MeetJoin.v gets the edit after the ruling. Still the
  file's live poison axiom; All.v must not seal over it.
  DELTA (2026-07-22): RULED by main — option (a) as Scott and I
  recommended: demote to an unasserted named Prop Definition
  (T_Meet_GreatestLowerBound_claim), RULE comment retained with the
  true STATUS conjectured plus the demotion rationale (a
  doc-disclosed-refutable universal must not sit as a usable axiom;
  MutableBlockMissedBottom is the witness), rule id stays greppable,
  MeetJoin Admitted 12 -> 11. The convention break is sanctioned via
  a Traceability variant bullet plus a full KF-053 catalog record in
  main's pending batch (which also cross-references KF-051/052 to
  KF-054). Hopper lands the demotion after Dijkstra audits the
  batch; I verify the statement shape at green. Entry stays OPEN
  until that verification.
  DELTA 2 (2026-07-22): DEMOTION LANDED AND VERIFIED — RESOLVED.
  Method: diffed current MeetJoin.v against the pre-polish rescue
  snapshot, so the verification covers the whole file delta, not
  just the target lines. The batch is exactly three deltas: (1) THE
  DEMOTION (:920-942) — statement body byte-identical to the former
  Theorem's (the diff touches only the `Theorem` header line, now
  `Definition T_Meet_GreatestLowerBound_claim : Prop :=`, and the
  removed `Admitted.`); RULE header keeps STATUS conjectured (:920);
  the original refutation disclosure is retained and the new
  demotion comment carries main's ruling, catalog entry 73, the
  witness's name, the zero-axiom-footprint rationale, and the
  option-(b) upgrade path — item-for-item the REFUTED-CONJECTURE
  variant spec. (2) T_Meet_Store_CoercionErasure Admitted -> Qed
  (:963-981), statement unchanged, real proof (record_on unfold +
  fupd_lookup_hit + name_eqb_eq), entry-34-as-amended
  reflexivity-class disclosure in the comment; Qed-under-conjectured
  inventory 5 -> 6. (3) One comment typo fix (:1741 below->above).
  Real `Admitted.` terminators 12 -> 10 exactly as booked (grep -c
  'Admitted' misleads here — the new demotion comment itself
  mentions the word twice); .vo rebuilt after the edit. The file's
  live poison axiom is GONE; nothing blocks All.v from this entry.
  With this, the Admitted-false family
  (KF-037/038/041/043/044/049/050/051/052/053) is fully resolved on
  disk, and the open-discrepancy inventory is EMPTY pending the
  All.v gate review.

### KF-054 — consolidated doc-clarification candidate: the formalism never states where symbol bindings are guaranteed to live, and three independent findings in three files each patched a symptom of that silence (low-medium; doc lane, escalated to main; does NOT supersede KF-051's .v fix)
- Chapter: ch. 07 section 4 preamble (valuation rho and consistency;
  07-types-domain.md:646-658 territory) is the natural home; the
  seam also touches ch. 04's OS.Simple.Eval (symbol evaluation) and
  wherever heap/static symbol layout is introduced. Rocq sighting
  sites: Values.v:357-366 (simple_eval's Name_sym fallback to
  V_ptr (Addr_sym s) under an unbinding rho);
  Concretization.v:623-628 + :649-655 (sat_ext / consistent's
  literal rho-binding demands — the KF-051 mismatch); plus, as
  reported by Plotkin: his H_mod heap_sim discovery (heap_sim
  demanding both sides populated at symbols) and Dijkstra's
  entry-16 rho-first-with-fallback candidate. Owner: main (doc);
  consolidation per Plotkin's pattern observation, 2026-07-18.
- What differs: the model gives symbols a STATIC denotation
  (simple_eval is total on symbols: rho first, V_ptr (Addr_sym s)
  fallback), so symbols are evaluable with empty rho/H — while
  three separate obligations in three files each independently
  assume a symbol is BOUND somewhere: sat_ext's literal
  `rho n = Some v` per recorded equation (KF-051's refutation
  channel); heap_sim's both-sides-populated demand (Plotkin's
  H_mod discovery); the entry-16 evaluation-order candidate
  (Dijkstra). Three findings from three files reduce to one
  unstated doc premise: the doc never says whether symbol bindings
  are guaranteed by rho, by the heap's static segment, by
  te_defined_symbols, or by construction of the translation unit.
- Why it matters: each symptom got (or will get) a local fix, but
  without the doc stating the guarantee, every future file that
  touches symbols re-decides the seam independently — the exact
  condition under which the mechanization keeps sprouting
  KF-051-shaped mismatches. One sentence-level doc clarification
  ("symbol bindings live in X; valuations need not bind symbols;
  obligations on names must read symbols through their static
  denotation" — or whichever ruling main prefers) fixes the root.
- Relationship to open fixes: KF-051's sat_ext repair proceeds
  regardless (the .v must be self-consistent under whatever the
  doc says); this entry is about pinning the doc so the repair has
  a stated premise to cite rather than an inferred one.
- CROSS-REF to KF-052 (added 2026-07-18, Plotkin's suggestion via
  Scott): consistent's new clause 5 (`rho (Name_sym sym) = Some v
  -> value_has_kind v K_value`, Concretization.v:673-674) is now
  the MODEL-side answer to this seam — a fourth sighting, but the
  first stated as a positive guarantee rather than a patched
  symptom. If main's doc sentence lands, clause 5 is its direct
  transcription and the pair should be checked against each other
  (Dijkstra's doc-first audit is the natural place); if the doc
  sentence takes a different shape (e.g. bindings guaranteed by the
  heap's static segment rather than kind-restricted rho), clause 5
  may need to follow it.
- Status: OPEN (filed 2026-07-18; escalated to main as a doc
  FINDING per charter; Dijkstra cc'd so entry-16 links here;
  framing credit: Plotkin's three-sightings observation).
  DELTA (2026-07-22): DOC EDIT LANDED AND VERIFIED — RESOLVED. New
  paragraph at 07-types-domain.md:660-667 (§4 preamble, directly
  after the γ-conjectured note, before T.Gamma.Kind; git diff vs
  HEAD isolates it — the chapter was otherwise unmodified). Content:
  symbols need no entry in ρ; always evaluable, ⟦sym⟧ρ = ptr sym, a
  Value-kinded pointer to its static, irrespective of ρ (cites §04
  OS.Simple.Eval); no clause built on §4 (consistency of ρ with E,
  satisfaction of an extension, or a simulation premise over them)
  may demand that ρ literally bind a symbol — obligations read names
  the way evaluation does, through ⟦·⟧ρ; dually, a ρ binding for a
  symbol is a Value-kinded value, per symbols-denote-statics. All
  three sightings are covered: sat_ext/consistent (KF-051's channel,
  the "consistency/satisfaction" words), Plotkin's heap_sim
  (the "simulation premise" words), Dijkstra's entry-16 (the
  reads-through-evaluation sentence). The dual clause takes exactly
  the shape of Concretization.v's clause 5 (:673-674), so clause 5
  is its direct transcription and NO .v follow-up is needed;
  KF-051/052's landed repairs now have a stated doc premise to cite.
  Text-only — no RULE/STATUS movement.
  DELTA 2 (2026-07-22, main's self-caught AMENDMENT re-verified —
  PASS, stays RESOLVED). The original sentence's "⟦sym⟧ρ = ptr sym
  … irrespective of ρ" was the literal OS.Simple.Eval reading, which
  the wave-0 record already knew to be a doc gap: OS.Let.Static
  ρ-binds each static set-of-closures symbol to a closure value
  (04-opsem.md:348 "each symⱼ ↦ clos ℓ fⱼ"), and a
  literal-ptr-always reading strands static-closure projections.
  Amended text now reads "through ρ when ρ binds it (as
  OS.Let.Static arranges for static sets of closures), and to the
  static pointer ptr sym otherwise" — verified EXACT against
  simple_eval's ρ-first-with-fallback shape (Values.v:357-366, whose
  ENCODING NOTE :347-356 states the same rationale). "In either case
  a Value-kinded value" keeps clause 5 the direct transcription (the
  fallback branch is V_ptr, K_value by construction; the ρ branch is
  clause 5). The guarantee sentence (no literal-ρ-binding demands;
  read through ⟦·⟧ρ) is retained verbatim, so the three-sightings
  coverage is unchanged. Text-only, 9-line insertion, no RULE/STATUS.
  RIDER — ch. 04 ruling (main delegated the call to this lane):
  LOAD-BEARING; recommend a one-sentence NOTES amendment on
  OS.Simple.Eval. Grounds: (i) the rule's equation line
  (04-opsem.md:129 "⟦sym⟧ρ = ptr sym", unconditional) now
  contradicts the ch. 07 paragraph THAT CITES IT, for exactly the
  SOC-symbol case; (ii) ch. 04 is internally inconsistent on its
  own: OS.Let.Static's conclusion (:353 "ρ[symⱼ ↦ ptr symⱼ …]")
  appears to bind ptr symⱼ while its match clause (:348) binds
  clos ℓ fⱼ — the override cannot be reconstructed from ch. 04
  alone, and ch. 04 is the foundational chapter; (iii) the KF-054
  principle itself: the seam is now stated in ch. 07, but the
  foundational equation still misleads a ch. 04-only reader, which
  is how the seam sprouted three symptoms in the first place.
  Candidate sentence (NOTES of OS.Simple.Eval): "When ρ rebinds a
  symbol — OS.Let.Static does this for static sets of closures,
  binding symⱼ to clos ℓ fⱼ — ρ takes precedence; the equation
  states the denotation for symbols ρ does not rebind (Block_like
  and imported symbols). See 07 §4's preamble." Optionally clarify
  OS.Let.Static's conclusion "…" similarly. Text-only, no STATUS
  movement, same class as the KF-045/054 edits.
  RIDER RESOLVED (2026-07-22, same day): main ACCEPTED the ruling
  and landed BOTH parts — verified via git diff (two hunks, same
  RULE ids, STATUS untouched): (1) OS.Simple.Eval NOTES
  (04-opsem.md:136-141) carries my candidate sentence nearly
  verbatim plus a ch. 07 §4 cross-reference; (2) beyond my minimum,
  OS.Let.Static's conclusion is CORRECTED — ρ[symⱼ ↦ ptr symⱼ …]
  -> ρ[symⱼ ↦ vⱼ …] with a NOTES sentence defining vⱼ (clos ℓ fⱼ
  for SOC symbols, ρ-precedence noted; ptr symⱼ for Block_like) —
  which closes ground (ii), the internal :348-vs-:353
  inconsistency. Mechanization already matches the corrected line,
  verified down to definitions: OS_Let_Static (Opsem.v:690) walks
  install_static, whose IS_set_of_closures clause binds via
  bind_closure_syms (:413-417, env_upd … (V_clos l f)) and whose
  IS_block_like clause binds V_ptr (Addr_sym sym) (:459-467) — the
  doc moved TO the .v; no .v follow-up. Note hunk (2) is rule-BODY
  text (not NOTES-only), though id|status pairs are unmoved —
  disclosed to Dijkstra for his stamp. TWO LOW RESIDUES, neither
  blocking: (a) ch. 04 §1.4 prose (:117-118) still states the
  literal "a symbol sym resolves to ptr sym" without the override,
  two paragraphs above the corrected rule — one parenthetical would
  resync it; (b) Opsem.v's OS_Let_Static header comment (:687-689)
  quotes the OLD conclusion shape ("rho[sym_j |-> ptr sym_j, ...]
  (and clos l f_j for closure symbols)") — content-equivalent but
  no longer quoting the doc's line; comment-only resync, polish-wave
  candidate. The KF-054 seam is now closed at the root in all three
  places: foundational chapter, types chapter, mechanization.

### KF-055 — the revised P.Binary.PhysEqual's letter leaves defined comparisons without derivations: identity-pinned immutables fall through every clause, and ch. 04's `=`-premises don't accept relational results (high; on the item-8 resolution package, ruling-review finding; the DESIGN is right — the letter lags it in three places)
- Chapter: 06-primitives-memory.md:1191-1234 (revised PhysEqual),
  04-opsem.md (OS.Let.Prim.Pure/Effect premises `⟦p⟧(v̄;H) = (v,H′)`),
  13-soundness.md:33-46 (§1 folding enumeration), :751-754 (the §4
  RESOLVED paragraph that already records the correct decision).
  Owner: main (doc).
- What differs, three legs of one gap:
  (a) The ι-class decision EXCLUDES Immutable_unique blocks/arrays
  (extension constructors; 13 §4:751-754 — compiler refuses to
  share/duplicate them, exception matching observes identity;
  corroborated by S.Rewrite.CSE.Eligible :439, whose generative
  clause admits Only_generative_effects Immutable ONLY). But the
  NORMATIVE definition site (06:1191-1193 "an immutable block …
  an immutable array") and 13 §1's enumeration carry no qualifier —
  per the mutability grammar (06:62, {Immutable, Immutable_unique,
  Mutable}) their letter INCLUDES unique.
  (b) With unique correctly excluded, unique operands take the
  "neither operand is ι" clauses — whose 1-clause reads "same
  immediate, same pointer to a MUTABLE object, or both null"
  (06:1195-1197). A same-pointer pair of Immutable_unique objects
  satisfies neither the 1-clause (not mutable) nor the 0-clause
  (they ARE the same word): NO derivation. Stuck = undef
  (OS.Unit.Final) — exception dispatch (compare a raised extension
  constructor with itself) becomes UB by the letter of the rule.
  The :1226 bullet ("Two pointers to mutable objects…") has the
  same gap.
  (c) The rule's relational clauses use `∋` "per the notation for
  nondeterministic denotations" — no such notation is defined
  anywhere (grep: ∋ occurs only at 06:1205/:1208), and ch. 04's
  OS.Let.Prim.Pure/Effect premises are written `⟦p⟧ = (v,H′)`. Read
  literally, a relational result never matches an `=`-premise, so
  NO machine transition exists for any ι-comparison — every
  phys_equal on immutables would be stuck = UB, collapsing the
  resolution. (The .v is immune — denot is already a relation — but
  the doc is the normative layer; KF-054's lesson: pin the
  cross-chapter reading where it is consumed.)
- Why it matters: (b) and (c) make defined programs undef in the
  letter of the semantics; (a) is the decision recorded at the
  wrong layer (a §4 findings entry is not a definition site).
- Candidate fixes (three one-liners + one sentence): 06 ι-definition
  gains "excluding Immutable_unique blocks/arrays (extension
  constructors — identity-pinned; 13 §4 item 8)"; 06's word-equality
  clause and the :1226 bullet say "mutable or Immutable_unique
  object"; 13 §1's enumeration gains the same two words; one
  sentence where ⟦p⟧'s judgment forms are declared (ch. 06 preamble
  or ch. 04): relational denotations are written ⟦p⟧(v̄;H) ∋ (v,H′),
  and the OS.Let.Prim.* `=`-premises read as membership for them.
- Status: OPEN (filed 2026-07-22, item-8 ruling review; to main,
  pre-mechanization blocker — Hopper holds anyway).
  DELTA (2026-07-22, re-verified against final disk after Church's
  round + main's three late fixes): legs (a) and (b) RESOLVED —
  the ι-definition now carries the exclusion at the normative site
  (06:1191-1198, "immutable (NOT immutable-unique) block or array",
  with the Eligible_for_cse corroboration quoted and "they compare
  exactly, like mutable objects" routing unique pairs through the
  exact clauses' primary same-machine-word condition), and 13 §1:41
  pins "only non-ι objects — mutable and Immutable_unique alike".
  Two COSMETIC residues on leg (b): the exact 1-clause's
  illustrative parenthetical (06:1201 "same pointer to a mutable
  object") and the :1237 bullet still omit unique from the
  illustration — substance is covered by :1197-1198; two words each
  for completeness. Leg (c) STILL OPEN and still the one
  pre-mechanization letter blocker: ∋ remains used only at
  06:1205/:1208, "the notation for nondeterministic denotations"
  is defined nowhere, and ch. 04's OS.Let.Prim.* =-premises still
  lack the membership-reading sentence — without it no machine
  transition exists for any relational result. Also verified in the
  same pass: Church's fold-to-1 extension (identical canonicals OR
  provably-same immediate/null word, check_heads' two cases,
  :1223-1226) and the two-branch fold-to-0 (:1226-1232: never from
  distinct DYNAMIC allocations; distinct symbols via always-0 when
  an operand is ι, via the exact clause otherwise — mutable statics
  the live case) — both sound; I probed the SOC-symbol corner (two
  slots of one set: clos ℓ f vs clos ℓ g are distinct values,
  closures-ι gives the relational branch, and literal-slot leaf
  treatment keeps 1 underivable) and the mixed unique/ι pair (1
  underivable — pinned leaf never fold-equals an ι object; 0 in
  envelope) — no new holes.
  DELTA 2 (2026-07-22, closure cluster verified): ENTRY CLOSED —
  all three legs and both cosmetic residues now on disk. Leg (c):
  ch. 06's ⟦p⟧ judgment preamble (:34-43) declares relational
  denotations ("⟦p⟧(v̄; H) ∋ (v, H′) — the judgment relates the
  application to every result so listed") WITH the membership
  reading of ch. 04's OS.Let.Prim.* =-premises ("the machine may
  take any derivable result") — exactly the required sentence, at
  the right site. Leg (b) cosmetics: the word-equality gloss
  (:1212) and the pointers bullet (:1248) both read "mutable or
  Immutable_unique". Leg (a) second half: 13 §1's enumeration now
  "immutable — not Immutable_unique — blocks and arrays". Also
  verified in the same cluster: 11-inlining.md:170 now
  "the right-hand configuration observationally refines the left"
  (direction CORRECT — right-hand = the rewritten side, matching
  §2's C[e′] refines C[e]). The letter now matches the design
  everywhere this entry touched.

### KF-056 — INV.ToCmm.Simulates' current direction (∀ Flambda behavior ∃ Cmm outcome) is refutable under the revised relational PhysEqual; the re-posing must flip to the "Cmm refines Flambda" direction EndToEnd's own gloss already names (high; design-direction ruling on the item-8 package; disclosed-in-place by the "to be re-posed" marker — this entry pins WHAT the re-posing must say)
- Chapter: 20-to-cmm-soundness.md §2 (Simulates, "to be re-posed"
  per the :74-78 FORMER-VIOLATION header), §3 EndToEnd :95-115.
  Owner: main (doc) / Milner-lane .v when re-posed.
- What differs: the pre-revision Simulates shape (kept in the .v:
  given fl behavior b of U′, exists Cmm outcome o with
  tocmm_beh_rel b o) is REFUTABLE under the revised PhysEqual:
  an abstract run resolving phys_equal(ptr ℓ, ptr ℓ) → 0 on an
  ι-operand takes a branch the emitted word-equality Cmm code never
  takes; its observation (different module value, say) relates to
  no outcome of the deterministic comparison the program P actually
  contains. Sound direction: ∀ Cmm outcome o ∃ Flambda behavior b
  of U′ with tocmm_beh_rel b o (given U′ undef-free) — "≈-refinement
  of Flambda by Cmm", which is verbatim EndToEnd's :101-102
  justification gloss. Composition then goes through cleanly:
  Preserves (U′ ⊑fold U₀) ∘ re-posed Simulates (P ⊑≈ U′) ⇒ EndToEnd
  (P refines U₀) — and EndToEnd's conclusion :97 was already
  refinement-shaped, so it survives unchanged. Two riders: (i)
  EndToEnd's :101 parenthetical "INV.Simplify.Preserves
  (equivalence on Flambda observations)" is now stale — one word,
  "refinement". (ii) §2's decomposition sentence (INV.ToCmm.Control
  + TC.Prim.Sound) describes forward-simulation machinery; a
  backward-direction top statement additionally needs a
  determinacy-modulo-oracles bridge (GC/extern/alloc
  nondeterminism) — one NOTES sentence at re-posing time.
- Why it matters: without the direction pinned, the .v re-posing
  could faithfully transcribe the old shape and mint a fresh
  Admitted-refutable — the KF-037 class — on the flagship
  simulation.
- Status: OPEN (filed 2026-07-22, item-8 ruling review; direction
  ruling delivered to main; .v holds with Hopper until doc
  re-posing lands).
  DELTA (2026-07-22, consolidated-ruling round): direction ruling
  RESTATED as binding for Hopper's re-pose: her "quotient the
  Flambda behavior, tocmm_beh_rel untouched" shape is CONFIRMED for
  the fold-placement half (matches my R.Observe verification) but
  is only half the re-pose — the quantifier flip (∀ Cmm outcome
  ∃ Flambda behavior) is the other half and non-negotiable per this
  entry's witness. Rider (i) still open on disk: EndToEnd :101
  "(equivalence on Flambda observations)". Endorsed from her prep:
  obs_refines as obs_equiv's second conjunct with fold-extended
  similarity (transformed ⊆ original — matches 13 §1 as verified),
  and the reflexivity-lemma family as the formal witness of the
  identity-free-both-directions sentence, which is also what
  reconciles the always-derivable-0-on-equal-pointers clause with
  fold_eq reflexivity (the denotation is a set; both memberships
  hold on the diagonal).
  DELTA 2 (2026-07-22, same session): RE-POSE LANDED AND VERIFIED —
  RESOLVED. 20 §2's Simulates now states the backward claim
  verbatim per this entry ("EVERY outcome of the Cmm run … ∀ Cmm
  outcome ∃ Flambda behaviour, '≈-refinement of Flambda by Cmm'",
  :51-54), per-outcome cases each existential, trace preservation
  incremental with fold-compared snapshots; the NOTES cite KF-056
  by id, state the former forward direction's refutation with my
  witness, and carry rider (ii) as the determinacy-modulo-oracles
  bridge (:79-87: forward machinery over abstract runs whose
  identity resolutions match the emitted code's; backward claim by
  Cmm determinacy given the oracles' choices). I adversarially
  checked the bridge: the code-matching abstract resolution always
  EXISTS — word-equal comparisons resolve to 1 via fold reflexivity
  (Hopper's booked reflexivity family is thus load-bearing here,
  not just for the both-directions sentence), word-distinct via the
  always-0 clause (ι) or the exact clause (non-ι) — so the witness
  construction is total. Rider (i) FIXED: EndToEnd :118 now
  "(refinement on Flambda observations)". NEW cosmetic residue: the
  ch. 20 PREAMBLE (:10-12) still says ch. 13 is "an equivalence
  within one machine" and calls this chapter's property "a forward
  simulation" — both now imprecise (refinement; backward claim via
  forward machinery); one sentence.
  DELTA 3 (2026-07-22): preamble residue FIXED and verified —
  20:9-14 now "the Cmm run refines the Flambda run, through ≈";
  ch. 13 described as "a refinement within one machine (up to
  immutable-identity folding, 13 §1)"; this chapter's property as
  "a refinement between two machines … established by
  forward-simulation machinery plus determinacy of the target
  modulo its oracles (§2 NOTES)" — the machinery-vs-property
  distinction drawn exactly as this entry required. NOTHING remains
  open on KF-056; the item-8 letter items are all closed except
  KF-057's catalog sentence, which rides main's batch.

### KF-057 — the refinement reading carries a new implicit premise: cextern's licensed answers must be monotone under immutable-identity folding of its heap argument (medium; discipline note + watch, not poison)
- Chapter: 13-soundness.md §1 (refinement definition); catalog
  entry 20 territory (cextern_rel) + the :580 KF-015 discipline
  note. Owner: main (catalog/doc sentence), Dijkstra (watch).
- What differs: dropping/sharing/lifting immutable allocations
  changes the heap PASSED TO later cexterns (not just the compared
  snapshots — the oracle's INPUT). Refinement of the composite run
  needs: if H′ folds into H, cextern's licensed answers at H′ are
  within (up to folding) its answers at H. Nothing on disk states
  this. NOT the Admitted-false class: cextern is an abstract
  Parameter, so no in-development False is derivable (contrast
  KF-053) — but an intended instantiation violating it falsifies
  the conjecture's intent, and the EffectLinear Drop-arm answer
  ("counters are not a modeled observable") silently leans on it:
  the counter channel is closed precisely because the oracle is
  ASSUMED not to read allocation history out of dom(H).
- Candidate fix: one sentence at the cextern catalog entry (and/or
  13 §1): the intended cextern instantiation answers up to
  immutable-identity folding of its heap argument — the same
  discipline family as the KF-015 note; plus a Dijkstra watch
  keyed to any future cextern constraint or instantiation (the
  gc_reloc/W-32 pattern).
- Status: OPEN (filed 2026-07-22, item-8 ruling review).

<!-- Finding template:
### KF-NNN — <RULE id> (<severity: high/med/low>)
- Chapter: <file>:<line>; Rocq: theories/<X>.v:<line>; Owner: <writer>
- What differs:
- Why it matters:
- Status: open | resolved | rebutted | escalated — <one-line disposition>
-->
