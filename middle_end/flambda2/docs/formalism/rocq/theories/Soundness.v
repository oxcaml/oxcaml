(** * Soundness.v -- what Simplify preserves (ch. 13)

    Mechanizes 13-soundness.md: the observational-equivalence
    apparatus of its section 1 (a prose definition there, not a rule
    block), the "does not exhibit undefined behaviour" hypothesis,
    and the chapter's twelve statements.

    Owner: Plotkin (wave 6).
    Imports: Base, Syntax, Values, Opsem, WellFormed, TypeGrammar,
    PrimMemoryA, Concretization, MeetJoin, Machine, Simplify. *)

(* DRAFT NOTE (task #24): increments 1 (observation apparatus:
   obs_equiv, no_ub), 2a (the eight anchors, per the sanctioned
   split -- CORRESPONDENCE.md traceability exception), the
   one-hole context type (expr_ctx / ctx_plug, approved by main),
   and 2b (INV.Simplify.Preserves, INV.Rewrite.Local,
   INV.NameMode.Coherent, INV.Simplify.AliasesMonotoneDown) are
   ALL in: the file's rule census is complete at 12 headers,
   4 Admitted.  COMPILE GATE cleared: Simplify.v (incl. the
   W-24 3-way mutual simplifies block) is green; this file is
   in Hopper's compile loop.  Rulings
   applied: Preserves' code-environment hypothesis conjoins
   (forall cid c0, C cid = Some c0 -> code0_wf c0), code0_wf
   consumed BY NAME (four conjuncts since KF-023 added the
   my_alloc_mode region disequality; never destructure the
   bundle as a fixed conjunction); expr_wf does not conjoin
   WF_Prim_ArgKinds at Let-of-prim (Knuth's W-21) -- no 2b
   statement ended up needing it explicitly.  Proof-phase
   pointer (Church): Preserves' induction owes code0_wf
   preservation for rw_code_loopify's rebuilt code item
   (wrap_loopified); discharge premises are local in
   S_Rewrite_Loopify_Body, and with the W-24 mutual block the
   discharge site is Simplify.v's S_code_rebuild.  Any induction
   descending through S_code_rebuild needs a combined Scheme
   over simplifies/loopify_simplifies/simplify_code (the default
   principle is the weak non-mutual one); Pilot.v's increment-2
   instance exhibits a derivation, so it does not. *)

(* String BEFORE List: String's length/concat/get must not shadow the
   list ones (build-captain guidance). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Syntax Values Opsem WellFormed
  TypeGrammar PrimMemoryA Concretization MeetJoin Machine Simplify.
Import ListNotations.

Set Implicit Arguments.

(** ** Observations -- 13-soundness.md section 1

    "Two terms are observationally equivalent when, started from the
    same heap, they induce the same observations: the same C-call
    effect trace and the same termination outcome (including the
    same final module block value at sym_mod)."  Alpha-renaming and
    coercions are not observable -- they never reach the behavior
    type, so nothing here mentions them.

    This paragraph is prose in the chapter (no rule block), so it is
    mechanized as ordinary Definitions carrying this citation rather
    than under a traceability header.

    ENCODING NOTE (observation up to a location bijection): the
    behaviors of Opsem.v carry the final heap (see the ENCODING NOTE
    at [behavior] there); the restriction to the reachable
    substructure and the renaming of dynamic locations happen HERE.
    Rationale, per the joint reading with ch. 15 and ch. 19:
    - Locations are opaque (ch. 19: the abstract machine has "a
      logical heap of opaque locations"), and ch. 15's CM.Unit.Final
      compares "the bytes of the module block reachable from the
      module symbol" through the representation relation, never by
      absolute address.  So the module-block observation is the
      structure reachable from sym_mod, compared up to a partial
      bijection [b] on addresses that is the identity on symbols.
    - Reachability is not a separate definition: [fold_heap_sim]
      quantifies only over the pairs of [b], and [fold_osim] sends
      every pointer field of a compared object back into [b] or
      (for iota-objects, post-item-8) into structural descent, so
      seeding [b] with the observation roots (the sym_mod address in
      [Bsim_return]; the exception value in [Bsim_exn]; the trace
      values) makes the comparison close over exactly the reachable
      substructure.  A larger [b] only constrains more, and [b] is
      chosen existentially, so the minimal reachable-closed choice
      is the operative one.
    - The doc's "the same C-call effect trace": trace events carry
      runtime VALUES (Ev_ccall_return / Ev_ccall_raise), which may
      be pointers; literal equality of location-carrying values
      would contradict location opacity, so trace values are
      compared through the same bijection [b].  Knuth CONCURS
      (fidelity review W-1) with two strengthenings, both applied:
      ONE bijection scoped over the entire behavior (every event,
      the exception payload, and the final observation share [b] --
      external code can retain a pointer from one C call and
      compare it at the next, so cross-event physical identity is
      observable at the boundary), and [b] is the IDENTITY on every
      address populated in the shared starting heap ([pins] below:
      the doc quantifies "from every starting heap", the context
      retains pointers into that heap, so initial-heap objects keep
      their identity and their final contents are observable).
      Mutable-through-events aliasing (external code writing back
      through retained pointers) is sound to ignore here only
      because cextern_rel already threads the heap through each
      event -- the oracle sees and produces real heaps, so a
      write-back is an ordinary heap update, not a hidden channel.
    - Closures are compared as LEAVES ([FO_closures], Values.v,
      relates any two Closures objects): Simplify legitimately
      renames code ids,
      rewrites code bodies, and prunes dead value slots
      (INV.Simplify.DeadValueSlotCoherence), so a structural
      comparison of Closures objects would falsify
      INV.Simplify.Preserves on its own validation examples.  A
      first-order "reachable structure" observation stops at a
      closure; a closure's behavior is observable only by applying
      it, which is the whole-program part of the statement.
      [FV_clos] requires the SAME function slot literally --
      RESOLVED correct: Simplify never renames the slots of an
      existing set (simplify_set_of_closures.ml:687 uses
      Function_slot.rename only to derive a lifted-closure SYMBOL
      name; the set's function_decls keys are untouched), and the
      one fresh-slot site (simplify_apply_expr.ml:468, the NEW
      partial-application wrapper closure) is absorbed by the
      existential f_pa in OS.Apply.IndirectUnknownArity.Partial.
    - Events additionally carry the CALL-TIME heap (Opsem.v's
      ENCODING NOTE at [event]); see [event_sim] below.  The
      externally-readable fragment at event j is seeded
      CUMULATIVELY (everything carried by events 1..j-1, not just
      event j's own callee/arguments): external code retains
      pointers across calls, so a mid-trace divergence in a
      C-shared buffer is observable at a later call even if the
      runs re-converge before the final heap (Knuth KF-015,
      resolved by the [seen] accumulator in [trace_sim_from]).
    - ITEM-8 AMENDMENT (2026-07-22; CORRESPONDENCE entries 44/75):
      observations are now compared UP TO FOLDING of iota-objects
      (13 section 1, adopted with the P.Binary.PhysEqual revision).
      The location relation [b] generalizes from a full partial
      bijection to the fold discipline ([fold_bij] below): bijective
      on non-iota objects only, unconstrained on iota-objects, with
      loc-to-symbol pairs admitted for lifting; value/object/heap
      similarity is Values.v's class-aware fold core, which descends
      structurally through iota-objects.  The event apparatus keeps
      its KF-015 shape; only the [b]/[b_ev] discipline changes.
      Closures stay leaves, per ruling (c) at the fold core. *)

(** The location-relation discipline (ITEM-8 AMENDMENT, entry 44;
    formerly a full partial bijection): the simulation core --
    fold_vsim / fold_osim / fold_heap_sim and the iota-class -- is
    hosted in Values.v; this file instantiates it with the
    OBSERVATION discipline.  [b] is a partial bijection on NON-iota
    objects only (mutables and Immutable_unique keep pinned
    identities, matched one-to-one) and is UNCONSTRAINED by
    injectivity/functionality on iota-objects (one shared target
    block may stand for two source blocks and vice versa -- CSE
    sharing and re-boxing respectively); a source-side symbol maps
    only to itself, while a target-side symbol may also be reached
    from a dynamic iota-location (lifting).  The classifying heaps
    [Hcls]/[Hcls'] are the heaps at the comparison point (call-time
    heaps in event_sim, final heaps in beh_sim); an address's class
    is stable once allocated (immutability is permanent, addresses
    are not reused), so any populating heap classifies
    consistently. *)
Record fold_bij (Hcls Hcls' : heap)
    (b : address -> address -> Prop) : Prop :=
  mk_fold_bij {
  fb_functional : forall a a1 a2,
      b a a1 -> b a a2 -> ~ iota_addr Hcls a -> a1 = a2;
  fb_injective : forall a1 a2 a,
      b a1 a -> b a2 a -> ~ iota_addr Hcls' a -> a1 = a2;
  fb_sym_l : forall s a, b (Addr_sym s) a -> a = Addr_sym s;
  fb_sym_r : forall a s,
      b a (Addr_sym s) -> a = Addr_sym s \/ iota_addr Hcls a
}.

(** [b] pins the starting heap: it is the identity on every address
    populated in [H0].  Both runs start from the SAME heap and the
    external world retains pointers into it, so initial-heap objects
    keep their physical identity (and, via [heap_sim], have their
    final contents observed). *)
Definition pins (H0 : heap) (b : address -> address -> Prop)
  : Prop :=
  forall a o, H0 (HK_addr a) = Some o -> b a a.

(** Value, object and heap similarity are the fold simulation core
    of Values.v ([fold_vsim] / [fold_osim] / [fold_heap_sim]),
    heap-parameterized so that iota-objects relate by structural
    descent through the compared heaps while non-iota pointers and
    closures go through [b] (entry 44's ITEM-8 AMENDMENT; the
    pre-item-8 [value_sim]/[obj_sim]/[heap_sim] of this file were
    the b-channel-only special case).  Together with the root
    seeding in [beh_sim], [fold_heap_sim] is "the reachable
    substructures coincide up to renaming and iota-folding". *)

Definition sub_rel (b1 b2 : address -> address -> Prop) : Prop :=
  forall a a', b1 a a' -> b2 a a'.

(** C-call trace similarity: same events, values compared under
    [b], and the memory the external can READ at the call compared
    at call time.  Each event carries H_call (Opsem.v); the
    comparison uses a per-event sub-bijection [b_ev] of the
    behavior-wide [b], seeded with what the external RETAINS at
    that moment -- the initial-heap pointers ([pins]), every value
    carried by every EARLIER event ([seen]: callees, arguments,
    results and raise payloads -- external code can stash a pointer
    at one call and read through it at the next; Knuth KF-015), and
    this event's callee and arguments -- and heap-consistent at the
    CALL-TIME heaps.  A sub-relation is needed because [b] also
    relates locations allocated AFTER this event, which H_call does
    not populate; since the fold discipline is classified per
    comparison point, [b_ev] carries its own [fold_bij] at the
    call-time heaps (the pre-item-8 shape inherited bijectivity
    from the behavior-wide [b]).  Results are compared under the
    full [b]: they enter the program and are observed at later
    points (and join [seen] for later events); a fresh iota result
    is not yet in H_call, so its structural folding is anchored
    where later comparisons populate it (the next event's [seen]
    check, or the final heaps). *)
Inductive event_sim (H0 : heap) (b : address -> address -> Prop)
  (seen : list (value * value)) : event -> event -> Prop :=
| Esim_ccall_return : forall b_ev vf vf' args args' Hc Hc' rs rs',
    sub_rel b_ev b ->
    pins H0 b_ev ->
    fold_bij Hc Hc' b_ev ->
    Forall (fun vv => fold_vsim b_ev Hc Hc' (fst vv) (snd vv)) seen ->
    fold_vsim b_ev Hc Hc' vf vf' ->
    Forall2 (fold_vsim b_ev Hc Hc') args args' ->
    fold_heap_sim b_ev Hc Hc' ->
    Forall2 (fold_vsim b Hc Hc') rs rs' ->
    event_sim H0 b seen (Ev_ccall_return vf args Hc rs)
      (Ev_ccall_return vf' args' Hc' rs')
| Esim_ccall_raise : forall b_ev vf vf' args args' Hc Hc' v v',
    sub_rel b_ev b ->
    pins H0 b_ev ->
    fold_bij Hc Hc' b_ev ->
    Forall (fun vv => fold_vsim b_ev Hc Hc' (fst vv) (snd vv)) seen ->
    fold_vsim b_ev Hc Hc' vf vf' ->
    Forall2 (fold_vsim b_ev Hc Hc') args args' ->
    fold_heap_sim b_ev Hc Hc' ->
    fold_vsim b Hc Hc' v v' ->
    event_sim H0 b seen (Ev_ccall_raise vf args Hc v)
      (Ev_ccall_raise vf' args' Hc' v').

(** The value pairs an event pair leaves in the external world's
    hands: the callee, the arguments, and what came back (results,
    or the raise payload the external constructed).  Mismatched
    constructors never occur under [event_sim]; the [] fallback is
    for totality only. *)
Definition retained (ev ev' : event) : list (value * value) :=
  match ev, ev' with
  | Ev_ccall_return vf args _ rs, Ev_ccall_return vf' args' _ rs' =>
      (vf, vf') :: combine args args' ++ combine rs rs'
  | Ev_ccall_raise vf args _ v, Ev_ccall_raise vf' args' _ v' =>
      (vf, vf') :: combine args args' ++ [(v, v')]
  | _, _ => []
  end.

(** Trace similarity accumulates the retained pairs: event j's
    sub-bijection must also relate everything carried by events
    1..j-1 (the cumulative-seed resolution of KF-015). *)
Inductive trace_sim_from (H0 : heap)
  (b : address -> address -> Prop)
  : list (value * value) -> list event -> list event -> Prop :=
| Tsim_nil : forall seen, trace_sim_from H0 b seen [] []
| Tsim_cons : forall seen ev ev' tr tr',
    event_sim H0 b seen ev ev' ->
    trace_sim_from H0 b (seen ++ retained ev ev') tr tr' ->
    trace_sim_from H0 b seen (ev :: tr) (ev' :: tr').

Definition trace_sim (H0 : heap) (b : address -> address -> Prop)
  : list event -> list event -> Prop :=
  trace_sim_from H0 b [].

CoInductive stream_sim_from (H0 : heap)
  (b : address -> address -> Prop)
  : list (value * value) -> event_stream -> event_stream -> Prop :=
| Ssim_cons : forall seen ev ev' s s',
    event_sim H0 b seen ev ev' ->
    stream_sim_from H0 b (seen ++ retained ev ev') s s' ->
    stream_sim_from H0 b seen (Es_cons ev s) (Es_cons ev' s').

Definition stream_sim (H0 : heap)
  (b : address -> address -> Prop)
  : event_stream -> event_stream -> Prop :=
  stream_sim_from H0 b [].

(** Behavior similarity: same termination outcome, same trace, and
    for the terminating outcomes the same observed heap structure
    up to iota-folding -- rooted at sym_mod for normal termination
    (13-soundness.md section 1: "the same final module block value
    at sym_mod") and at the exception value for an uncaught
    exception.  ONE location relation is chosen per pair of
    behaviors, scoped over the whole behavior (all events and the
    final observation), pinning the shared starting heap [H0]; its
    fold discipline ([fold_bij]) is anchored at the final heaps for
    the terminating outcomes and per-event (inside [event_sim]) for
    the others, which carry no final heap to classify by. *)
Inductive beh_sim (sym_mod : symbol) (H0 : heap)
  : behavior -> behavior -> Prop :=
| Bsim_return : forall b tr tr' Hf Hf',
    fold_bij Hf Hf' b ->
    pins H0 b ->
    b (Addr_sym sym_mod) (Addr_sym sym_mod) ->
    trace_sim H0 b tr tr' ->
    fold_heap_sim b Hf Hf' ->
    beh_sim sym_mod H0 (Beh_return tr Hf) (Beh_return tr' Hf')
| Bsim_exn : forall b tr tr' v v' Hf Hf',
    fold_bij Hf Hf' b ->
    pins H0 b ->
    trace_sim H0 b tr tr' ->
    fold_vsim b Hf Hf' v v' ->
    fold_heap_sim b Hf Hf' ->
    beh_sim sym_mod H0 (Beh_exn tr v Hf) (Beh_exn tr' v' Hf')
| Bsim_diverge : forall b tr tr',
    pins H0 b ->
    trace_sim H0 b tr tr' ->
    beh_sim sym_mod H0 (Beh_diverge tr) (Beh_diverge tr')
| Bsim_react : forall b s s',
    pins H0 b ->
    stream_sim H0 b s s' ->
    beh_sim sym_mod H0 (Beh_react s) (Beh_react s')
| Bsim_undef : forall b tr tr',
    pins H0 b ->
    trace_sim H0 b tr tr' ->
    beh_sim sym_mod H0 (Beh_undef tr) (Beh_undef tr').

(** Observational equivalence of two behavior SETS (13-soundness.md
    section 1).  A behavior set is any [behavior -> Prop];
    Machine.v's [fl_unit_behavior flags u rho_pre H0] is the
    canonical instance.  The section-1 quantification "from every
    starting heap" lives in the theorem statements (which quantify
    the initial heap and pre-environment); [H0] appears here only as
    the pinning set for the bijection ([pins]).
    NOTE: [pins] plus [heap_sim] means the final contents of every
    initial-heap object are observed for terminating outcomes, not
    just the sym_mod-reachable part -- deliberate: the context
    retains pointers into the starting heap, so mutations to it are
    observable even when unreachable from the module block. *)
Definition obs_equiv (sym_mod : symbol) (H0 : heap)
    (B1 B2 : behavior -> Prop) : Prop :=
  (forall beh1, B1 beh1 ->
     exists beh2, B2 beh2 /\ beh_sim sym_mod H0 beh1 beh2) /\
  (forall beh2, B2 beh2 ->
     exists beh1, B1 beh1 /\ beh_sim sym_mod H0 beh1 beh2).

(** Observational REFINEMENT (13-soundness.md section 1, item-8
    resolution; entry 44: the second conjunct of [obs_equiv] alone):
    every behavior of the transformed side [B2] has a source-side
    match in [B1].  Refinement rather than two-sided equivalence is
    FORCED, not chosen: Simplify legitimately RESOLVES the identity
    looseness -- the simplify_phys_equal folds, CSE sharing, lifting
    and re-boxing each prune outcomes on the identity dimension --
    so no two-sided statement survives the loose denotation.  On
    programs whose observations never consult loose identity,
    refinement holds in both directions and coincides with
    [obs_equiv] (witnessed by the reflexivity family in Values.v). *)
Definition obs_refines (sym_mod : symbol) (H0 : heap)
    (B1 B2 : behavior -> Prop) : Prop :=
  forall beh2, B2 beh2 ->
    exists beh1, B1 beh1 /\ beh_sim sym_mod H0 beh1 beh2.

(** "U does not exhibit undefined behaviour" -- the modulo-UB
    hypothesis of INV.Simplify.Preserves (13-soundness.md section 1:
    reaching Invalid, a missing switch arm, an undef denotation, or
    a stuck or wild state).  Beh_undef covers undef_next OR stuck
    (OS.Unit.Final's encoding in Opsem.v), so excluding it excludes
    both. *)
Definition no_ub (B : behavior -> Prop) : Prop :=
  forall tr, ~ B (Beh_undef tr).

(** ** One-hole expression contexts -- 13-soundness.md section 2

    INV.Rewrite.Local quantifies over "every context C[.]": an
    arbitrary program position at which a rewrite may fire, i.e. a
    term of the mutual expr block with one expr subterm removed.
    The expr positions are exactly those of Syntax.v's mutual block
    (its header comment): let bodies, code bodies inside a
    static-consts binding, continuation-handler bodies (nonrec and
    rec), and let-cont bodies.  Apply, apply_cont, switch and
    invalid contain no expr subterm, so they contribute no frame.
    This is a rewrite-position context, not an evaluation context
    (contrast Cmm.v's cm_ectx, which fixes evaluation order); it is
    transcription of the doc's quantifier over modeled syntax, per
    the sanctioned ch. 13 split. *)

(** Record-update helpers: the carried code0 / cont_handler supplies
    every field EXCEPT its body, which [ctx_plug] replaces (the
    original body field is dead).  code0_with_body is Simplify.v's
    (imported; a local twin lived here until Simplify.v landed). *)
Definition ch_with_body (h : cont_handler) (e : expr)
  : cont_handler :=
  match h with
  | Mk_cont_handler ps _ exn cold => Mk_cont_handler ps e exn cold
  end.

(** Outermost frame first: [Ctx_let_body p d C] is
    [E_let p d C[.]]. *)
Inductive expr_ctx : Type :=
  Ctx_hole
| Ctx_let_body (p : bound_pattern) (d : named) (C : expr_ctx)
| Ctx_let_code (p : bound_pattern)
    (pre : list static_const_or_code)
    (c : code0) (C : expr_ctx)
    (post : list static_const_or_code)
    (body : expr)
| Ctx_let_cont_nonrec_body (k : continuation) (h : cont_handler)
    (C : expr_ctx)
| Ctx_let_cont_nonrec_handler (k : continuation) (h : cont_handler)
    (C : expr_ctx) (body : expr)
| Ctx_let_cont_rec_body (ips : list (variable * kind_ws))
    (hs : list (continuation * cont_handler)) (C : expr_ctx)
| Ctx_let_cont_rec_handler (ips : list (variable * kind_ws))
    (pre : list (continuation * cont_handler))
    (k : continuation) (h : cont_handler) (C : expr_ctx)
    (post : list (continuation * cont_handler))
    (body : expr).

Fixpoint ctx_plug (C : expr_ctx) (e : expr) : expr :=
  match C with
  | Ctx_hole => e
  | Ctx_let_body p d C1 => E_let p d (ctx_plug C1 e)
  | Ctx_let_code p pre c C1 post body =>
      E_let p
        (N_static_consts
           (pre ++ SCC_code (code0_with_body c (ctx_plug C1 e))
                :: post))
        body
  | Ctx_let_cont_nonrec_body k h C1 =>
      E_let_cont_nonrec k h (ctx_plug C1 e)
  | Ctx_let_cont_nonrec_handler k h C1 body =>
      E_let_cont_nonrec k (ch_with_body h (ctx_plug C1 e)) body
  | Ctx_let_cont_rec_body ips hs C1 =>
      E_let_cont_rec ips hs (ctx_plug C1 e)
  | Ctx_let_cont_rec_handler ips pre k h C1 post body =>
      E_let_cont_rec ips
        (pre ++ (k, ch_with_body h (ctx_plug C1 e)) :: post) body
  end.

(* ================================================================== *)
(* Ch. 13 statements: the headline theorem, the local obligation,     *)
(* and the name-mode invariant (increment 2b)                         *)
(* ================================================================== *)

(** RULE INV.Simplify.Preserves (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/simplify.ml#run
    CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0

    The single property the whole formalism exists to make precise.
    Doc premises, in order: (1) Gamma |- U ok (chapters 02, 03) --
    modeled as expr_wf on the unit body plus, per main's ruling
    (Knuth's W-23), the code-tying hypothesis that every code item
    the pass's table binds is code0_wf (the binder disciplines the
    doc assumes by alpha-fiat; WellFormed.v, consumed by name);
    (2) Simplify(U) = U' -- modeled as simplify_unit (Simplify.v:
    the congruent closure of the rewrite union conjoined with the
    ch. 09 output contract); (3) U does not exhibit undefined
    behaviour -- modeled as no_ub, quantified per starting state
    ("from every starting heap" is the conclusion's quantifier and
    scopes over this hypothesis too: the license is per-heap).
    Conclusion: obs_refines of the two units' behavior sets at the
    unit's module symbol, from the same rho_pre / H0 -- U'
    observationally REFINES U.  REVISED (13 s4 item 8, resolution
    2026-07-22; entry 75): previously two-sided obs_equiv against
    the deterministic PhysEqual denotation; the pair was jointly
    refutable by a UB-free program (item 8's witness), and
    refinement up to immutable-identity folding is the correct
    statement for the language's actual license.

    ENCODING NOTE: two premises with no doc counterpart, both
    abstraction-soundness ties the doc leaves implicit:
    - consistent E0 rho_pre H0 (Concretization.v): the initial
      typing environment soundly describes the ambient runtime.
      Without it the conjecture is false outright (an E0 claiming
      x : {1} over a runtime 0 licenses an arm prune that changes
      defined behavior).
    - C carried by the starting heap: every code id the table
      binds is pre-installed in H0 with the same code item.  This
      covers IMPORTED code (the table run builds before
      traversal).  Unit-internal code -- the versions the pass's
      dacc table tracks as the body's own SCC_code items are
      rewritten -- is dacc machinery outside the model, so those
      runs fall outside the stated envelope (HYBRID-flavored
      coverage note; cf. CORRESPONDENCE.md).  Simplify.v's
      ENCODING NOTE on the fixed E is the same under-
      approximation on the typing side: both narrow coverage on
      the sound side; neither weakens the claim on the runs
      covered.
    - The kind/arity tables (WellFormed.v Section WithKindTables)
      are abstracted parameters of expr_wf with no canonical
      instantiation in the development, so the statement
      quantifies them universally: result_kind_table,
      code_params_arity, code_result_arity.  (The fourth
      Variable, prim_arg_kinds, is pruned at Section close --
      the expr_wf block never reads it; cf. Knuth's W-21 in the
      DRAFT NOTE.)  At the ch. 05/06 rows this specializes to
      the doc's premise (1).  Recorded because the hypothesis is
      thereby satisfiable under non-canonical tables, widening
      the premise's reach -- benignly, since kind pathologies
      surface as stuck runs, which no_ub excludes; if a
      canonical table module lands, specialize. *)
Theorem INV_Simplify_Preserves :
  forall (fl : eff_flags) (C : code_env) (E0 : tenv)
         (rkt : prim_op -> prim_result_kind)
         (cpa cra : code_id -> option arity)
         (Gamma : kctx) (Delta : cctx) (U U' : flambda_unit),
    expr_wf rkt cpa cra Gamma Delta (fu_body U) ->
    (forall cid c0, C cid = Some c0 -> code0_wf c0) ->
    simplify_unit fl C E0 U U' ->
    forall (rho_pre : env) (H0 : heap),
      consistent E0 rho_pre H0 ->
      (forall cid c0,
          C cid = Some c0 ->
          heap_get_code H0 cid = Some (HO_Code c0)) ->
      no_ub (fl_unit_behavior fl U rho_pre H0) ->
      obs_refines (fu_module_symbol U) H0
        (fl_unit_behavior fl U rho_pre H0)
        (fl_unit_behavior fl U' rho_pre H0).
Admitted.

(** RULE INV.Rewrite.Local (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects

    The per-rewrite obligation the headline theorem factors
    through.  Doc premises, in order: the rewrite E |- e ~> e'
    ("a rule in S.Rewrite.*, S.Inline.*, S.Unbox.*" -- modeled as
    the rewrites union, Simplify.v, whose constructors carry each
    rule's side conditions); E a sound abstraction of the runtime
    state (consistent E rho H); C[e] well-formed (expr_wf on the
    plugged term; contexts are the expr_ctx inventory above,
    mirrored one-for-one by Simplify.v's congruence frames).

    ENCODING NOTEs:
    - REVISED (13 s4 item 8, resolution 2026-07-22; entry 75): the
      conclusion is obs_refines -- the plugged rewritten term
      REFINES the original -- exactly as for the headline rule; a
      rewrite may resolve identity looseness, so two-sided
      equivalence is not the doc's claim.
    - The doc takes E's soundness "at e" -- at the hole, mid-run.
      The modeled premise takes it at the machine state the
      config starts from (the top of C[.]).  States whose E-facts
      are established only inside C[.] (by tests the context
      performs) fall outside the stated envelope: the premise is
      unsatisfiable there, so coverage narrows and nothing is
      asserted.  Full fidelity needs the reachability
      instrumentation (dacc-side), unmodeled.
    - "C[e] is reachable" is dropped in favor of quantifying the
      machine state and premising consistency: the doc's
      reachability picks out states arising in real runs, which
      the model cannot name; quantifying all consistent states
      asserts the same license the doc's abstraction premise
      supplies.
    - no_ub on the plugged config is the chapter's modulo-UB
      license (13-soundness.md section 1.1), stated here so the
      obligation composes to INV.Simplify.Preserves: a rewrite
      may send an already-UB run anywhere, and beh_sim relates
      Beh_undef only to Beh_undef.
    - The kind/arity tables of expr_wf are quantified
      universally, exactly as in INV_Simplify_Preserves (see the
      note there).
    - The rewrites union is a strict subset of the doc's
      quantification (KF-030, FIDELITY.md; REWRITTEN at the item-8
      resolution -- entry 55, ruling (d)): it deliberately
      excludes S.Rewrite.CSE.Replace / S.Rewrite.CSE.Extend
      (composed whole by Simplify.v's cse_deep) and
      S.Loopify.SelfTailCall (composed by stc_deep), all e ~> e'
      rules under the chapter, on the STRUCTURAL ground alone,
      which stands and is item-8-independent: the CSE table is
      denv state, so those rules live in side judgments with the
      table an explicit argument (RewritesPrim.v) and cannot be
      union arms; SelfTailCall needs the loopify ambient indices
      (self continuation, entry depth).  The former SECOND ground
      -- that CSE.Replace's obligation here was FALSE (the item-8
      witness, e.g. Cx = let x = Make_block(0,Immutable)[a] in
      [.], establishing the table's equation in-context) -- is
      DISSOLVED: it refuted only the pre-resolution two-sided
      equivalence; under this rule's refinement reading it is
      true-shaped ({true} contained in {true, false}), so the
      local refinement obligation on rw_cse is now STATABLE --
      booked as a follow-up item (entry 55).  Until it lands, the
      factoring consequence stands: an induction proving
      INV_Simplify_Preserves gets no per-arm lemma from this
      obligation at the S_cse and simplify_code arms; those arms
      carry their composed obligations directly.  (The remaining
      Loopify rules, Body and RecursiveRecompute, are not
      e ~> e' rules, so their absence needs no defense.) *)
Theorem INV_Rewrite_Local :
  forall (fl : eff_flags) (C : code_env) (E : tenv) (e e' : expr),
    rewrites fl C E e e' ->
    forall (rkt : prim_op -> prim_result_kind)
           (cpa cra : code_id -> option arity)
           (Cx : expr_ctx) (Gamma : kctx) (Delta : cctx)
           (rho : env) (K : kenv) (H : heap) (T : trap_stack)
           (R : region_stack) (sym_mod : symbol),
      consistent E rho H ->
      expr_wf rkt cpa cra Gamma Delta (ctx_plug Cx e) ->
      no_ub (fl_has_behavior fl
               (mk_config (Ctl_expr (ctx_plug Cx e)) rho K H T R)) ->
      obs_refines sym_mod H
        (fl_has_behavior fl
           (mk_config (Ctl_expr (ctx_plug Cx e)) rho K H T R))
        (fl_has_behavior fl
           (mk_config (Ctl_expr (ctx_plug Cx e')) rho K H T R)).
Admitted.

(* The mode-carrying binders of a pattern (the let-binder
   inventory: singleton and set-of-closures patterns bind
   bound_vars; static patterns bind symbols and code ids, which
   carry no name mode). *)
Definition bp_bound_vars (p : bound_pattern) : list bound_var :=
  match p with
  | BPat_singleton bv => [bv]
  | BPat_set_of_closures bvs => bvs
  | BPat_static _ => []
  end.

(** RULE INV.NameMode.Coherent (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let

    In U', every name occurring in a term position is bound at a
    mode with can_be_in_terms (Normal); Phantom-demoted names
    (S.Rewrite.Let.Phantom) occur only in debugging information;
    In_types names never occur in terms.

    ENCODING NOTEs:
    - The modeled syntax has no debugging-information positions,
      so every occurrence is a term-position occurrence and the
      Phantom clause sharpens to "does not occur free in its
      scope at all" (free_vars, the sanctioned binder Parameter).
    - Stated over every let binder of the output body, reached
      through the expr_ctx inventory above (every expr position
      of the mutual block is some ctx_plug); the binder's scope
      is the let body.  mode <> Normal covers Phantom and
      In_types in one clause (WF.Syntax.NameModeInTerms already
      excludes In_types binders from well-formed terms).
    - The unit's free names (parameters, imports) are bound
      outside the unit and carry no modeled binding mode; the doc
      treats them as Normal by convention. *)
Theorem INV_NameMode_Coherent :
  forall (fl : eff_flags) (C : code_env) (E0 : tenv)
         (U U' : flambda_unit),
    simplify_unit fl C E0 U U' ->
    forall (Cx : expr_ctx) (p : bound_pattern) (d : named)
           (body : expr) (bv : bound_var),
      fu_body U' = ctx_plug Cx (E_let p d body) ->
      In bv (bp_bound_vars p) ->
      bv_name_mode bv <> NM_normal ->
      ~ free_vars body (bv_var bv).
Admitted.

(* ================================================================== *)
(* Ch. 13 statements: anchors (increment 2a)                          *)
(*                                                                    *)
(* The eight rules below are anchored, not modeled, under the         *)
(* sanctioned traceability exception (CORRESPONDENCE.md): each        *)
(* quantifies irreducibly over unmodeled pass internals (dacc,        *)
(* flow_acc, required_names, slot_offsets, cmx) whose honest          *)
(* paraphrase would have to invent the flow analyses wholesale.       *)
(* True CLAIM preserved in each header; full text in                  *)
(* 13-soundness.md.                                                   *)
(* ================================================================== *)

(** RULE INV.KindChecks.Gated (CLAIM descriptive) -- 13-soundness.md
    CODE middle_end/flambda2/ui/flambda_features.ml#kind_checks
    CODE driver/oxcaml_args.ml
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_apply_shared

    The ch. 03 kind/arity checks (WF.Check.Gated) are OFF by default
    (flambda_features.kind_checks = false); a mismatch WF.* would
    reject is not a fatal error at simplify time but becomes Invalid
    (S.Rewrite.Apply.Invalid, ...), covered by the modulo-UB clause
    of INV.Simplify.Preserves.  A flag-behavior claim about the
    driver: documented, not modeled. *)
Definition INV_KindChecks_Gated_documented : Prop := True.

(** RULE INV.Simplify.EffectfulDeletionInventory (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/terms/flambda_primitive.ml#is_end_region
    CODE middle_end/flambda2/simplify/named_rewrite.mli#Prim_rewrite
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#make_result

    If a reachable primitive application with Arbitrary_effects is
    absent after a whole Simplify run, it is one of exactly two
    shapes, each licensed by a whole-body flow analysis: (a)
    End_region -- never End_try_region -- on a region not in
    required_names (S.Rewrite.Let.DeadRegion), or (b) a Block_set
    deleted/invalidated by mutable unboxing's escape analysis.  Every
    other deletion path requires at-most-generative effects.
    Anchored: the inventory quantifies over rebuild_let's gates and
    flow_acc's use accounting (unmodeled).  Scope: primitives only;
    extcalls and trap push/pop are separately licensed. *)
Definition INV_Simplify_EffectfulDeletionInventory_documented
  : Prop := True.

(** RULE INV.Simplify.RegionPairAtomic (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/flow/flow_acc.ml#record_let_binding
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/terms/flambda_primitive.ml#is_end_region
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region

    For let rho = Begin_region with End_region rho at each of the
    body's exits (ordinary regions, not try-regions): after one
    pass, either the region is required and Begin plus ALL Ends
    survive, or it is not and ALL Ends are deleted, whereupon Begin
    follows -- never a mixed outcome.  The deliberate coinductive
    cut: flow_acc does not count an End's use of rho as a use.
    Anchored: the dichotomy is stated on required_names and the
    flow_acc use-skip (unmodeled). *)
Definition INV_Simplify_RegionPairAtomic_documented : Prop := True.

(** RULE INV.Simplify.RequiredNamesSound (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
    CODE middle_end/flambda2/simplify/flow/dominator_graph.ml#create
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#filter_and_choose_alias
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#find_cse_simple

    The rebuilt term's free variables at normal name mode are
    contained in required_names R (exn-bucket first params excepted),
    split into downwards completeness (every input occurrence is
    recorded in flow_acc, unconditionally or conditioned on an
    R-surviving name) and upwards stability (every upwards
    occurrence-introducer -- an inventory of exactly FIVE -- draws
    only from names already in R).  Precondition making two runtime
    fatals unreachable.  Anchored: quantifies over flow_acc edges,
    the dominator graph, and the introducer inventory (unmodeled). *)
Definition INV_Simplify_RequiredNamesSound_documented : Prop := True.

(** RULE INV.Simplify.DeadCodeBodyLocal (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#record_free_names_of_apply_as_used
    CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
    CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze

    Liveness-based deletion never crosses a surviving (non-inlined)
    Apply in either direction: parameters of a Code binding are
    never deleted/reordered; an argument computation is kept even if
    the callee ignores it; a value feeding the callee's return is
    kept even if every caller discards it.  Exactly three cross-body
    channels influence liveness: inlining, used_value_slots
    (whole-unit), and reachable_code_ids at toplevel.  Anchored:
    stated on per-function-body flow domains and the Apply-use
    recording (unmodeled); type-driven cross-body rewrites are
    expressly distinguished and unaffected. *)
Definition INV_Simplify_DeadCodeBodyLocal_documented : Prop := True.

(** RULE INV.Simplify.LiftedConstGranularity (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/flow/flow_acc.ml#normalize_lifted_constant_aux
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#keep_lifted_constant_only_if_used
    CODE middle_end/flambda2/simplify/expr_builder.ml#create_let_symbol0
    CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#reachable_code_ids

    Dead-code elimination on a lifted-constant group operates at
    exactly two granularities: SYMBOLS all-or-nothing per group
    (being_defined makes any reachable member require every
    sibling), and CODE per-definition (code ids excluded from
    being_defined; unreachable ones emitted as Deleted_code within a
    kept group).  Referencing one symbol retains every sibling, the
    closure blocks, and all slot code; only version-chain-only code
    dies in practice.  Anchored: the two granularities live in the
    agreement of three files' internal accumulators (unmodeled). *)
Definition INV_Simplify_LiftedConstGranularity_documented
  : Prop := True.

(** RULE INV.Simplify.DeadValueSlotCoherence (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot
    CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
    CODE middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#value_slot_is_used
    CODE middle_end/flambda2/cmx/exported_code.ml#prepare_for_export

    U = the whole-unit used_value_slots accumulator; a defined,
    non-imported slot outside U is pruned coherently by FIVE
    consumers (flow-graph capture edges; static-set term dropping;
    slot_offsets layout; cmx exported types; exported code bodies),
    making dead-capture elimination whole-unit and non-local.  The
    Simplify SIDE of a mandatory merge with INV.ToCmm.SlotLiveness
    (INV_ToCmm_SlotLiveness_documented, ToCmmSoundness.v; ch. 20,
    Milner's side) -- two faces of ONE pruning event; the
    doc records the SURVIVAL implies RECORDED alignment as
    accidental, with a Cinvalid-at-runtime consequence if it breaks.
    Anchored: quantifies over dacc/slot_offsets/cmx accumulators
    (unmodeled).  This anchor is also why obs_equiv treats Closures
    objects as leaves (ENCODING NOTE above). *)
Definition INV_Simplify_DeadValueSlotCoherence_documented
  : Prop := True.

(* Same alias class in E, read through the derived canonical map
   (TypeGrammar.v's canonical over te_canonical); no new
   interface. *)
Definition same_class (E : tenv) (s1 s2 : simple) : Prop :=
  canonical E s1 = canonical E s2.

(** RULE INV.Simplify.AliasesMonotoneDown (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/types/env/aliases.mli#add
    CODE middle_end/flambda2/types/env/aliases.ml#add
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env
    CODE middle_end/flambda2/types/env/binding_time.ml#consts

    HYBRID (CORRESPONDENCE.md hybrid-anchor variant): the
    alias-coarsening half is stated below as a real conjecture;
    the lineage / binding-time framing is documented here.

    Doc premise: E0 -> E1 -> ... -> En is a straight-line descent
    lineage, each step one of the S.Struct.EnvRefineOnly
    operations (definitions, add_equation / add_env_extension,
    code-age extension), excluding program-point switches (join
    handler entry, recorded use envs).  Modeled premise:
    tenv_descends E E' (MeetJoin.v) -- the reflexive-transitive
    closure of tenv_define (fresh definition) and tenv_extends
    (operational replay of a recorded extension: each alias
    equation merges the class of its name into its target's class
    with the earlier-binding-time canonical, per aliases.ml#add;
    non-alias types meet into te_types).  The operational
    definition is load-bearing: were tenv_extends DEFINED by
    "classes only merge", the conjecture below would be a vacuous
    corollary of its own definition rather than a property of the
    documented operations (main's approval records this
    rationale).

    Doc conclusion beyond the stated half, documented not stated:
    no class member is ever removed (the Aliases API has no
    removal operation); each class's canonical moves only toward
    earlier binding times, constants (binding time 0) absorbing
    (T.Env.ConstCanonicalPersists); hence prover answers derived
    purely from aliasing have stable proved-ness under further
    descent, with witnesses monotonically upgrading toward
    earlier binding times -- proved-ness-stable, NOT
    answer-stable (Proved(var) can become Proved(sym) or
    Proved(c) within the same class).  Scope refinement from the
    doc NOTES: variable witnesses additionally degrade Proved ->
    Unknown at closure entry, because DE.enter_set_of_closures
    bumps min_binding_time (typing_env.ml:1037-38) and the
    T.Prove.SimpleModeBoundary mode floor filters the outer
    variables.  This is the semantically-monotone core of the
    refuted S.Struct.TypesMonotoneDown: the concrete-type half is
    only algorithmically monotone (its gamma can grow,
    S.Struct.EnvRefineOnly (b)); the alias half is genuinely
    gamma-monotone (each merge only adds equality
    constraints). *)
Theorem INV_Simplify_AliasesMonotoneDown :
  forall (E E' : tenv) (s1 s2 : simple),
    tenv_descends E E' ->
    same_class E s1 s2 ->
    same_class E' s1 s2.
Admitted.

(** RULE INV.Loopify.TrapNeutral (CLAIM normative)
    -- 13-soundness.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_env.ml#add_continuation
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#compile_staticfail
    CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr

    For loopified code with self continuation k: the entry jump and
    every SelfTailCall redirect are trap-action-free at entry trap
    depth; EVERY apply_cont targeting k (including shortcut-
    retargeted ones, which keep their site's trap action) is
    trap-DEPTH-neutral; the lowered recursive Ccatch's every Cexit
    is depth-matched -- a Pop may occur, a Push never; and the
    decision itself is trap-sensitive -- a self call in a
    with-handler branch leaves the function loopifiable, while
    self calls inside a try body reject loopification.  Anchored:
    legs quantify over the loopify decision machinery, the CPS
    conversion's handler discipline, and Cmm lowering; leg (b)
    (depth-neutrality of jumps to k) is statable over the modeled
    machine's trap stack should a later wave want it. *)
Definition INV_Loopify_TrapNeutral_documented : Prop := True.
