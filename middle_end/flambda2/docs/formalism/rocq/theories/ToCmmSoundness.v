(* ToCmmSoundness.v -- ch. 20 (20-to-cmm-soundness.md): to_cmm
   preserves observable behaviour through the representation
   relation -- a forward simulation between the Flambda machine
   (Opsem.v/Machine.v) and the Cmm machine (Cmm.v/CmmMemory.v).
   All 12 rules of the chapter are STATUS conjectured.

   HEADER OBLIGATIONS, binding on this file's simulation statement
   (coordinator ruling at ToCmmData.v's let_prim_ext, and reviewer
   booking Knuth W-26):
   (1) The simulation is posed against the TC.Let.Subst-CLOSURE of
       ToCmmData.v's tc_expr_data, NOT the bare relation: the
       concrete ch. 18 closure emits bind-point Clet placements
       only, while real to_cmm outputs sink bindings to flush
       points.  Posed against the bare relation, real outputs would
       come out unrelated.
   (2) The closure's DEFINITION in this file is where the doc's
       validity side conditions (hoisting above a branch,
       Control_flow_point), drop-of-unused-pure, and flush
       placement itself must be carried -- or their absence
       explicitly disclosed at the definition site.

   Section map (doc section -> here):
     s1  related configurations       -> the event/trace
                                         correspondence + the
                                         initial-config premises
     s2  the simulation claim         -> INV.ToCmm.Simulates
     s3  end-to-end, Invalid,
         discharging invariants      -> INV.ToCmm.EndToEnd,
                                         INV.ToCmm.InvalidUnreached,
                                         and the nine discharging
                                         invariants (eight anchors +
                                         the AddrConfined hybrid)
     s4  validation summary           -> (prose; no rule blocks)
     s5  known discrepancies          -> cited where they narrow a
                                         statement

   Ch. 17's and ch. 18's vocabulary (rep_val, rep_heap, rep_observe,
   loc_map, symaddr_agree, tc_expr_data) is imported, never
   redeclared; likewise ch. 03/07/13's (expr_wf, consistent,
   simplify_unit, beh_sim, no_ub) for the end-to-end composition.
   Conventions: rocq/CORRESPONDENCE.md.
   Wave 6; owner: Milner. *)

(* String is imported BEFORE List so that List's [length] wins the
   name clash (the Representation.v lesson); [List.length] is written
   qualified throughout regardless. *)
From Stdlib Require Import ZArith Bool String List.
Import ListNotations.
Open Scope Z_scope.

From Flambda2 Require Import Base Syntax Values Opsem WellFormed
  TypeGrammar PrimMemoryA Concretization Simplify Soundness
  Cmm CmmMemory Machine Representation ToCmmControl ToCmmData.

(* ================================================================== *)
(* 1. Location maps as cumulative sub-bijections (KF-015)             *)
(* ================================================================== *)

(* The pointwise-extension order on location maps: L1 is a sub-map
   of L2.  This is the order TC_Prim_Sound's conclusion already uses
   ("the location map only grows"); here it also orders each
   observable event's sub-map below the behavior-wide map, the
   heterogeneous image of Soundness.v's sub_rel. *)
Definition sub_loc (L1 L2 : loc_map) : Prop :=
  forall a z, L1 a = Some z -> L2 a = Some z.

(* Injectivity: distinct Flambda addresses have distinct Cmm images.
   rep_heap constrains L pointwise; injectivity is the global
   "bijection onto its range" half of the doc's "the two heaps are
   related by a location map" and is what makes the module-image
   observation unambiguous (ch. 17 R.Observe, ch. 20 s1). *)
Definition loc_map_inj (L : loc_map) : Prop :=
  forall a1 a2 z, L a1 = Some z -> L a2 = Some z -> a1 = a2.

(* ================================================================== *)
(* 2. The per-event correspondence (ch. 20 s1/s2; R.Observe's ev_rel) *)
(* ================================================================== *)

(* Representation.v's rep_observe is parameterized over
   ev_rel : event -> cm_event -> Prop and its ENCODING NOTE says
   "ch. 20 instantiates ev_rel with its cumulative per-event
   correspondence (reviewer finding KF-015)".  This section is that
   correspondence.  It is the heterogeneous image of Soundness.v's
   event_sim/trace_sim_from (the KF-015 cumulative-seed shape):
   value_sim under a per-event sub-bijection becomes rep_val under a
   per-event sub-map L_ev of the behavior-wide L, and the [seen]
   accumulator carries the (Flambda value, Cmm value) pairs every
   earlier event left in the external world's hands -- external
   code can stash a pointer at one call and read through it at the
   next. *)

Section EventCorrespondence.

(* Symbol addresses of the Cmm program under observation
   (cp_symaddr P at use sites); rep_heap is keyed by them. *)
Variable SA : fmap cmm_symbol Z.

(* ENCODING NOTE (quantified hook; flagged to the coordinator): the
   Flambda event carries the callee VALUE (Opsem.v keys the cextern
   oracle by value) while the Cmm event carries the callee's linkage
   NAME (a string; Cmm.v keys cextern_c by it).  No rule in 15-20
   defines the value-to-name correspondence -- it lives in the
   calling convention the doc axiomatizes at CM.Extcall -- so this
   file quantifies over it, the same treatment as ch. 16's six open
   hooks (extcall_image in particular).  "The two sides consult the
   same axiomatized external oracle" (Representation.v's note on
   rep_observe) is exactly this hook's intended reading. *)
Variable callee_name : value -> string -> Prop.

(* One observable event, Flambda vs Cmm.  The call-time memories
   are related under the per-event sub-map L_ev (the call-time
   memory does not populate locations allocated later, so the
   behavior-wide L would be too strong); results are related under
   the full L: they enter the program and are observed at later
   points (and join [seen] for later events).
   ENCODING NOTE: there is no raising constructor.  The Cmm machine
   models only RETURNING externals (Cmm.v, CM.Extcall: "a raising
   external transfers to the current trap handler (not modeled; the
   rule covers the returning case)"), so Ev_ccall_raise has no Cmm
   event to correspond to; traces containing a raising external are
   outside this correspondence, a narrowing inherited from ch. 15,
   not introduced here.  Unlike Soundness.v's [retained], the
   callee contributes no retained pair: the Cmm side of the callee
   is a string, not a value; the Flambda callee (a predefined
   symbol's value) is covered by L's symbol clause
   (symaddr_agree). *)
Inductive rep_event (L : loc_map) (seen : list (value * cmm_value))
  : event -> cm_event -> Prop :=
| Rep_ev_return : forall L_ev vf f args ws Hc Mc rs ts,
    sub_loc L_ev L ->
    Forall (fun vw => rep_val L_ev (fst vw) (snd vw)) seen ->
    callee_name vf f ->
    Forall2 (rep_val L_ev) args ws ->
    rep_heap SA Hc L_ev Mc ->
    Forall2 (rep_val L) rs ts ->
    rep_event L seen (Ev_ccall_return vf args Hc rs)
      (CME_extern f ws Mc ts).

(* The value pairs an event pair leaves in the external world's
   hands: arguments and results, positionally paired.  The raise
   row is a totality fallback only (no rep_event constructor ever
   relates a raise). *)
Definition retained_pairs (ev : event) (cev : cm_event)
  : list (value * cmm_value) :=
  match ev, cev with
  | Ev_ccall_return _ args _ rs, CME_extern _ ws _ ts =>
      combine args ws ++ combine rs ts
  | Ev_ccall_raise _ args _ _, CME_extern _ ws _ _ =>
      combine args ws
  end.

(* Trace correspondence, accumulating the retained pairs: event j's
   sub-map must also relate everything carried by events 1..j-1
   (the cumulative-seed resolution of KF-015, as in Soundness.v's
   trace_sim_from). *)
Inductive rep_trace_from (L : loc_map)
  : list (value * cmm_value) -> list event -> list cm_event -> Prop :=
| RT_nil : forall seen, rep_trace_from L seen [] []
| RT_cons : forall seen ev cev tr tr_c,
    rep_event L seen ev cev ->
    rep_trace_from L (seen ++ retained_pairs ev cev) tr tr_c ->
    rep_trace_from L seen (ev :: tr) (cev :: tr_c).

Definition rep_trace (L : loc_map)
  : list event -> list cm_event -> Prop :=
  rep_trace_from L [].

(* Reactive divergence carries an infinite trace on both sides
   (Beh_react / CMO_reacts); the correspondence is the coinductive
   image of rep_trace_from, with the same cumulative seed (the
   stream_sim_from shape of Soundness.v). *)
CoInductive rep_stream_from (L : loc_map)
  : list (value * cmm_value) -> event_stream -> cm_event_stream
    -> Prop :=
| RS_cons : forall seen ev cev s s_c,
    rep_event L seen ev cev ->
    rep_stream_from L (seen ++ retained_pairs ev cev) s s_c ->
    rep_stream_from L seen (Es_cons ev s) (CMS_cons cev s_c).

Definition rep_stream (L : loc_map)
  : event_stream -> cm_event_stream -> Prop :=
  rep_stream_from L [].

(* The binary projection rep_observe's Forall2 can consume: an
   event pair related under SOME seed.  rep_trace is strictly
   stronger (it chains the seeds); the lemma below discharges the
   Forall2 form from it, so the simulation statement can assert
   rep_trace and still instantiate R.Observe literally. *)
Definition tocmm_ev_rel (L : loc_map) (ev : event) (cev : cm_event)
  : Prop :=
  exists seen, rep_event L seen ev cev.

Lemma rep_trace_from_forall2 :
  forall L seen tr tr_c,
    rep_trace_from L seen tr tr_c ->
    Forall2 (tocmm_ev_rel L) tr tr_c.
Proof.
  intros L seen tr tr_c HT.
  induction HT as [seen0 | seen0 ev cev tr0 tr_c0 Hev Hrest IH].
  - constructor.
  - constructor.
    + exists seen0. exact Hev.
    + exact IH.
Qed.

End EventCorrespondence.

(* ================================================================== *)
(* 3. The TC.Let.Subst closure of tc_expr_data (HEADER OBLIGATIONS)   *)
(* ================================================================== *)

(* ENCODING NOTE (the closure's design is catalog item 64; approved
   by the coordinator): the closure adds SINK moves only.  Of the
   doc's three flush-time reorderings, inlining a delayed binding at
   its use and dropping an unused pure binding are already inside
   the bare tc_expr_data (let_prim_ext's LPE_inline/LPE_drop rows);
   what the bare relation cannot express is a binding EMITTED later
   than its bind point, i.e. a Clet pushed down to a flush point.
   One move sinks a Clet past a STRAIGHT-LINE prefix (Csequence and
   Clet frames only) -- a sink can therefore never cross a branch,
   switch, handler entry or static-catch boundary, which is the
   structural image of the doc's validity side conditions (the
   To_cmm_env flush at Branching_point / entering a loop; hoisting
   above a branch is inexpressible here, and that is where the
   doc's garbage-value hazard lives).  Reviewer booking Knuth W-26
   is discharged at this definition site:
   (1) validity side conditions: carried structurally by sl_ctx, as
       above;
   (2) drop-of-unused-pure: NOT re-represented here -- it is
       LPE_drop inside tc_expr_data (ToCmmData.v); this closure
       adds no drops;
   (3) flush placement: IS this closure -- no other artifact
       represents it.
   The guards are fidelity, not caution: posed against an unguarded
   sink closure, the ch. 20 simulation would be trivially FALSE
   rather than conjectured (a sunk effectful or capture-prone
   binding changes observable behaviour), the same over-
   approximation failure mode as reviewer finding KF-035.  Reviewer
   finding KF-041 caught exactly this failure in an earlier revision
   of Sink_intro (one-directional guards); the fixed premises and
   the load-bearing witness live at the Sink_intro site below. *)

(* The occurrence predicates cmm_mentions and cmm_mentions_label are
   hosted in Cmm.v (coordinator placement ruling, superseding this
   file as the host): syntactic occurrence of a backend var / static
   label, binders included -- conservative over-approximations of
   free occurrence.  Blocking a move on a shadowed occurrence only
   narrows the closure, the safe direction. *)

(* Straight-line contexts: the only frames a sink may cross. *)
Inductive sl_ctx : Type :=
  SL_hole
| SL_seq (e : cmm_expr) (sl : sl_ctx)
| SL_let (x : backend_var) (d : cmm_expr) (sl : sl_ctx).

Fixpoint sl_fill (sl : sl_ctx) (b : cmm_expr) : cmm_expr :=
  match sl with
  | SL_hole => b
  | SL_seq e sl' => Csequence e (sl_fill sl' b)
  | SL_let x d sl' => Clet x d (sl_fill sl' b)
  end.

(* Does the context bind x on the path to the hole? *)
Fixpoint sl_binds (sl : sl_ctx) (x : backend_var) : bool :=
  match sl with
  | SL_hole => false
  | SL_seq _ sl' => sl_binds sl' x
  | SL_let y _ sl' => backend_var_eqb x y || sl_binds sl' x
  end.

(* Does x occur in the context's own expressions? *)
Fixpoint sl_mentions (sl : sl_ctx) (x : backend_var) : bool :=
  match sl with
  | SL_hole => false
  | SL_seq e sl' => cmm_mentions x e || sl_mentions sl' x
  | SL_let _ d sl' => cmm_mentions x d || sl_mentions sl' x
  end.

(* Does the static label l occur in the context's own expressions?
   sl_ctx frames bind no labels, so this single predicate covers the
   label channel in both directions (KF-041 quadrant iii). *)
Fixpoint sl_mentions_label (sl : sl_ctx) (l : static_label) : bool :=
  match sl with
  | SL_hole => false
  | SL_seq e sl' => cmm_mentions_label l e || sl_mentions_label sl' l
  | SL_let _ d sl' => cmm_mentions_label l d || sl_mentions_label sl' l
  end.

(* The semantic commuting license (the doc's effects/coeffects
   quadruple by its CONTENT, not its vocabulary): from EVERY machine
   state, d evaluates silently to a value, leaving memory, traps and
   regions unchanged -- and states agreeing on d's mentioned
   variables agree on the value, whatever their kenv, memory, trap
   or region state.  Totality over all states is what makes the
   license non-vacuous: an allocating, storing, raising, jumping or
   diverging d has no such run from some (indeed any) state, so it
   is excluded positively, never by a vacuously-true implication.
   A licensed d may also be safely DISCARDED on a prefix that exits
   or raises before the sunk binding is reached: its evaluation was
   silent and state-preserving, so not running it is unobservable.
   Kernel-shaped, as TC_Let_Subst (ToCmmData.v, catalog 62): the
   sound-side direction, pinning M, TT, RR across the run. *)
Definition sink_license (P : cmm_program) (d : cmm_expr) : Prop :=
  (forall ce chi M TT RR,
     exists w ce1 chi1,
       cmem_run P (CmCfg d ce chi M TT RR) []
                  (CmCfg (Cval w) ce1 chi1 M TT RR)) /\
  (forall ce ce' chi chi' M M' TT TT' RR RR'
          w w' ce1 chi1 ce2 chi2,
     (forall y, cmm_mentions y d = true -> ce' y = ce y) ->
     cmem_run P (CmCfg d ce chi M TT RR) []
                (CmCfg (Cval w) ce1 chi1 M TT RR) ->
     cmem_run P (CmCfg d ce' chi' M' TT' RR') []
                (CmCfg (Cval w') ce2 chi2 M' TT' RR') ->
     w = w').

(* One sink move: a licensed binding crosses a straight-line prefix
   that neither uses nor rebinds its variable, and whose variable
   and label vocabularies are disjoint from the definition's in BOTH
   directions (reviewer finding KF-041).  The symmetric premises are
   load-bearing, not caution.  Witness (Knuth): with only the
   x-guards and the license, d = Clet y (Cconst_int 2) (Cconst_int 1)
   is licensed (total, silent, always 1), yet with
   sl = SL_let z (Cvar y) SL_hole and b = Cvar z, BEFORE the move
   d's internal binding y := 2 persists in the flat venv
   (CM.Head.Let) so z binds 2, while AFTER the move Cvar y reads the
   outer value -- the closure would relate behavior-distinct
   expressions and refute the Admitted simulation (the KF-037
   class).  cmm_mentions counts binders, so the variable premise
   closes all three variable quadrants at once: d's internal binders
   read by sl (the witness), binders internal to sl's frame
   expressions read by d, and plain capture of d's free variables by
   sl's binders (the old one-directional sl_captures check, now
   subsumed).  The label premise closes the chi channel: a licensed
   d's self-contained Ccatch persistently extends chi
   (CM.Catch.NonRec), which a prefix's bare Cexit could otherwise
   come to read across the move; a bare-Cexit d is already excluded
   by license totality. *)
Inductive sink_step (P : cmm_program) : cmm_expr -> cmm_expr -> Prop :=
| Sink_intro : forall x d sl b,
    sl_binds sl x = false ->
    sl_mentions sl x = false ->
    (forall y, cmm_mentions y d = true ->
       sl_mentions sl y = false /\ sl_binds sl y = false) ->
    (forall l, cmm_mentions_label l d = true ->
       sl_mentions_label sl l = false) ->
    sink_license P d ->
    sink_step P (Clet x d (sl_fill sl b)) (sl_fill sl (Clet x d b)).

(* Reflexive-transitive congruence closure: sink moves at any depth
   (a move wholly INSIDE a branch arm or handler body is fine; only
   the move itself cannot cross the boundary). *)

(* The nested Forall2 premises below make the induction-scheme
   generator recurse through Forall2, which needs its "All" scheme
   registered (warning register-all, promoted to error; Hopper's
   compile round). *)
Scheme All for Forall2.

Inductive sunk (P : cmm_program) : cmm_expr -> cmm_expr -> Prop :=
| Sunk_refl : forall e, sunk P e e
| Sunk_trans : forall e1 e2 e3,
    sunk P e1 e2 -> sunk P e2 e3 -> sunk P e1 e3
| Sunk_sink : forall e e', sink_step P e e' -> sunk P e e'
| Sunk_let : forall x d d' b b',
    sunk P d d' -> sunk P b b' ->
    sunk P (Clet x d b) (Clet x d' b')
| Sunk_seq : forall e1 e1' e2 e2',
    sunk P e1 e1' -> sunk P e2 e2' ->
    sunk P (Csequence e1 e2) (Csequence e1' e2')
| Sunk_tuple : forall es es',
    Forall2 (sunk P) es es' -> sunk P (Ctuple es) (Ctuple es')
| Sunk_op : forall op es es',
    Forall2 (sunk P) es es' -> sunk P (Cop op es) (Cop op es')
| Sunk_if : forall c c' t t' f f',
    sunk P c c' -> sunk P t t' -> sunk P f f' ->
    sunk P (Cifthenelse c t f) (Cifthenelse c' t' f')
| Sunk_switch : forall s s' idx cs cs',
    sunk P s s' -> Forall2 (sunk P) cs cs' ->
    sunk P (Cswitch s idx cs) (Cswitch s' idx cs')
| Sunk_catch : forall flag hs hs' b b',
    Forall2 (sunk_handler P) hs hs' -> sunk P b b' ->
    sunk P (Ccatch flag hs b) (Ccatch flag hs' b')
| Sunk_exit : forall lbl es es' tas,
    Forall2 (sunk P) es es' ->
    sunk P (Cexit lbl es tas) (Cexit lbl es' tas)

with sunk_handler (P : cmm_program)
  : cmm_static_handler -> cmm_static_handler -> Prop :=
| Sunk_shandler : forall lbl ps b b' cold,
    sunk P b b' ->
    sunk_handler P (SHandler lbl ps b cold) (SHandler lbl ps b' cold).

(* The judgment the ch. 20 simulation is posed against (HEADER
   OBLIGATION 1): tc_expr_data followed by sink moves.  The six
   arguments are ToCmmData.v's Section Variables in declaration
   order. *)
Definition tc_expr_data_sunk
    (lookup_code : code_id -> option code0)
    (classify_handler :
       continuation -> cont_handler -> expr -> cont_class -> Prop)
    (debug_flag : bool)
    (indirect_call_image :
       tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
    (indirect_full_call_image :
       tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
    (extcall_image :
       tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop)
    (P : cmm_program) (th : tcenv) (e : expr) (e_c : cmm_expr)
  : Prop :=
  exists e_c0,
    tc_expr_data lookup_code classify_handler debug_flag
      indirect_call_image indirect_full_call_image extcall_image
      th e e_c0 /\
    sunk P e_c0 e_c.

(* ================================================================== *)
(* 4. The simulation claim (ch. 20 s2)                                *)
(* ================================================================== *)

(* The behavior/outcome correspondence: one constructor per bullet
   of the rule's conclusion.  Each terminating/diverging bullet
   carries its own behavior-wide location map L extending the
   initial L0 (locations allocated during the run enter L; the
   per-event sub-maps of rep_trace sit below it), with rep_trace on
   the C-call traces -- the doc's "the SAME C-call effect trace...
   in every outcome except resource exhaustion", which is also why
   TBR_exhaust alone asserts nothing about traces.  The normal-
   termination bullet pairs rep_observe with rep_heap on the final
   states, exactly as Representation.v's R.Observe note anticipates
   ("ch. 20's statement does exactly that"): rep_heap is what makes
   the module-image observation transitive through pointer
   fields. *)
Inductive tocmm_beh_rel (SA : fmap cmm_symbol Z)
    (callee_name : value -> string -> Prop) (sym_mod : symbol)
    (L0 : loc_map) : behavior -> cm_outcome -> Prop :=
| TBR_normal : forall L tr tr_c H_fin M_fin,
    sub_loc L0 L ->
    loc_map_inj L ->
    rep_heap SA H_fin L M_fin ->
    rep_trace SA callee_name L tr tr_c ->
    rep_observe (tocmm_ev_rel SA callee_name L) SA sym_mod
      H_fin L M_fin tr tr_c ->
    tocmm_beh_rel SA callee_name sym_mod L0
      (Beh_return tr H_fin) (CMO_normal M_fin tr_c)
| TBR_uncaught : forall L tr tr_c v_exn w_exn H_fin,
    sub_loc L0 L ->
    loc_map_inj L ->
    rep_val L v_exn w_exn ->
    rep_trace SA callee_name L tr tr_c ->
    tocmm_beh_rel SA callee_name sym_mod L0
      (Beh_exn tr v_exn H_fin) (CMO_uncaught w_exn tr_c)
| TBR_diverge : forall L tr tr_c,
    sub_loc L0 L ->
    loc_map_inj L ->
    rep_trace SA callee_name L tr tr_c ->
    tocmm_beh_rel SA callee_name sym_mod L0
      (Beh_diverge tr) (CMO_diverges tr_c)
| TBR_react : forall L s s_c,
    sub_loc L0 L ->
    loc_map_inj L ->
    rep_stream SA callee_name L s s_c ->
    tocmm_beh_rel SA callee_name sym_mod L0
      (Beh_react s) (CMO_reacts s_c)
| TBR_exhaust : forall b,
    (forall tr, b <> Beh_undef tr) ->
    tocmm_beh_rel SA callee_name sym_mod L0
      b CMO_resource_exhaustion.

(* KF-040 carve-out (coordinator ruling at ToCmmData.v's ov_hole):
   the code's deferred update for a Var payload of a CUSTOM boxed
   number (int32/int64/nativeint/float32) stores at index 0, the ops
   word, where R.Obj puts the payload at +8 -- a compiler-bug
   candidate the model transcribes code-faithfully, so the
   simulation would be FALSE against R.Obj on a unit containing such
   a constant.  This predicate names exactly those constants; the
   theorem premise excludes them via Syntax.v's static_consts_in
   occurrence Parameter (the sanctioned nominal-occurrence set). *)
Definition custom_boxed_var_payload (sc : static_const) : Prop :=
  match sc with
  | SC_boxed_int32 (OV_var _) => True
  | SC_boxed_int64 (OV_var _) => True
  | SC_boxed_nativeint (OV_var _) => True
  | SC_boxed_float32 (OV_var _) => True
  | _ => False
  end.

(** RULE INV.ToCmm.Simulates (STATUS conjectured) -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr

    Posed against tc_expr_data_sunk, the TC.Let.Subst closure of the
    ch. 18 translation (HEADER OBLIGATIONS at the top of this file).

    The doc's setup line "initial(U) =cfg initc(P), with H0 = M0 on
    predefined symbols" is transcribed as follows.  initc(P) is the
    CmCfg in the conclusion: the module initialiser's body (the
    cp_funs entry at s_init, parameterless), empty venv and kenv,
    M0, an empty trap stack, and the Flambda run's own region stack
    (R = RR is equality, as in INV.ToCmm.Control; fl_unit_behavior's
    existential is unfolded into initial + fl_has_behavior precisely
    so the initial config's region stack can seed the Cmm side).
    The K = chi clause on the only two continuations initial(U)
    knows becomes the two tc_phi wiring premises: the unit's return
    continuation realized R_return (the function boundary -- these
    premises are load-bearing, not caution: left unwired, a th
    realizing the return continuation as a jump into the empty
    initial chi refutes the Admitted statement, the KF-037 class),
    and its exn continuation realized R_exn, whose raises emit a
    bare Craise (TC.ApplyCont.Raise) -- against the empty initial
    trap stack an uncaught raise IS the CMO_uncaught outcome.

    ENCODING NOTE (narrowing, disclosed per coordinator ruling): the
    correspondence covers RETURNING externals only.  rep_event has
    no raising constructor because the Cmm machine models only
    returning externals (Cmm.v, CM.Extcall), so runs whose trace
    contains a raising external are outside this statement -- a
    narrowing inherited from ch. 15, not introduced here.
    ENCODING NOTE (further statement shape, cataloged with main):
    - the modulo-UB clause is the (forall tr, b <> Beh_undef tr)
      premise, inherited verbatim from ch. 13 as the doc says; the
      resource-exhaustion bullet is TBR_exhaust, related to every
      non-undef behavior with NO trace obligation;
    - the doc's single divergence bullet is the union of the
      TBR_diverge and TBR_react constructor pairs (the
      Beh_diverge/Beh_react split, CORRESPONDENCE entry 11);
    - trap-depth asymmetry at the halt boundary: initial(U) places
      the halt exn entry on T while initc's trap stack must be
      empty (cm_uncaught_config requires it), so ~cfg's "T = TT
      same depth" gloss holds only above the halt boundary; flagged
      to the coordinator;
    - the six ch. 18 hooks and callee_name are universally
      quantified and the conjecture is read under the intended
      instantiation (the INV.ToCmm.Control precedent);
    - KF-040 carve-out (coordinator ruling): the premise on
      static_consts_in excludes units containing a custom-boxed
      Var-payload static constant, whose code-faithful lowering
      (ToCmmData.v, ov_hole) clobbers the ops word and falsifies
      the statement against R.Obj -- a compiler-bug candidate
      escalated in the final report, latent in production;
    - global-freshness premise (reviewer watch W-27, sanctioned):
      cmm_unique_binders (Cmm.v) pins the Barendregt condition on
      the translated body.  The colliding binder and label choices
      live existentially INSIDE the quantified tc derivation --
      not pinnable at the statement -- so only this output-term
      constraint blocks a hostile derivation from refuting the
      Admitted statement through the machine's flat environments
      (the KF-037 class); the same premise is the global discharge
      that ToCmmControl.v's file convention defers for its
      rule-minted atoms (x_exn, the KF-037 agreement premises).
      Intended instantiation: the code's create_local /
      next_raise_count freshness;
    - KNOWN VIOLATION (doc, s5.6): -Oclassic Box_number duplication
      plus the deterministic ch. 06 PhysEqual denotation falsifies
      the rule as stated for a UB-free program; the doc says to
      read it modulo immutable-block identity until fixed, and this
      statement transcribes the rule as written, violation and
      all -- it is Admitted, never used as an axiom. *)
Theorem INV_ToCmm_Simulates :
  forall (flags : eff_flags) (U : flambda_unit) (P : cmm_program)
         (lookup_code : code_id -> option code0)
         (classify_handler :
            continuation -> cont_handler -> expr -> cont_class -> Prop)
         (debug_flag : bool)
         (indirect_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (indirect_full_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (extcall_image :
            tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop)
         (callee_name : value -> string -> Prop)
         (th : tcenv) (s_init : cmm_symbol) (fd : cmm_fundecl)
         (ptys : list machtype) (lbl_exn : static_label)
         (rho_pre : env) (H0 : heap) (M0 : cmm_mem) (L0 : loc_map)
         (c0 : config) (b : behavior),
    cp_funs P s_init = Some fd ->
    fd_params fd = [] ->
    tc_expr_data_sunk lookup_code classify_handler debug_flag
      indirect_call_image indirect_full_call_image extcall_image
      P th (fu_body U) (fd_body fd) ->
    cmm_unique_binders (fd_body fd) ->
    tc_phi th (fu_return_continuation U) = Some (R_return ptys) ->
    tc_phi th (fu_exn_continuation U) = Some (R_exn lbl_exn) ->
    (forall sc, static_consts_in (fu_body U) sc ->
       ~ custom_boxed_var_payload sc) ->
    rep_heap (cp_symaddr P) H0 L0 M0 ->
    symaddr_agree P L0 ->
    loc_map_inj L0 ->
    initial U rho_pre H0 c0 ->
    fl_has_behavior flags c0 b ->
    (forall tr, b <> Beh_undef tr) ->
    exists o,
      cmem_unit_behaves P
        (CmCfg (fd_body fd) fempty kenv_empty M0 [] (c_R c0)) o /\
      tocmm_beh_rel (cp_symaddr P) callee_name (fu_module_symbol U)
        L0 b o.
Admitted.

(* ================================================================== *)
(* 5. End-to-end composition and Invalid (ch. 20 s3)                  *)
(* ================================================================== *)

(** RULE INV.ToCmm.EndToEnd (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
    CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit

    The composed correctness of the two formalized passes.  Doc
    premises, in order: (1) U0 a well-formed raw unit -- expr_wf
    plus the code-tying hypothesis, exactly INV_Simplify_Preserves's
    premise (1) (Soundness.v); (2) U' = Simplify(U0) --
    simplify_unit; (3) P = [[U']] -- the INV_ToCmm_Simulates premise
    block on U', verbatim (translation, wiring, freshness, KF-040
    carve-out, initial-state relations); (4) U0 has no undefined
    behaviour -- no_ub on U0's behavior set, plus membership of the
    behavior b0 under scrutiny.  ENCODING NOTE: the doc says the
    rule "INHERITS INV.Simplify.Preserves as a premise"; theorem
    statements cannot appear as hypotheses, so this statement
    inherits the premise SETS of both component theorems instead,
    each extra abstraction-soundness tie (consistent E0, the
    code-table/heap tie, rep_heap / symaddr_agree / loc_map_inj)
    riding with the component that introduced it.

    Conclusion: for the given behavior b0 of U0 there are a behavior
    b' of U' (drawn from the same rho_pre/H0; its initial config's
    region stack seeds the Cmm side, as in INV_ToCmm_Simulates) and
    a Cmm outcome o with b0 related to b' by beh_sim (ch. 13's
    per-behavior observation) and b' related to o by tocmm_beh_rel
    (ch. 20's) -- "P's observable behaviour refines U0's" as the
    transitive chain through U', which is how the doc's
    by-transitivity proof sketch reads.

    ENCODING NOTE (KNOWN COUNTEREXAMPLE, doc NOTES): the doc says
    the rule as stated is FALSE for any U0 containing a compile-time
    int->float32 conversion -- Simplify's constant fold
    double-rounds (13 s4.7) -- and to read it modulo s5.1 until the
    fold is fixed.  The "modulo ... the known int->float32
    constant-fold unsoundness" words in the rule text are that
    reading instruction, not a formalizable carve-out; this
    statement transcribes the rule as written, counterexample and
    all -- it is Admitted, never used as an axiom (the
    INV_ToCmm_Simulates KNOWN-VIOLATION precedent).  The gap is
    entirely in Simplify: the ch. 18 lowering single-rounds
    (TC.Prim.NumConv), which is the doc's s5.1 positive finding. *)
Theorem INV_ToCmm_EndToEnd :
  forall (flags : eff_flags) (C : code_env) (E0 : tenv)
         (rkt : prim_op -> prim_result_kind)
         (cpa cra : code_id -> option arity)
         (Gamma : kctx) (Delta : cctx)
         (U0 U' : flambda_unit) (P : cmm_program)
         (lookup_code : code_id -> option code0)
         (classify_handler :
            continuation -> cont_handler -> expr -> cont_class -> Prop)
         (debug_flag : bool)
         (indirect_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (indirect_full_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (extcall_image :
            tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop)
         (callee_name : value -> string -> Prop)
         (th : tcenv) (s_init : cmm_symbol) (fd : cmm_fundecl)
         (ptys : list machtype) (lbl_exn : static_label)
         (rho_pre : env) (H0 : heap) (M0 : cmm_mem) (L0 : loc_map)
         (b0 : behavior),
    expr_wf rkt cpa cra Gamma Delta (fu_body U0) ->
    (forall cid cd, C cid = Some cd -> code0_wf cd) ->
    simplify_unit flags C E0 U0 U' ->
    consistent E0 rho_pre H0 ->
    (forall cid cd,
        C cid = Some cd ->
        heap_get_code H0 cid = Some (HO_Code cd)) ->
    cp_funs P s_init = Some fd ->
    fd_params fd = [] ->
    tc_expr_data_sunk lookup_code classify_handler debug_flag
      indirect_call_image indirect_full_call_image extcall_image
      P th (fu_body U') (fd_body fd) ->
    cmm_unique_binders (fd_body fd) ->
    tc_phi th (fu_return_continuation U') = Some (R_return ptys) ->
    tc_phi th (fu_exn_continuation U') = Some (R_exn lbl_exn) ->
    (forall sc, static_consts_in (fu_body U') sc ->
       ~ custom_boxed_var_payload sc) ->
    rep_heap (cp_symaddr P) H0 L0 M0 ->
    symaddr_agree P L0 ->
    loc_map_inj L0 ->
    no_ub (fl_unit_behavior flags U0 rho_pre H0) ->
    fl_unit_behavior flags U0 rho_pre H0 b0 ->
    exists c0' b' o,
      initial U' rho_pre H0 c0' /\
      fl_has_behavior flags c0' b' /\
      beh_sim (fu_module_symbol U0) H0 b0 b' /\
      cmem_unit_behaves P
        (CmCfg (fd_body fd) fempty kenv_empty M0 [] (c_R c0')) o /\
      tocmm_beh_rel (cp_symaddr P) callee_name (fu_module_symbol U')
        L0 b' o.
Admitted.

(** RULE INV.ToCmm.InvalidUnreached (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
    CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#nullary_primitive
    CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
    CODE backend/cmm.mli#Cinvalid

    Doc premises, in order: (1) the Flambda side never steps a
    reachable config to OS.Invalid -- modeled as no_ub on the unit's
    behavior set (Opsem.v's Beh_undef covers reaching Invalid, an
    undef denotation, and stuck states, so excluding it excludes
    them all); (2) slot-liveness -- ENCODING NOTE: no model
    counterpart.  Of the four Cinvalid site-classes the doc
    conclusion discharges, only (a) the Flambda Invalid expression
    (ToCmmControl.v's TC_Invalid row) and (b) the unreachable switch
    case (TC_Switch's synthesized Cinvalid cases) exist in the
    mechanized translation; (c) the nullary Invalid primitive and
    (d) the dead closure-slot projection arms are not modeled (the
    ch. 18 projection images carry no Dead arms), so the premise
    that excludes (d) -- INV.ToCmm.SlotLiveness, a documented anchor
    below -- is not needed by this statement and its absence is
    disclosed here instead.  The remaining premises are the
    INV_ToCmm_Simulates setup block, which is what the doc's
    "the =cfg-related Cmm configuration" imports from s1/s2.

    Conclusion: the Cmm run never exhibits CMO_undef.  ENCODING
    NOTE (mild strengthening, disclosed): the Cmm machine does not
    separate reaching-Cinvalid from other stuck states --
    CM.Unit.Final's undef outcome is "reaching Cinvalid or a stuck
    read/store" (Cmm.v), and CM.Invalid makes Cinvalid
    transition-free -- so the mechanized conclusion asserts the
    union: no reachable Cinvalid AND no other stuckness.  The doc's
    rule is the Cinvalid half; the remainder is the simulation's own
    non-stuckness, which the doc's conclusion already presupposes
    (INV.ToCmm.Simulates lists only non-stuck outcomes). *)
Theorem INV_ToCmm_InvalidUnreached :
  forall (flags : eff_flags) (U : flambda_unit) (P : cmm_program)
         (lookup_code : code_id -> option code0)
         (classify_handler :
            continuation -> cont_handler -> expr -> cont_class -> Prop)
         (debug_flag : bool)
         (indirect_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (indirect_full_call_image :
            tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop)
         (extcall_image :
            tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop)
         (th : tcenv) (s_init : cmm_symbol) (fd : cmm_fundecl)
         (ptys : list machtype) (lbl_exn : static_label)
         (rho_pre : env) (H0 : heap) (M0 : cmm_mem) (L0 : loc_map)
         (c0 : config),
    no_ub (fl_unit_behavior flags U rho_pre H0) ->
    cp_funs P s_init = Some fd ->
    fd_params fd = [] ->
    tc_expr_data_sunk lookup_code classify_handler debug_flag
      indirect_call_image indirect_full_call_image extcall_image
      P th (fu_body U) (fd_body fd) ->
    cmm_unique_binders (fd_body fd) ->
    tc_phi th (fu_return_continuation U) = Some (R_return ptys) ->
    tc_phi th (fu_exn_continuation U) = Some (R_exn lbl_exn) ->
    (forall sc, static_consts_in (fu_body U) sc ->
       ~ custom_boxed_var_payload sc) ->
    rep_heap (cp_symaddr P) H0 L0 M0 ->
    symaddr_agree P L0 ->
    loc_map_inj L0 ->
    initial U rho_pre H0 c0 ->
    ~ cmem_unit_behaves P
        (CmCfg (fd_body fd) fempty kenv_empty M0 [] (c_R c0))
        CMO_undef.
Admitted.

(* ================================================================== *)
(* 6. The discharging invariants (ch. 20 s3): eight documented        *)
(*    anchors and the AddrConfined hybrid                             *)
(* ================================================================== *)

(* Catalog 37's decision rule (CORRESPONDENCE.md): a conjectured
   property whose quantification is irreducibly over unmodeled pass
   internals becomes a documented anchor -- full rule-text citation,
   _documented := True, true STATUS preserved in the comment.  Eight
   of the nine discharging invariants quantify over exactly such
   internals (slot_offsets / dacc / cmx accumulators, to_cmm_env
   bookkeeping, the fatal-site inventory, linker state, and the
   axiomatized GC and calling conventions); AddrConfined is the
   hybrid exception, with one clause proved outright on its
   mechanized envelope. *)

(** RULE INV.ToCmm.SlotLiveness (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/flambda2.ml#build_run_result
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#finalize
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#mark_slot_as_removed
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot
    CODE middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots
    CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
    CODE middle_end/flambda2/cmx/exported_code.ml#prepare_for_export
    CODE middle_end/flambda2/types/env/cached_level.ml#remove_unused_value_slots_and_shortcut_aliases

    Under (P1) SURVIVAL implies RECORDED (every value slot projected
    by a reachable non-phantom binding of the final term is in the
    used_slots accumulator -- an ACCIDENTAL alignment, since
    unboxing-materialized projections never record and the upwards
    pass cannot) and (P2) EXPORTED-CODE SYNC (inlinable cmx bodies
    are a subset of the final term's code bindings): (a) TOTALITY --
    every slot lookup to_cmm performs has an exported_offsets entry,
    so the "Missing offset" fatal is unreachable -- needing also
    that slot lookups are reached only from normal-mode occurrences
    (the doc NOTES' at_normal_mode qualifier: INV.NameMode.Coherent
    plus the Phantom skip), the seam to the one mechanized
    invariant this rule composes with; (b) DEAD implies
    UNREACHABLE -- a Dead_function_slot / Dead_value_slot entry
    (site-class (d) of INV.ToCmm.InvalidUnreached) is never
    evaluated by any reachable configuration of any program linking
    U'.

    Documented anchor (catalog 37): the rule quantifies over THREE
    distinct value-slot liveness accumulators (build_run_result's
    used_slots, DA.add_use_of_value_slot, the barred upwards
    snapshot), Slot_offsets.finalize, and the five-subsystem cmx
    pruning event -- all pass internals outside the model; the
    mechanized translation carries no Dead arms at all (disclosed at
    INV_ToCmm_InvalidUnreached above).  MANDATORY MERGE face: this
    is the to_cmm side of ONE pruning event with
    INV.Simplify.DeadValueSlotCoherence
    (INV_Simplify_DeadValueSlotCoherence_documented, Soundness.v),
    whose anchor text names this rule as its other face.  Failure
    mode if (P1)'s alignment breaks: a reachable projection with a
    Dead offset entry, i.e. Cinvalid EXECUTED at runtime. *)
Definition INV_ToCmm_SlotLiveness_documented : Prop := True.

(** RULE INV.ToCmm.ClosureScanBoundary (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#update_set_for_slot
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#layout_aux
    CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
    CODE backend/cmm_helpers.ml#pack_closure_info

    Every emitted closure block has a SINGLE word boundary startenv
    with (i) scanned Word_val value slots at offsets >= startenv --
    under the subkind-soundness premise that an is_always_immediate
    capture really is an immediate at runtime, a ch. 07/13 typing
    fact where a wrong bit is silent GC corruption; (ii) nothing the
    GC may interpret below startenv; (iii) every function slot's
    closinfo word encoding startenv - off(f) against the SAME
    absolute startenv; (iv) a function slot at offset 0, so the
    closinfo word the GC unconditionally reads at field 1 yields
    that startenv.  Conclusion: the GC's scan set for a closure
    block is exactly its scanned value slots, discharging
    CM.Alloc.GC premise (ii) for closure blocks.

    Documented anchor (catalog 37): (i)+(ii) reduce to the
    three-zone ordering enforced by the greedy GLOBAL offset
    assignment (slots shared across sets, cross-unit slots arriving
    pre-assigned; the mutable-bound fatal in update_set_for_slot is
    the primary enforcement, layout_aux's asserts secondary) --
    slot_offsets internals outside the model -- and the consumer is
    the GC, which ch. 19 axiomatizes, so premise (ii) of
    CM.Alloc.GC is not a statement the model can discharge into.
    The per-set layout residue that IS modeled lives in ch. 17/18
    (R.Obj.Closures; ToCmmData.v's set-of-closures images).  The
    subkind-soundness premise is shared with
    INV.ToCmm.StaticUpdateBarrier's Immediate arm (anchored
    below). *)
Definition INV_ToCmm_ClosureScanBoundary_documented : Prop := True.

(** RULE INV.ToCmm.AddrConfined (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#machtype_of_kind
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
    CODE middle_end/flambda2/kinds/flambda_kind.ml#t
    CODE backend/cmm.mli#machtype_component

    In the translated Cmm, every Addr-typed value (a Cadda /
    field_address result) is an anonymous subexpression created and
    consumed inside the image of a single TC.Prim.* emission; no
    binder the translation creates carries machtype Addr: (i) every
    declared machtype comes from machtype_of_kind, total from
    Flambda kinds into {Val, Int, Float, Float32, Vec*} with NO
    typ_addr arm, because Flambda's kind grammar has no address
    kind; (ii) every Clet emitted by flush_delayed_lets binds the
    image of a Flambda-kinded binding whose ROOT operator yields
    that kind's non-Addr machtype.  Conclusion: no Addr value is
    let-bound, passed to a join point, or held live across an
    allocation point -- CM.Addr.NoSurvive holds for to_cmm output BY
    KIND PRESERVATION.  Scope: translated phrases only (the
    generic-functions machinery Clet-binds an Addr, send_function's
    cache_ptr -- the doc's deliberate counterexample at the
    boundary).

    HYBRID (catalog 37 variant): clause (i) has a mechanized
    envelope, proved outright below -- the development's one
    concrete machtype assignment, ToCmmData.v's
    machtype_of_kind_data, never yields MC_addr; Cmm.v's
    machtype_component grammar HAS an MC_addr constructor, so the
    exclusion is real, not vacuous.  Clause (ii) and the
    moved-emission argument (a moved image recomputes its interior
    Cadda from a Val base at the use site, which stays a GC root)
    quantify over flush_delayed_lets bookkeeping and the interior
    structure of every TC.Prim.* image -- documented here.  Ch. 16's
    machtype_of_kind is an OPEN hook (ToCmmControl.v Section
    Variable), so the envelope binds the data-chapter
    instantiation, the only one the development defines. *)
Definition INV_ToCmm_AddrConfined_documented : Prop := True.

(* Clause (i)'s envelope, stated on machtype_of_kind_data and proved
   outright (the envelope-Qed pattern): no Flambda kind's register
   classification contains the Addr component. *)
Lemma machtype_of_kind_data_no_addr :
  forall kw, ~ In MC_addr (machtype_of_kind_data kw).
Proof.
  intros kw Hin.
  unfold machtype_of_kind_data in Hin.
  destruct (ws_kind kw) as [| nn | |]; try destruct nn;
    simpl in Hin; intuition discriminate.
Qed.

(** RULE INV.ToCmm.EffectLinear (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr

    Along every Cmm execution path of [[U']], the sequence of
    ARBITRARY-EFFECTS operations evaluated equals that of the
    simulated Flambda path (the C-call / write subtrace preserved
    exactly), and coeffect-only operations keep their order relative
    to effects; per delayed binding, the occurrence-count table:
    zero occurrences + Arbitrary_effects emitted EXACTLY once at the
    next flush; zero + generative (any mutability) / pure /
    coeffect-only droppable (dead-read and dead-ALLOCATION
    elimination, sound because a never-referenced block's identity
    and contents are unobservable); one inlined at most once;
    More_than_one + Delay duplicated only under the
    P.Effects.DelayDuplicable license; More_than_one + Strict
    regular.  The conclusion's load-bearing edge is apply_expr's
    Case 3: the call ITSELF enters the delayed set, sound only
    because an Arbitrary_effects binding is never dropped (zero
    occurrences still emits exactly once at the next flush).
    Conclusion: the effect subtrace of
    INV.ToCmm.Simulates is preserved by the delayed-binding
    machinery -- the missing quantitative half of TC.Let.Subst.

    Documented anchor (catalog 37): the classification quantifies
    over to_cmm_env's delayed-binding bookkeeping and Simplify's
    upwards occurrence counts consumed without revalidation (the
    doc's non-local premise (P1), with the loop-counting and
    Not_found conventions) -- pass internals outside the model.  The
    model carries the rule's LICENSE half instead, as semantic side
    conditions where the doc has a quantitative table: LPE_drop /
    LPE_inline inside ToCmmData.v's let_prim_ext, and this file's
    sink_license (totality + agreement), so the trace-preservation
    conclusion is subsumed into INV_ToCmm_Simulates's proof
    obligation rather than separately statable.  The drop and
    duplicate licenses both need immutable-block identity to be
    unobservable -- exactly what the ch. 06 PhysEqual denotation
    withholds (the s5.6 KNOWN VIOLATION transcribed at
    INV_ToCmm_Simulates). *)
Definition INV_ToCmm_EffectLinear_documented : Prop := True.

(** RULE INV.ToCmm.CallConvCoherent (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/terms/code_metadata.ml#function_slot_size
    CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
    CODE backend/cmm_helpers.ml#curry_function_sym
    CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#create_function_slot

    For every function slot in every emitted closure block: (1) LIVE
    code -- slot size 2 iff (num_params <= 1 and not tupled), else
    3, agreeing even when the size was fixed by a DIFFERENT
    compilation unit because both sides derive from the same
    Code_metadata; the closinfo arity field equals the code's
    PARAMETER count (negated for Tupled, not the unarized count), so
    caml_applyN reaches exactly code(cid) on a full-arity indirect
    call, agreeing with the direct call TC.Apply.Call emits; (2)
    DELETED code -- the slot holds a NULL code pointer (0n) and a
    FABRICATED arity, safe iff no reachable configuration ever
    APPLIES a closure value pointing at it (projection, in-set
    moves, and GC scanning remain fine).  Conclusion: direct and
    indirect application agree per slot, and null-code slots are
    never applied.

    Documented anchor (catalog 37): non-local three ways --
    cross-unit (slot sizes re-derived per unit from cmx'd
    Code_metadata), cross-component (closinfo arity/startenv against
    the runtime's caml_applyN / caml_curryN / GC conventions, which
    the model axiomatizes: CM.Apply, and the ch. 16/18
    indirect-call images are OPEN hooks), and cross-pass (leg (2)'s
    discharge is Reaper's deletion criterion, which the scope ledger
    excludes).  The doc flags leg (2) as real and UNDISCHARGED
    anywhere in 15-20 -- a violation jumps to address 0; the anchor
    preserves that flag verbatim for the final report. *)
Definition INV_ToCmm_CallConvCoherent_documented : Prop := True.

(** RULE INV.ToCmm.StaticUpdateBarrier (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update
    CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#update_field
    CODE backend/cmm_helpers.ml#setfield

    Every deferred symbol-field update (the Or_variable holes of
    TC.Let.Static, filled at module-init time) is emitted as:
    Pointer kind -> setfield Root_initialization (the
    caml_initialize barrier), never a plain store; Immediate kind ->
    plain store, sound because a non-pointer cannot create a
    static-to-minor-heap edge, resting on the SAME
    is_always_immediate subkind premise as
    INV.ToCmm.ClosureScanBoundary (i); Naked_* kinds -> plain store,
    sound because the target field lies in a region the GC never
    scans -- and this classification is COMPLETE.  Conclusion: the
    GC's knowledge of static-to-heap pointers is exactly the set of
    updates routed through caml_initialize; CM.Alloc.GC premise (ii)
    on the STATIC segment, the static-data parallel of
    INV.ToCmm.ClosureScanBoundary.

    Documented anchor (catalog 37): the mechanized residue is
    ToCmmData.v's static_update, which carries exactly the rule's
    two SHAPES (the caml_initialize extcall and the plain
    initializing Cstore) but with the arm chosen existentially per
    hole, not keyed by the hole's kind (its KF-036 ENCODING NOTE:
    the hole's kind is not recorded there; R.Obj.* fixes layout per
    constant) -- so the kind-keyed COMPLETENESS claim, which is this
    rule's content, is not statable on it; and the conclusion
    discharges a premise of the axiomatized GC, as at
    INV_ToCmm_ClosureScanBoundary_documented.  The doc's warning
    stands: a violation manifests as a missed remembered-set entry,
    GC-scheduling-dependent corruption invisible to the byte-layout
    validation all tocmm-* case studies perform. *)
Definition INV_ToCmm_StaticUpdateBarrier_documented : Prop := True.

(** RULE INV.ToCmm.LoweringTotal (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#inline_variable
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#get_continuation
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#param_machtype_of_kinded_parameter

    On a unit that is (a) well-formed (WF.*, ch. 03), (b) name-mode
    coherent (INV.NameMode.Coherent), (c) trap-disciplined (ch. 04
    s1.7), and
    (d) equipped with total slot/code metadata, the translation is
    TOTAL: none of the ~109 Misc.fatal_error sites across
    to_cmm/*.ml is reachable, each fatal class discharged by a named
    upstream invariant (scoping + name modes; continuation scoping
    with Phi fixed at the binder; WF continuation arities; trap
    discipline + the exn first-param convention; SlotLiveness;
    CallConvCoherent leg (1); WF kinding) -- except class (e),
    to_cmm_env's own internal-machinery fatals, discharged by
    to_cmm's OWN construction, not an upstream premise.
    Conclusion: compilation of Simplify output cannot abort inside
    to_cmm -- the umbrella making INV.ToCmm.Simulates non-vacuous (a
    simulation presumes a translation EXISTS).

    Documented anchor (catalog 37): the quantification is over the
    fatal-site inventory of the OCaml implementation (census:
    exactly 109 sites across 9 files) and class (e)'s to_cmm_env
    bookkeeping, none of which has a model counterpart -- the
    mechanized translation relations are partial by construction (a
    fatal corresponds to no derivation), so the rule's content is an
    adequacy claim about the CODE, not a property of the relations.
    Two of its upstream premises are anchors themselves
    (SlotLiveness above, CallConvCoherent above);
    INV.NameMode.Coherent is stated in Soundness.v. *)
Definition INV_ToCmm_LoweringTotal_documented : Prop := True.

(** RULE INV.ToCmm.SymbolInitPlacement (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#place_symbol_inits
    CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_bindings
    CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update

    When an Or_variable hole is filled from a variable whose
    translation is a bare Cvar, the init store is REGISTERED against
    the variable (add_symbol_init) rather than emitted inline; then
    (a) EXACTLY-ONCE: the store is emitted exactly once -- at the
    variable's Clet during flush_bindings, or at the fundecl /
    Ccatch-handler entry when it is a parameter -- and never dropped
    even if the binding is dead (the is_empty-inits conjunct in the
    removal guard); (b) ORDER: the store precedes, in Cmm program
    order, every read of that static field (readers can only SINK
    relative to the binder, never hoist); (c) the field holds its
    Cdata placeholder until the store runs, with no representation
    obligation before OS.Unit.Final / CM.Unit.Final.  Conclusion:
    deferred symbol initialization is linear and correctly ordered
    across the delayed-binding machinery.

    Documented anchor (catalog 37): "machinery formalized nowhere
    else" (doc NOTES) -- the registration/flush bookkeeping is
    to_cmm_env internal state.  The model never takes the
    registration path at all: ToCmmData.v's TC.Let.Static image
    sequences its static_update stores at the binding site directly
    (seq_updates), so exactly-once and ordering hold there by
    construction, and the rule's content -- that the REAL placement
    machinery preserves them -- has no mechanized counterpart.  The
    doc's SOFT SPOT is preserved for the final report: leftover
    inits at a RECURSIVE handler boundary are eprintf'd and DROPPED,
    guarded only by the unchecked convention that symbols are bound
    at top level -- a latent hazard if lifting ever changes. *)
Definition INV_ToCmm_SymbolInitPlacement_documented : Prop := True.

(** RULE INV.ToCmm.SymbolLocality (STATUS conjectured)
    -- 20-to-cmm-soundness.md
    CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol
    CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol_of_code_id
    CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#raw_symbol
    CODE middle_end/flambda2/flambda2.ml#build_run_result

    A data/function symbol is emitted with Cmm.Local linkage iff it
    belongs to the current unit AND is absent from reachable_names;
    soundness requires reachable_names to OVER-approximate the names
    any other unit can reference.  (a) COHERENCE: locality is a
    deterministic function of (compilation unit, reachable_names),
    so the "declared as both local and global" fatal is unreachable
    from the symbol / symbol_of_code_id paths; (b) LINK SOUNDNESS:
    under-approximation yields an undefined-symbol link error in an
    importing unit; over-approximation costs only symbol-table size
    (discharged structurally: cmx-visible names are a subset of
    reachable_names by construction; -opaque collapses everything to
    Local, sound).

    Documented anchor (catalog 37): the consumer is the system
    linker and the quantification is over reachable_names / cmx
    contents; the doc itself says R.Observe has no notion of
    linkage, so "15-20 cannot even STATE this as they stand" -- and
    the model's cmm_program carries no linkage field either (Cmm.v
    models cp_funs / cp_symaddr only).  The THIRD Reaper-consumer
    invariant, with the dead-slot and null-code anchors above;
    invisible to both single-unit simulation statements, manifesting
    only when another unit links. *)
Definition INV_ToCmm_SymbolLocality_documented : Prop := True.
