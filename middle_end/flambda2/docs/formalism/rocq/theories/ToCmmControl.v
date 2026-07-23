(* ToCmmControl.v -- ch. 16 (16-to-cmm-control.md): to_cmm stage 1,
   the control-flow half of the Flambda->Cmm translation.

   Section map (doc section -> here):
     s1  judgment and environment    -> realization, tc_env, the hooks
     s2  continuation classification -> cont_class, tc_realized
     s3  continuation binders        -> tc_expr, TC_LetCont_ cases
     s4  continuation calls          -> tc_expr, TC_ApplyCont_ cases
     s5  applications                -> tc_apply_call, tc_apply_return
     s6  switches                    -> tc_switch_form, TC_Switch
     s7  Invalid                     -> TC_Invalid
     s8  control simulation lemma    -> ctrl_kenv / ctrl_traps,
                                        INV_ToCmm_Control

   The simple-translation judgment and the delayed-binding (flush)
   state belong to 18-to-cmm-data.md and enter as Section hook
   Variables, mirroring the doc's "owned by [18]" annotations;
   ToCmmData.v closes them.  The call images also enter as hooks,
   but no chapter states rules for them: the doc places the calling
   conventions at CM.Apply / CM.Extcall (ch. 15) and ch. 20 treats
   them as axiomatized there, so they stay quantified.

   Conventions: rocq/CORRESPONDENCE.md.  Wave 3; owner: Milner. *)

From Stdlib Require Import ZArith Bool List.
Import ListNotations.
Open Scope Z_scope.

From Flambda2 Require Import Base Syntax Cmm.
(* Increment 2 (s8): the Flambda machine side of the control
   simulation lemma.  PrimMemoryA supplies eff_flags, over which
   Machine's fl_step is Section-generalized. *)
From Flambda2 Require Import Values Opsem PrimMemoryA Machine CmmMemory.

(* NAME CLASH NOTE: Syntax.v and Cmm.v both define types named
   trap_action and raise_kind (Flambda side vs Cmm side).  Cmm is
   imported last among the two, so those names unqualified refer to
   Cmm's; this file writes Syntax.trap_action / Syntax.raise_kind
   explicitly for the Flambda side.  Constructor names do not clash.
   The s8 imports define neither name (Values' kenv is the Flambda
   continuation env; Cmm's is cmm_kenv), so the resolution stands. *)

(* ================================================================== *)
(* 1. Continuation realizations and the environment (16 s1)           *)
(* ================================================================== *)

(* realization ::= Return | Jump | Inline | Exn, the four ways a
   continuation is realized (To_cmm_env's continuation table).
   param_types are Cmm machtypes (via the machtype_of_kind hook
   below).  Inline keeps the Flambda params with their kinds (needed
   for the Rec_info skip of TC.ApplyCont.Inline), the handler
   expression, and the recorded occurrence count occ. *)
Inductive realization : Type :=
  | R_return (param_types : list machtype)
  | R_jump (param_types : list machtype) (lbl : static_label)
  | R_inline (params : list (variable * kind_ws)) (handler : expr)
             (occ : nat)
  | R_exn (lbl : static_label).

Definition realization_label (r : realization) : option static_label :=
  match r with
  | R_jump _ lbl => Some lbl
  | R_exn lbl => Some lbl
  | _ => None
  end.

(* Theta = <V, Phi, D>.  Phi is concrete here (the control rules read
   and extend it).
   ENCODING NOTE: the doc lists V (simple translation) and D (delayed
   bindings) as separate components, both owned by ch. 18; nothing in
   ch. 16 distinguishes them, so they are bundled opaquely as tc_data
   of an abstract type. *)
Record tc_env (D : Type) : Type := Mk_tc_env {
  tc_phi : fmap continuation realization;
  tc_data : D
}.
Arguments Mk_tc_env {D} _ _.
Arguments tc_phi {D} _.
Arguments tc_data {D} _.

Definition upd_phi {D : Type} (th : tc_env D) (k : continuation)
    (r : realization) : tc_env D :=
  Mk_tc_env (fupd continuation_eqb (tc_phi th) k r) (tc_data th).

Definition upd_phi_list {D : Type} (th : tc_env D)
    (kvs : list (continuation * realization)) : tc_env D :=
  Mk_tc_env (fupd_list continuation_eqb (tc_phi th) kvs) (tc_data th).

(* "lbl fresh": no continuation is already realized at lbl. *)
Definition phi_label_fresh {D : Type} (th : tc_env D)
    (lbl : static_label) : Prop :=
  forall k r, tc_phi th k = Some r -> realization_label r <> Some lbl.

(* The flush points of the delayed-binding machinery
   (to_cmm_env.mli).  Flushing itself (materializing D as Clets) is
   ch. 18's; the rules here use it only as a side condition, exactly
   as the doc writes "flush(Theta) at ...". *)
Inductive flush_mode : Type :=
  | FM_branching_point
  | FM_entering_loop
  | FM_flush_everything.

(* classify_continuation_handler's result (to_cmm_effects.ml). *)
Inductive cont_class : Type :=
  | CC_may_inline
  | CC_regular.

(* Which way a Let_cont binder is realized (s2/s3 dispatch). *)
Inductive realized_kind : Type :=
  | RZ_inline
  | RZ_jump
  | RZ_exn.

(* ================================================================== *)
(* 2. Target-side helpers (smart-constructor images)                  *)
(* ================================================================== *)

(* C.make_tuple: a single expression is itself; any other arity is a
   Ctuple (Ctuple [] = void), mirroring cm_bundle on values. *)
Definition tc_make_tuple (es : list cmm_expr) : cmm_expr :=
  match es with
  | [e] => e
  | _ => Ctuple es
  end.

(* Flambda raise kinds -> Cmm raise kinds, one-to-one. *)
Definition tc_raise_kind (rk : Syntax.raise_kind) : Cmm.raise_kind :=
  match rk with
  | RK_regular => Raise_regular
  | RK_reraise => Raise_reraise
  | RK_no_trace => Raise_notrace
  end.

(* A Pop trap action's raise kind is optional; an absent kind is a
   NOTRACE raise (Trap_action.Raise_kind.option_to_lambda,
   trap_action.ml, via translate_raise -- reviewer-verified,
   KF-031). *)
Definition tc_raise_kind_opt (rk : option Syntax.raise_kind)
  : Cmm.raise_kind :=
  match rk with
  | Some r => tc_raise_kind r
  | None => Raise_notrace
  end.

(* Two-arm switch re-tagging (TC.Switch): the tagged form of an
   immediate d is 2d+1. *)
Definition tc_disc (tagd : bool) (d : Z) : Z :=
  if tagd then 2 * d + 1 else d.

(* C.tag_int's shape: (sc << 1) + 1. *)
Definition tc_retag (tagd : bool) (sc : cmm_expr) : cmm_expr :=
  if tagd
  then Cop Caddi [Cop Clsl [sc; Cconst_int 1]; Cconst_int 1]
  else sc.

(* The index-table form of TC.Switch (transl_switch_clambda): index
   covers [0, max_d]; each arm's discriminant maps to that arm's
   translation, and every gap maps to a synthesized Cinvalid case
   (needs_unreachable; the code's message is "unreachable switch
   case").  Scrutinee values above max_d fall out of the table:
   genuine UB.
   ENCODING NOTE: the relation permits the Cinvalid case to be
   appended even when there is no gap; needs_unreachable emits it
   exactly when a gap exists.  Harmless over-approximation: the extra
   case is then unreachable from the index. *)
Definition switch_table_ok (arms_c : list (Z * cmm_expr))
    (index : list nat) (cases : list cmm_expr) : Prop :=
  exists max_d,
    In max_d (map fst arms_c) /\
    (forall d, In d (map fst arms_c) -> 0 <= d <= max_d) /\
    length index = S (Z.to_nat max_d) /\
    (forall i d e,
        nth_error arms_c i = Some (d, e) ->
        nth_error index (Z.to_nat d) = Some i /\
        nth_error cases i = Some e) /\
    (forall j,
        (j < length index)%nat ->
        ~ In (Z.of_nat j) (map fst arms_c) ->
        exists ci msg,
          nth_error index j = Some ci /\
          nth_error cases ci = Some (Cinvalid msg)) /\
    (cases = map snd arms_c \/
     exists msg, cases = map snd arms_c ++ [Cinvalid msg]).

(* The emitted switch shapes over already-translated arms (the
   discriminants ride along with each arm's Cmm translation).
   Two arms: an if-then-else, kept as C.ite so Selectgen can fuse the
   comparison -- normally an equality test against one arm's
   (post-re-tagging) discriminant, or the bare-scrutinee collapse
   when an untagged discriminant is literally 0 (arm 0 as the else
   branch).  More than two arms: a Cswitch with an index table; a
   two-arm switch never takes the table form.  Fewer than two arms
   cannot occur (WF.Syntax.SwitchMinArms).
   ENCODING NOTE (KF-032): which arm is tested, whether the test is
   an equality or a disequality (the doc's "(if (!= x d))" form, a
   genuine Cne node -- NOT derivable by swapping branches under Ceq;
   the TCSw_Two_Ne clause below), and whether the scrutinee is
   re-tagged (must_tag_discriminant's size heuristic,
   cmm_arith_size), are deterministic compile-time choices in the
   code; all are nondeterministic here.
   UNDER-APPROXIMATION (KF-032, reviewer-confirmed): in the
   must_tag case the code does not re-tag syntactically -- it
   SELECTS a pre-existing tagged expression from Env.extra_info
   (to_cmm_expr.ml, typically a Cvar), so real tagged outputs match
   no clause's shape; tc_retag's (sc << 1) + 1 is the value-correct
   direct form.  The gap is a value-level equality (the selected
   expression computes 2*[[sc]]+1), the same family as ToCmmData's
   array_indexing note; the stated obligation is the kernel
   TC_Switch_TestByValue below (catalog-62 family), which ch. 20's
   simulation consumes to relate the direct form to the selected
   one.  All permitted forms agree on scrutinees drawn from the arm
   discriminants; other scrutinees are UB (OS.Switch.Undef). *)
Inductive tc_switch_form
  : cmm_expr -> list (Z * cmm_expr) -> cmm_expr -> Prop :=
  | TCSw_Two_Eq : forall sc tagd d0 e0 d1 e1 dt et eo,
      ((dt, et) = (d0, e0) /\ eo = e1 \/
       (dt, et) = (d1, e1) /\ eo = e0) ->
      tc_disc tagd dt <> 0 ->
      tc_switch_form sc [(d0, e0); (d1, e1)]
        (Cifthenelse
           (Cop (Ccmpi Ceq)
              [Cconst_int (tc_disc tagd dt); tc_retag tagd sc])
           et eo)
  | TCSw_Two_Ne : forall sc tagd d0 e0 d1 e1 dt et eo,
      ((dt, et) = (d0, e0) /\ eo = e1 \/
       (dt, et) = (d1, e1) /\ eo = e0) ->
      tc_disc tagd dt <> 0 ->
      tc_switch_form sc [(d0, e0); (d1, e1)]
        (Cifthenelse
           (Cop (Ccmpi Cne)
              [Cconst_int (tc_disc tagd dt); tc_retag tagd sc])
           eo et)
  | TCSw_Two_Zero : forall sc d0 e0 d1 e1 ez eother,
      (d0 = 0 /\ ez = e0 /\ eother = e1 \/
       d1 = 0 /\ ez = e1 /\ eother = e0) ->
      tc_switch_form sc [(d0, e0); (d1, e1)]
        (Cifthenelse sc eother ez)
  | TCSw_Table : forall sc arms_c index cases,
      (2 < length arms_c)%nat ->
      switch_table_ok arms_c index cases ->
      tc_switch_form sc arms_c (Cswitch sc index cases).

(* KF-032 kernel (catalog-62 family; the stated obligation for the
   must_tag UNDER-APPROXIMATION above): a two-arm test expression
   matters only through its VALUE.  If e_t and e_t' both evaluate
   silently and state-restoringly to the same word, and neither
   test's leftover binder extensions touch anything the branches
   mention, the two if-then-else forms have the same terminating
   runs.  The agreement premises are load-bearing, not caution
   (reviewer finding KF-037): the machine's environments are flat
   (CM_Head_Let persists bindings; CM_If runs the branch in the
   test-extended venv), so without them a test binding a variable
   or label the branch reads refutes the statement outright.
   cmm_mentions / cmm_mentions_label (Cmm.v) over-approximate free
   occurrence (binders and trap actions count), which only
   strengthens the premises; ch. 20 discharges them by freshness of
   translation-introduced backend variables and labels, which the
   branches never mention. *)
Theorem TC_Switch_TestByValue :
  forall (P : cmm_program) (e_t e_t' e1 e0 : cmm_expr)
         (ce ce2 ce2' : cmm_venv) (chi chi2 chi2' : cmm_kenv)
         (M : cmm_mem) (TT : list static_label)
         (RR : list region_handle) (w w' : cmm_value)
         (tr : list cm_event) (ce3 : cmm_venv) (chi3 : cmm_kenv)
         (M3 : cmm_mem) (TT3 : list static_label)
         (RR3 : list region_handle),
    cmem_run P (CmCfg e_t ce chi M TT RR) []
               (CmCfg (Cval w) ce2 chi2 M TT RR) ->
    cmem_run P (CmCfg e_t' ce chi M TT RR) []
               (CmCfg (Cval w) ce2' chi2' M TT RR) ->
    (forall x,
        (cmm_mentions x e1 || cmm_mentions x e0)%bool = true ->
        ce2 x = ce x /\ ce2' x = ce x) ->
    (forall l,
        (cmm_mentions_label l e1 || cmm_mentions_label l e0)%bool
          = true ->
        chi2 l = chi l /\ chi2' l = chi l) ->
    cmem_run P (CmCfg (Cifthenelse e_t e1 e0) ce chi M TT RR) tr
               (CmCfg (Cval w') ce3 chi3 M3 TT3 RR3) ->
    exists ce3' chi3',
      cmem_run P (CmCfg (Cifthenelse e_t' e1 e0) ce chi M TT RR) tr
                 (CmCfg (Cval w') ce3' chi3' M3 TT3 RR3).
Admitted.

(* ================================================================== *)
(* 3. The translation judgment (16 s1-s7)                             *)
(* ================================================================== *)

Section ToCmmControl.

(* Ch. 18 hooks.  The doc's s1 assigns V (simple translation) and D
   (delayed bindings) to 18-to-cmm-data.md; ch. 16 uses them only
   through the judgment "Theta |- s ~>v ce" and flush side
   conditions.  Mirroring Cmm.v's extension-hook pattern they enter
   as Section Variables, closed by ToCmmData.v.
   ENCODING NOTE (hook inventory; cataloged with main):
     - data_state bundles V and D opaquely;
     - simple_translates / simples_translate are the doc's ~>v on one
       simple / on an argument list (the list form is not pointwise:
       remove_skipped_args may drop arguments of proven-unused
       params);
     - bind_var_to_simple / bind_var_to_cmm are to_cmm_env's
       bind_var_to_simple / bind_variable (Inline-continuation
       argument binding, and call-result binding);
     - var_binds binds Flambda params to fresh Cmm variables at
       handler entry (a V extension);
     - tc_flush is the flush side condition (the materialized Clets
       and the D update are ch. 18's);
     - machtype_of_kind classifies a Flambda kind as a Cmm machtype;
     - let_binding_ext is the whole TC.Let-star data family
       (TC.Expr.Dispatch's Let row): it yields the updated env and
       the emitted binding context (identity when the binding is
       delayed);
     - lookup_code resolves code ids to code0 (the unit's code
       environment);
     - classify_handler stands for the occurrence-hint inputs of
       classify_continuation_handler (num_free_occurrences,
       is_applied_with_traps), elided from E_let_cont_nonrec;
     - debug_flag mirrors the global !Clflags.debug
       (TC.ApplyCont.Raise);
     - indirect_call_image / indirect_full_call_image /
       extcall_image are C.indirect_call (generic caml_apply),
       C.indirect_full_call, and C.extcall: closure-layout and
       calling-convention expansions with no owning rules in any
       chapter; the doc places the conventions at CM.Apply /
       CM.Extcall (ch. 15; ch. 20 treats them as axiomatized
       there), so ToCmmData.v leaves these hooks quantified
       (coordinator ruling). *)

Variable data_state : Type.

Local Abbreviation tcenv := (tc_env data_state).

Variable simple_translates : tcenv -> simple -> cmm_expr -> Prop.
Variable simples_translate :
  tcenv -> list simple -> list cmm_expr -> Prop.
Variable bind_var_to_simple :
  tcenv -> variable -> simple -> tcenv -> Prop.
Variable bind_var_to_cmm :
  tcenv -> variable -> cmm_expr -> tcenv -> Prop.
Variable var_binds :
  tcenv -> list (variable * kind_ws) -> list backend_var
  -> tcenv -> Prop.
Variable tc_flush : flush_mode -> tcenv -> Prop.
Variable machtype_of_kind : kind_ws -> machtype.
Variable let_binding_ext :
  tcenv -> bound_pattern -> named -> tcenv
  -> (cmm_expr -> cmm_expr) -> Prop.
Variable lookup_code : code_id -> option code0.
Variable classify_handler :
  continuation -> cont_handler -> expr -> cont_class -> Prop.
Variable debug_flag : bool.
Variable indirect_call_image :
  tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop.
Variable indirect_full_call_image :
  tcenv -> cmm_expr -> list cmm_expr -> cmm_expr -> Prop.
Variable extcall_image :
  tcenv -> apply_expr -> list cmm_expr -> cmm_expr -> Prop.

Definition param_machtypes (ps : list (variable * kind_ws))
  : list machtype :=
  map (fun p => machtype_of_kind (snd p)) ps.

(* Phi entries of a recursive group: k_i realized
   Jump <(zs ++ params_i) machtypes, lbl_i>, positionally. *)
Fixpoint rec_jump_entries (zs : list (variable * kind_ws))
    (lbls : list static_label)
    (hs : list (continuation * cont_handler)) {struct lbls}
  : list (continuation * realization) :=
  match lbls, hs with
  | lbl :: lbls', (k, h) :: hs' =>
      (k, R_jump (param_machtypes (zs ++ ch_params h)) lbl)
        :: rec_jump_entries zs lbls' hs'
  | _, _ => []
  end.

(* Trap-action translation of TC.ApplyCont.Jump: Push/Pop map
   one-to-one, resolving the handler continuation to its Cmm label
   through Phi (get_cmm_continuation). *)
Inductive tc_trap (th : tcenv)
  : option Syntax.trap_action -> list Cmm.trap_action -> Prop :=
  | TCTrap_None : tc_trap th None []
  | TCTrap_Push : forall k_h lbl_h,
      tc_phi th k_h = Some (R_exn lbl_h) ->
      tc_trap th (Some (Trap_push k_h)) [Push lbl_h]
  | TCTrap_Pop : forall k_h rk lbl_h,
      tc_phi th k_h = Some (R_exn lbl_h) ->
      tc_trap th (Some (Trap_pop k_h rk)) [Pop lbl_h].

(** RULE TC.LetCont.Classify (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont
    CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler

    May_inline => realized Inline; Regular and not an exn handler =>
    Jump; Regular and an exn handler => Exn.  The rule's fourth
    clause -- a Recursive group is realized Jump -- is carried by the
    TC_LetCont_Rec constructor below, which realizes every member of
    the group as Jump directly.  May_inline holds for a
    non-recursive, non-exn, non-cold handler used exactly once and
    not applied with traps (the source-verified claim of 13 s4.6);
    those occurrence hints are elided from the syntax and enter
    through the classify_handler hook over the binder's body. *)
Inductive tc_realized (k : continuation) (h : cont_handler)
    (body : expr) : realized_kind -> Prop :=
  | TC_Classify_Inline :
      classify_handler k h body CC_may_inline ->
      tc_realized k h body RZ_inline
  | TC_Classify_Jump :
      classify_handler k h body CC_regular ->
      ch_is_exn_handler h = false ->
      tc_realized k h body RZ_jump
  | TC_Classify_Exn :
      classify_handler k h body CC_regular ->
      ch_is_exn_handler h = true ->
      tc_realized k h body RZ_exn.

(* TC.ApplyCont.Inline's parameter binding: each non-Rec_info param
   is bound to its argument simple via bind_var_to_simple;
   Rec_info/depth params are skipped (their argument is dropped). *)
Inductive tc_bind_args
  : tcenv -> list (variable * kind_ws) -> list simple -> tcenv
    -> Prop :=
  | TCBind_Nil : forall th, tc_bind_args th [] [] th
  | TCBind_Rec_Info : forall th x kw ps s ss th',
      ws_kind kw = K_rec_info ->
      tc_bind_args th ps ss th' ->
      tc_bind_args th ((x, kw) :: ps) (s :: ss) th'
  | TCBind_Bind : forall th x kw ps s ss th1 th',
      ws_kind kw <> K_rec_info ->
      bind_var_to_simple th x s th1 ->
      tc_bind_args th1 ps ss th' ->
      tc_bind_args th ((x, kw) :: ps) (s :: ss) th'.

(** RULE TC.Apply.Call (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0

    Call-kind dispatch.  The direct form is concrete: a Capply to the
    code symbol with callees = Some [cid], my_closure appended to the
    arguments iff is_my_closure_used.  The indirect and extern forms
    expand through cmm_helpers smart constructors whose bodies are
    closure-layout / calling-convention detail (C.indirect_call via
    generic caml_apply, C.indirect_full_call, C.extcall via
    translate_external_call): they enter as ch. 18 hooks.
    ENCODING NOTE: is_my_closure_used is code metadata elided from
    code0 (Syntax.v); per the coordinator-approved ruling it is the
    term predicate "my_closure occurs free in the code body", stated
    with free_vars.  When it does not hold, no callee translation is
    demanded (the callee closure may be absent or unused).
    ENCODING NOTE: the Capply result machtype rt and region_close rc
    are data/backend-side (ch. 18); nondeterministic here.
    Method calls and effect operations are emitted by translate_apply0
    but their semantics are out of scope (ch. 01): no constructor,
    per the rule's NOTES ("noted, not given a correctness
    obligation"). *)
Inductive tc_apply_call (th : tcenv) (ap : apply_expr)
  : cmm_expr -> Prop :=
  | TCall_Direct : forall cid c0 vs args' rt rc,
      ap_call_kind ap = CK_function (FC_direct cid) ->
      lookup_code cid = Some c0 ->
      simples_translate th (ap_args ap) vs ->
      ((free_vars (c0_body c0) (c0_my_closure c0) /\
        (exists callee_s callee_c,
            ap_callee ap = Some callee_s /\
            simple_translates th callee_s callee_c /\
            args' = vs ++ [callee_c])) \/
       (~ free_vars (c0_body c0) (c0_my_closure c0) /\
        args' = vs)) ->
      tc_apply_call th ap
        (Cop (Capply rt rc (Some [CS_code cid]))
           (Cconst_symbol (CS_code cid) :: args'))
  | TCall_Indirect_Unknown : forall callee_s callee_c vs e_call,
      ap_call_kind ap = CK_function FC_indirect_unknown_arity ->
      ap_callee ap = Some callee_s ->
      simple_translates th callee_s callee_c ->
      simples_translate th (ap_args ap) vs ->
      indirect_call_image th callee_c vs e_call ->
      tc_apply_call th ap e_call
  | TCall_Indirect_Full : forall cids callee_s callee_c vs e_call,
      ap_call_kind ap = CK_function (FC_indirect_known_arity cids) ->
      ap_callee ap = Some callee_s ->
      simple_translates th callee_s callee_c ->
      simples_translate th (ap_args ap) vs ->
      indirect_full_call_image th callee_c vs e_call ->
      tc_apply_call th ap e_call
  | TCall_C_Call : forall needs_c is_builtin eff coeff vs e_call,
      ap_call_kind ap = CK_c_call needs_c is_builtin eff coeff ->
      simples_translate th (ap_args ap) vs ->
      extcall_image th ap vs e_call ->
      tc_apply_call th ap e_call.

(** RULE TC.Apply.ExnWrapper (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply
    CODE backend/cmm_helpers.ml#trywith
    CODE backend/cmm_helpers.ml#raise_prim

    The arity bridge for callee exceptions (KF-033, the Cmm-side
    twin of KF-019): when the Apply's exn continuation carries
    extra args, the call is wrapped so the callee's bucket-only
    uncaught exception (CM.Apply.Raise) is caught in a fresh
    extras-FREE Exn_handler and reraised with the extras as the
    remaining Craise operands -- translated at the CALL site,
    where they are in scope.  The Exn_handler is outermost
    (trywith is applied last, wrapping the pop/push scaffold);
    the reraise kind is debug-gated as in TC.ApplyCont.Raise
    (raise_prim).  The Push/Pop trap actions ride the Cexits,
    balancing the trap stack on both the return and raise paths.
    TC.Apply.Return consumes call' in place of call (the tying
    constructor TC_Apply below).  Steps to CM.Catch.Exn /
    CM.Raise.
    ENCODING NOTE: x_res's return-machtype annotation drops
    (cmm_static_handler params carry no machtypes;
    CM.Syntax.Fragment), and emitted handler coldness is false
    (the code's trywith / create_ccatch calls).  "Fresh" is
    stated locally, per file convention: the three labels by
    phi_label_fresh plus pairwise distinctness, and the one
    LOCAL capture hazard -- x_exn occurring in the translated
    extras, which are evaluated in the handler body under
    x_exn's binding -- by a cmm_mentions guard (the KF-035/
    KF-037 slack lesson: an unguarded choice would admit
    behaviour-changing instances no real output has).  x_res
    has no local hazard (its only occurrence is the pop
    handler's own body); global freshness of both variables
    (the code's next_raise_count / create_local) is ch. 20's
    discharge, as with KF-037's agreement premises.  The extras
    length premise is KF-042's raise-operand fix, as at
    TC.ApplyCont.Raise: the translated extras feed the wrapper's
    Craise tail directly, so skips must be excluded by length. *)
Inductive tc_exn_wrapper (th : tcenv) (ap : apply_expr)
  : cmm_expr -> cmm_expr -> Prop :=
  | TCWrap_None : forall call,
      ec_extra_args (ap_exn_continuation ap) = [] ->
      tc_exn_wrapper th ap call call
  | TCWrap_Extras : forall call ss_extra vs_extra x_exn x_res
        lbl_w lbl_pop lbl_push,
      ec_extra_args (ap_exn_continuation ap) <> [] ->
      ss_extra = map fst (ec_extra_args (ap_exn_continuation ap)) ->
      simples_translate th ss_extra vs_extra ->
      length vs_extra = length ss_extra ->
      Forall (fun v => cmm_mentions x_exn v = false) vs_extra ->
      phi_label_fresh th lbl_w ->
      phi_label_fresh th lbl_pop ->
      phi_label_fresh th lbl_push ->
      lbl_w <> lbl_pop ->
      lbl_w <> lbl_push ->
      lbl_pop <> lbl_push ->
      tc_exn_wrapper th ap call
        (Ccatch Exn_handler
           [SHandler lbl_w [x_exn]
              (Cop
                 (Craise
                    (if debug_flag then Raise_reraise
                     else Raise_notrace))
                 (Cvar x_exn :: vs_extra)) false]
           (Ccatch Normal
              [SHandler lbl_pop [x_res] (Cvar x_res) false]
              (Ccatch Normal
                 [SHandler lbl_push []
                    (Cexit (Lbl lbl_pop) [call] [Pop lbl_w]) false]
                 (Cexit (Lbl lbl_push) [] [Push lbl_w])))).

(** RULE TC.Expr.Dispatch (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr

    The translation is structural on the expression grammar: Let ->
    the ch. 18 data rules (through the let_binding_ext hook,
    constructor TC_Let_Data); Let_cont -> s3; Apply -> s5;
    Apply_cont -> s4; Switch -> s6; Invalid -> s7.  Of expr's
    quadruple result only the Cmm expression is semantic, so the
    judgment records just it. *)
Inductive tc_expr : tcenv -> expr -> cmm_expr -> Prop :=
  (* Dispatch row for Let: the defining named is data (ch. 18); the
     hook yields the updated env and the emitted binding context
     (identity when the binding is delayed), and translation
     continues structurally with the body. *)
  | TC_Let_Data : forall th p dfn body th' ctx body_c,
      let_binding_ext th p dfn th' ctx ->
      tc_expr th' body body_c ->
      tc_expr th (E_let p dfn body) (ctx body_c)

  (** RULE TC.LetCont.Inline (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_inlined
      CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_inline_cont

      No Cmm is emitted for the binder; the handler is recorded in
      Phi and spliced in at k's unique use site (TC.ApplyCont.Inline,
      or the Apply return-continuation Inline cases of
      TC.Apply.Return).  occ is the recorded occurrence count
      (add_inline_cont). *)
  | TC_LetCont_Inline : forall th k h e_body occ e_c,
      tc_realized k h e_body RZ_inline ->
      tc_expr
        (upd_phi th k (R_inline (ch_params h) (ch_handler h) occ))
        e_body e_c ->
      tc_expr th (E_let_cont_nonrec k h e_body) e_c

  (** RULE TC.LetCont.Jump (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined
      CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_jump_cont

      A non-inlined ordinary continuation becomes a non-recursive
      Ccatch (create_ccatch, rec_flag false); the Branching_point
      flush prevents a delayed binding from being duplicated into
      both body and handler.  The handler is translated under Theta_j
      with the params bound to fresh Cmm variables (var_binds).
      Steps to CM.Catch.NonRec / CM.Exit.  is_cold carries the
      handler's coldness. *)
  | TC_LetCont_Jump : forall th k h e_body lbl th_j xs_c th_h
        e_body_c e_h_c,
      tc_realized k h e_body RZ_jump ->
      ch_is_exn_handler h = false ->
      phi_label_fresh th lbl ->
      th_j = upd_phi th k
               (R_jump (param_machtypes (ch_params h)) lbl) ->
      tc_flush FM_branching_point th ->
      tc_expr th_j e_body e_body_c ->
      var_binds th_j (ch_params h) xs_c th_h ->
      tc_expr th_h (ch_handler h) e_h_c ->
      tc_expr th (E_let_cont_nonrec k h e_body)
        (Ccatch Normal [SHandler lbl xs_c e_h_c (ch_is_cold h)]
           e_body_c)

  (** RULE TC.LetCont.Exn (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler
      CODE backend/cmm_helpers.ml#trywith

      An exception-handler continuation becomes a Ccatch with flag
      Exn_handler, built by the trywith smart constructor -- there is
      no distinct Ctrywith machine node (CM.Catch.Exn).  The first
      parameter is the exception value, the rest are extra args
      (threaded through distinguished per-handler registers by
      cfg_selectgen, not mutable slots).  The handler body is
      translated under the ORIGINAL Theta (doc: "Theta |- e_h"), the
      body under Theta_x.  The trap-stack push is NOT part of this
      catch: it is a Push trap action on the Cexit entering the try
      body (TC.ApplyCont.Jump), with a matching Pop on the normal
      exit.  Steps to CM.Catch.Exn / CM.Raise. *)
  | TC_LetCont_Exn : forall th k h e_body lbl th_x xs_c th_h
        e_body_c e_h_c px pxs,
      tc_realized k h e_body RZ_exn ->
      ch_params h = px :: pxs ->
      ch_is_exn_handler h = true ->
      phi_label_fresh th lbl ->
      th_x = upd_phi th k (R_exn lbl) ->
      tc_expr th_x e_body e_body_c ->
      var_binds th (ch_params h) xs_c th_h ->
      tc_expr th_h (ch_handler h) e_h_c ->
      tc_expr th (E_let_cont_nonrec k h e_body)
        (Ccatch Exn_handler [SHandler lbl xs_c e_h_c (ch_is_cold h)]
           e_body_c)

  (** RULE TC.LetCont.Rec (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec

      A recursive continuation group is a recursive Ccatch
      (create_ccatch, rec_flag true); every member is realized Jump
      (the Recursive clause of TC.LetCont.Classify).  The
      Entering_loop flush is essential: it forbids inlining a binding
      into the loop body.  Recursive groups cannot contain exn
      handlers (contains_exn_handler check).  invariant_params are a
      leading prefix of each handler's params (OS.LetCont.Rec);
      handler coldness is false in the emitted handlers.  Steps to
      CM.Catch.Rec. *)
  | TC_LetCont_Rec : forall th zs handlers e_body lbls th_r
        e_body_c shs,
      Forall (fun kh => ch_is_exn_handler (snd kh) = false)
        handlers ->
      length lbls = length handlers ->
      NoDup lbls ->
      Forall (phi_label_fresh th) lbls ->
      th_r = upd_phi_list th (rec_jump_entries zs lbls handlers) ->
      tc_flush FM_entering_loop th ->
      tc_expr th_r e_body e_body_c ->
      tc_rec_handlers th_r zs lbls handlers shs ->
      tc_expr th (E_let_cont_rec zs handlers e_body)
        (Ccatch Recursive shs e_body_c)

  (** RULE TC.ApplyCont.Jump (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation

      A jump becomes Cexit carrying the translated trap actions
      (tc_trap: Push/Pop map one-to-one).  Cmm image of OS.ApplyCont
      / OS.ApplyCont.TrapPush / TrapPop; steps to CM.Exit /
      CM.Exit.Trap.  Argument skipping (remove_skipped_args) lives in
      the simples_translate hook. *)
  | TC_ApplyCont_Jump : forall th k ss ta ptys lbl tas vs,
      tc_phi th k = Some (R_jump ptys lbl) ->
      tc_trap th ta tas ->
      simples_translate th ss vs ->
      tc_flush FM_flush_everything th ->
      tc_expr th (E_apply_cont (Mk_apply_cont k ss ta))
        (Cexit (Lbl lbl) vs tas)

  (** RULE TC.ApplyCont.Return (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_return_continuation
      CODE backend/cmm_helpers.ml#trap_return

      A call to the current function's return continuation is not a
      jump: the argument tuple is the Cmm function body's result,
      reaching CM.Unit.Final / CM.Exit at Return_lbl.  A Pop trap
      action becomes a trap_return (a Return_lbl Cexit carrying the
      Pop).  A Push on the return continuation is ill-formed (fatal):
      no clause. *)
  | TC_ApplyCont_Return : forall th k ss ta ptys vs e_c,
      tc_phi th k = Some (R_return ptys) ->
      simples_translate th ss vs ->
      tc_flush FM_flush_everything th ->
      (ta = None /\ e_c = tc_make_tuple vs \/
       (exists k_h rk lbl_h,
           ta = Some (Trap_pop k_h rk) /\
           tc_phi th k_h = Some (R_exn lbl_h) /\
           e_c = Cexit Return_lbl [tc_make_tuple vs] [Pop lbl_h])) ->
      tc_expr th (E_apply_cont (Mk_apply_cont k ss ta)) e_c

  (** RULE TC.ApplyCont.Inline (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_cont

      Calling an inlined continuation splices its handler in, binding
      the params to the argument simples (tc_bind_args over
      bind_var_to_simple; Rec_info params skipped).  Emits no Cmm
      control construct -- this is where TC.LetCont.Inline pays off.
      An inlined continuation must not carry a trap action (checked;
      fatal otherwise): the source pattern requires None. *)
  | TC_ApplyCont_Inline : forall th k ss xs e_h occ th' e_h_c,
      tc_phi th k = Some (R_inline xs e_h occ) ->
      length ss = length xs ->
      tc_bind_args th xs ss th' ->
      tc_expr th' e_h e_h_c ->
      tc_expr th (E_apply_cont (Mk_apply_cont k ss None)) e_h_c

  (** RULE TC.ApplyCont.Raise (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise
      CODE backend/cmm_helpers.ml#raise_prim

      A raise -- an Apply_cont to an exn handler carrying a Pop --
      becomes raise_prim: the exception value is the first Craise
      operand, the extra handler args are the remaining operands.
      raise_prim preserves the source raise kind only under
      !Clflags.debug (the debug_flag hook); in the default build the
      operator is Raise_notrace (backtrace recording only, not
      control flow).  Cmm image of OS.ApplyCont.Raise; steps to
      CM.Raise.  Reaching an exn handler without a Pop is a fatal
      to_cmm error: no clause.  The Pop's own handler field is left
      unconstrained, matching the doc's ellipsis in
      "Some (Pop { raise_kind; ... })".  The length premise is
      load-bearing (reviewer finding KF-042): simples_translate's
      skip row may drop positions, and unlike call sites -- where
      ch. 16's parameter pairing re-fixes the arity -- the operand
      tail here feeds Craise directly, so a dropped extra would
      mismatch the handler's parameter count at CM.Raise; the real
      code translates every extra (translate_raise's simple_list).
      Skips only shorten, so length equality forces the pointwise
      reading. *)
  | TC_ApplyCont_Raise : forall th k s_exn ss_extra k_h rk lbl_h
        v_exn vs_extra,
      tc_phi th k = Some (R_exn lbl_h) ->
      simple_translates th s_exn v_exn ->
      simples_translate th ss_extra vs_extra ->
      length vs_extra = length ss_extra ->
      tc_flush FM_flush_everything th ->
      tc_expr th
        (E_apply_cont
           (Mk_apply_cont k (s_exn :: ss_extra)
              (Some (Trap_pop k_h rk))))
        (Cop
           (Craise
              (if debug_flag then tc_raise_kind_opt rk
               else Raise_notrace))
           (v_exn :: vs_extra))

  (* The tying constructor of TC.Apply.Call / TC.Apply.ExnWrapper /
     TC.Apply.Return: the call expression selected by call kind,
     wrapped when the exn continuation carries extras, placed by
     the return continuation (which consumes call' in place of
     call, per TC.Apply.ExnWrapper's conclusion). *)
  | TC_Apply : forall th ap call call' e_c,
      tc_apply_call th ap call ->
      tc_exn_wrapper th ap call call' ->
      tc_apply_return th (ap_result_continuation ap) call' e_c ->
      tc_expr th (E_apply ap) e_c

  (** RULE TC.Switch (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
      CODE backend/cmm_helpers.ml#transl_switch_clambda
      CODE backend/cmm_helpers.ml#ite

      Each arm action (an Apply_cont) is translated by the s4 rules
      (tc_arms); the Branching_point flush keeps bindings from being
      duplicated across arms; the emitted shape (two-arm if-then-else
      with optional re-tagging, or index-table Cswitch) is
      tc_switch_form above.  Cmm image of OS.Switch /
      OS.Switch.Undef; steps to CM.If / CM.Switch. *)
  | TC_Switch : forall th s arms sc arms_c e_c,
      simple_translates th s sc ->
      tc_arms th arms arms_c ->
      tc_flush FM_branching_point th ->
      tc_switch_form sc arms_c e_c ->
      tc_expr th (E_switch (Mk_switch s arms)) e_c

  (** RULE TC.Invalid (CLAIM normative) -- 16-to-cmm-control.md
      CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
      CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid

      Flambda Invalid (OS.Invalid) becomes Cmm Cinvalid (CM.Invalid);
      the message-symbol allocation of C.invalid is elided (the
      message rides in the node).  A correct pipeline never reaches
      it (ch. 20 composes with ch. 13's "reachable does not imply
      Invalid"). *)
  | TC_Invalid : forall th msg,
      tc_flush FM_flush_everything th ->
      tc_expr th (E_invalid msg) (Cinvalid msg)

(** RULE TC.Apply.Return (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr

    Placement of the call result by return continuation (apply_expr's
    cases; the tying constructor is TC_Apply above).  Never_returns
    and a Return-continuation that is the function's own return leave
    the call as the tail expression.  A Jump target wraps the call in
    a Cexit to the join point.  A single-param Inline binds the call
    result (bind_var_to_cmm) and splices the handler, emitting no Cmm
    control.  A multi-param Inline uses a Ccatch/Cexit pair to
    destructure the unboxed multi-value result.  NOTES' wrap of the
    call in a reraising trywith when the Apply's exn continuation
    carries extra args is TC.Apply.ExnWrapper (tc_exn_wrapper,
    above tc_expr), threaded by the tying constructor TC_Apply:
    this relation consumes call' in place of call, per that rule's
    conclusion (fidelity finding KF-033 resolved). *)
with tc_apply_return
  : tcenv -> result_continuation -> cmm_expr -> cmm_expr -> Prop :=
  | TCRet_Never : forall th call,
      tc_flush FM_flush_everything th ->
      tc_apply_return th RC_never_returns call call
  | TCRet_Return : forall th k ptys call,
      tc_phi th k = Some (R_return ptys) ->
      tc_flush FM_flush_everything th ->
      tc_apply_return th (RC_return k) call call
  | TCRet_Jump : forall th k ptys lbl call,
      tc_phi th k = Some (R_jump ptys lbl) ->
      tc_flush FM_flush_everything th ->
      tc_apply_return th (RC_return k) call
        (Cexit (Lbl lbl) [call] [])
  | TCRet_Inline_Single : forall th k x kw e_h occ th' e_h_c call,
      tc_phi th k = Some (R_inline [(x, kw)] e_h occ) ->
      bind_var_to_cmm th x call th' ->
      tc_expr th' e_h e_h_c ->
      tc_apply_return th (RC_return k) call e_h_c
  | TCRet_Inline_Multi : forall th k xs e_h occ lbl xs_c th_h
        e_h_c call,
      tc_phi th k = Some (R_inline xs e_h occ) ->
      (1 < length xs)%nat ->
      tc_flush FM_branching_point th ->
      phi_label_fresh th lbl ->
      var_binds th xs xs_c th_h ->
      tc_expr th_h e_h e_h_c ->
      tc_apply_return th (RC_return k) call
        (Ccatch Normal [SHandler lbl xs_c e_h_c false]
           (Cexit (Lbl lbl) [call] []))

(* Switch arms: the actions are Apply_cont expressions translated by
   the s4 rules; the discriminants ride along for tc_switch_form. *)
with tc_arms
  : tcenv -> list (target_ocaml_int * apply_cont_expr)
    -> list (Z * cmm_expr) -> Prop :=
  | TCArms_Nil : forall th, tc_arms th [] []
  | TCArms_Cons : forall th d ac e_c arms arms_c,
      tc_expr th (E_apply_cont ac) e_c ->
      tc_arms th arms arms_c ->
      tc_arms th ((d, ac) :: arms) ((d, e_c) :: arms_c)

(* Handler bodies of a recursive group, translated under the group
   env with invariant params ++ own params bound to fresh Cmm
   variables; emitted handler coldness is false (TC.LetCont.Rec's
   conclusion). *)
with tc_rec_handlers
  : tcenv -> list (variable * kind_ws) -> list static_label
    -> list (continuation * cont_handler)
    -> list cmm_static_handler -> Prop :=
  | TCRec_Nil : forall th zs, tc_rec_handlers th zs [] [] []
  | TCRec_Cons : forall th zs lbl lbls k h hs xs_c th_h e_h_c shs,
      var_binds th (zs ++ ch_params h) xs_c th_h ->
      tc_expr th_h (ch_handler h) e_h_c ->
      tc_rec_handlers th zs lbls hs shs ->
      tc_rec_handlers th zs (lbl :: lbls) ((k, h) :: hs)
        (SHandler lbl xs_c e_h_c false :: shs).

(* ================================================================== *)
(* 4. The control simulation lemma (16 s8)                            *)
(* ================================================================== *)

(* The control relation of INV.ToCmm.Control, at the doc's own
   granularity: K-entries are linked to Phi's realizations by CLASS
   (the doc writes "Jump |-> CHandler at the same label; Inline |->
   spliced; Return |-> the function boundary; Exn |-> Exn_handler"),
   not field-by-field. *)
Definition centry_is_handler (en : centry) : Prop :=
  match en with
  | CE_handler _ _ _ _ _ | CE_rec _ _ _ _ _ => True
  | _ => False
  end.

Definition centry_is_return (en : centry) : Prop :=
  match en with
  | CE_return _ _ _ _ _ _ | CE_halt_return => True
  | _ => False
  end.

Definition centry_is_exn (en : centry) : Prop :=
  match en with
  | CE_exn _ _ _ _ _ _ | CE_halt_exn => True
  | _ => False
  end.

(* chi carries a handler of the given Ccatch kind at lbl.  (A member
   of a recursive group reads back as kind Normal via chi_lookup,
   matching TC.LetCont.Rec's Jump realization.) *)
Definition chi_has_kind (chi : cmm_kenv) (lbl : static_label)
    (kd : ccatch_flag) : Prop :=
  exists hv, chi_lookup chi lbl = Some hv /\ chv_kind hv = kd.

(* "linking the Flambda continuation env K with chi via Phi". *)
Definition ctrl_kenv (th : tcenv) (K : kenv) (chi : cmm_kenv)
  : Prop :=
  forall k r,
    tc_phi th k = Some r ->
    match r with
    | R_return _ =>
        exists en, K k = Some en /\ centry_is_return en
    | R_jump _ lbl =>
        (exists en, K k = Some en /\ centry_is_handler en) /\
        chi_has_kind chi lbl Normal
    | R_inline _ _ _ =>
        exists en, K k = Some en /\ centry_is_handler en
    | R_exn lbl =>
        (exists en, K k = Some en /\ centry_is_exn en) /\
        chi_has_kind chi lbl Exn_handler
    end.

(* "the trap stacks T ~ TT (same handler identities, same depth)":
   pointwise through Phi, so depth equality is Forall2's. *)
Definition ctrl_traps (th : tcenv) (T : trap_stack)
    (TT : list static_label) : Prop :=
  Forall2 (fun k_h lbl_h => tc_phi th k_h = Some (R_exn lbl_h)) T TT.

(* The rule's transition side condition "(an OS.LetCont*/ApplyCont*/
   Switch/Apply/Invalid step)", by source-expression shape (the doc
   fix adding OS.LetCont* to the parenthetical landed;
   16-to-cmm-control.md line 380). *)
Definition control_shape (e : expr) : Prop :=
  match e with
  | E_let _ _ _ => False
  | _ => True
  end.

(** RULE INV.ToCmm.Control (CLAIM normative) -- 16-to-cmm-control.md
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
    CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler

    Premises, in doc order: Theta |- e ~> e_c; the control relation
    (ctrl_kenv linking K with chi via Phi; ctrl_traps; R = RR); the
    Flambda control transition (control_shape at the source, a
    silent fl_step between expression configurations).  Conclusion:
    a matching Cmm run with Theta' |- e' ~> e_c', the relation
    re-established, and H ~ M preserved unchanged.
    ENCODING NOTE (statement granularity; cataloged with main):
      - R ~ RR is equality: both sides draw region_handle from Base,
        and ch. 19 states the correspondence one-to-one on handles
        (CM.Region.End NOTES: iota-in-RR corresponds to iota-in-R).
      - The step is restricted to L_tau between Ctl_expr endpoints:
        the doc's configurations hold source expressions (Ctl_jump
        is the machine-internal boundary state, catalog #7, whose
        translation is value-level, chs. 17/20), and the
        event-emitting Apply row (C calls, which mutate the heap
        through the oracle) belongs to ch. 20's full simulation --
        the doc's "H ~ M is preserved unchanged" presupposes the
        silent reading.  "Unchanged" is H' = H and M' = M (the
        relation itself is ch. 17's; unchangedness needs no layout).
      - Stated over this Section's open ch. 18 hooks; the conjecture
        is read under the intended ToCmmData.v instantiation, where
        ch. 20's proof will discharge it by cases on the transition
        (the doc's NOTES: a specialization of the ch. 20 simulation
        to the control fragment).  Function-call steps enter the
        callee small-step on the Flambda side but big-step on the
        Cmm side (CM.Apply's nested run): matching them uses the
        ch. 20 call-frame devices; flagged with main. *)
Theorem INV_ToCmm_Control :
  forall (fl : eff_flags) (P : cmm_program) th e e_c rho K H T R
         e' rho' K' H' T' R' ce chi M TT RR,
  tc_expr th e e_c ->
  ctrl_kenv th K chi ->
  ctrl_traps th T TT ->
  R = RR ->
  control_shape e ->
  fl_step fl (mk_config (Ctl_expr e) rho K H T R) L_tau
             (mk_config (Ctl_expr e') rho' K' H' T' R') ->
  exists th' e_c' ce' chi' M' TT' RR',
    cmem_run P (CmCfg e_c ce chi M TT RR) []
               (CmCfg e_c' ce' chi' M' TT' RR') /\
    tc_expr th' e' e_c' /\
    ctrl_traps th' T' TT' /\
    R' = RR' /\
    ctrl_kenv th' K' chi' /\
    H' = H /\ M' = M.
Admitted.

End ToCmmControl.
