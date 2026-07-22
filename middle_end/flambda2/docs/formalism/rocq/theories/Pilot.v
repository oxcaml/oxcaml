(* Pilot.v: Tier-1 pilot proofs (wave 7).

   Concrete end-to-end executions of the instantiated machine
   (Machine.v): Qed-complete fl_unit_behavior derivations for small
   whole units.  pilot_return exercises OS.Unit.Init and the halting
   return entry; pilot_let adds a let-bound Simple and a variable
   lookup; pilot_loop drives a two-iteration counting loop through
   the K' knot (CE_rec / bind_rec_group, CORRESPONDENCE catalog #1):
   two apply-cont entries into a recursive group, each re-tying the
   fixed point at lookup time (HE_rec), with switch dispatch
   selecting the loop arm and then the exit arm.

   NO rule ids live in this file: every rule is transcribed at its
   defining site; this file only exercises them.

   DRAFT NOTE (task #27): increment 2 is IN (section 5): the
   const-fold micro-instance of INV.Simplify.Preserves.  Scope:
   (a) pilot_constfold_simplifies derives the Simplify side
   (S.Rewrite.Prim.ConstFold through rw_prim/rewrites/simplifies),
   conditional on ONE row of the sanctioned prim_arg_kinds oracle
   and on the argument constants being their own canonicals
   (which T.Env.Canonical.Least guarantees for every real E);
   (b) pilot_constfold_preserves proves the obs_equiv conclusion
   of INV.Simplify.Preserves outright for the concrete pair, by
   behavior-set uniqueness on both sides.  Discovery recorded
   below at H_mod: beh_sim observes the module block, so the
   starting heap must populate sym_mod for units (like these)
   that never store it. *)

(* String before List: List's length must win (build captain). *)
From Stdlib Require Import ZArith Bool String List Lia.
From Flambda2 Require Import Base Syntax Values Opsem PrimScalar
  PrimMemoryA TypeGrammar MeetJoin.
From Flambda2 Require Import Machine RewritesPrim Simplify
  Soundness.
Import ListNotations.

Set Implicit Arguments.

(* ================================================================ *)
(* 1. Common atoms                                                  *)
(* ================================================================ *)

(* Distinct ids keep the eqb-driven environment lookups computable
   by reflexivity. *)
Definition pk_ret : continuation := Mk_continuation 0%nat.
Definition pk_exn : continuation := Mk_continuation 1%nat.
Definition pk_loop : continuation := Mk_continuation 2%nat.
Definition pv_region : variable := Mk_variable 0%nat.
Definition pv_ghost : variable := Mk_variable 1%nat.
Definition pv_x : variable := Mk_variable 2%nat.
Definition pv_n : variable := Mk_variable 3%nat.
Definition psym : symbol := Mk_symbol 0%nat.

Definition pilot_unit (body : expr) : flambda_unit :=
  Mk_flambda_unit pk_ret pk_exn pv_region pv_ghost psym body.

(* ================================================================ *)
(* 2. Immediate return                                              *)
(* ================================================================ *)

(* The unit body jumps straight to the return continuation with a
   constant: the initial configuration is already a halting one
   (zero steps; trace and heap untouched). *)
Definition unit_ret : flambda_unit :=
  pilot_unit
    (E_apply_cont (Mk_apply_cont pk_ret
       [Simple_const (Const_tagged_immediate 0%Z)] None)).

Theorem pilot_return :
  forall (flags : eff_flags) (rho_pre : env) (H0 : heap),
    (forall x, env_get_var rho_pre x = None) ->
    fl_unit_behavior flags unit_ret rho_pre H0 (Beh_return [] H0).
Proof.
  intros flags rho_pre H0 Hno.
  unfold fl_unit_behavior.
  eexists.  split.
  - exact (@Init unit_ret rho_pre H0 (Mk_region_handle 0%nat) Hno).
  - unfold fl_has_behavior.
    eapply HB_return.
    + apply Steps_refl.
    + cbn.  eapply AAC_expr.  reflexivity.
    + reflexivity.
Qed.

(* ================================================================ *)
(* 3. Let-bound Simple, then return                                 *)
(* ================================================================ *)

(* One OS.Let.Simple step binds x to a constant; the return jump
   then evaluates x from the extended environment. *)
Definition unit_let : flambda_unit :=
  pilot_unit
    (E_let (BPat_singleton (Mk_bound_var pv_x NM_normal))
       (N_simple (Simple_const (Const_tagged_immediate 42%Z)))
       (E_apply_cont (Mk_apply_cont pk_ret
          [Simple_name (Name_var pv_x) Coercion_id] None))).

Theorem pilot_let :
  forall (flags : eff_flags) (rho_pre : env) (H0 : heap),
    (forall x, env_get_var rho_pre x = None) ->
    fl_unit_behavior flags unit_let rho_pre H0 (Beh_return [] H0).
Proof.
  intros flags rho_pre H0 Hno.
  unfold fl_unit_behavior.
  eexists.  split.
  - exact (@Init unit_let rho_pre H0 (Mk_region_handle 0%nat) Hno).
  - unfold fl_has_behavior.
    eapply HB_return.
    + eapply Steps_tau.
      * cbn.  eapply OS_Let_Simple.  reflexivity.
      * apply Steps_refl.
    + cbn.  eapply AAC_expr.  reflexivity.
    + reflexivity.
Qed.

(* ================================================================ *)
(* 4. A counting loop through the K' knot                           *)
(* ================================================================ *)

(* A recursive continuation group with one member k_loop, whose
   handler switches on its naked-immediate parameter n: arm 1 jumps
   back to k_loop with 0 (the second trip through the knot), arm 0
   exits to the return continuation.  Entered with n = 1, the run
   is: LetCont.Rec, ApplyCont (HE_rec, knot re-tied), Switch (arm
   1), ApplyCont (HE_rec again), Switch (arm 0), and the resulting
   apply-cont to pk_ret is the halting configuration.  The handler
   runs in its DEFINITION environment each time (rho_def, not the
   caller's), so both entries bind exactly [n]. *)
Definition loop_handler : cont_handler :=
  Mk_cont_handler [(pv_n, ws_of_kind K_naked_immediate)]
    (E_switch (Mk_switch (Simple_name (Name_var pv_n) Coercion_id)
       [(0%Z, Mk_apply_cont pk_ret
                [Simple_const (Const_tagged_immediate 0%Z)] None);
        (1%Z, Mk_apply_cont pk_loop
                [Simple_const (Const_naked_immediate 0%Z)] None)]))
    false false.

Definition unit_loop : flambda_unit :=
  pilot_unit
    (E_let_cont_rec [] [(pk_loop, loop_handler)]
       (E_apply_cont (Mk_apply_cont pk_loop
          [Simple_const (Const_naked_immediate 1%Z)] None))).

Theorem pilot_loop :
  forall (flags : eff_flags) (rho_pre : env) (H0 : heap),
    (forall x, env_get_var rho_pre x = None) ->
    fl_unit_behavior flags unit_loop rho_pre H0 (Beh_return [] H0).
Proof.
  intros flags rho_pre H0 Hno.
  unfold fl_unit_behavior.
  eexists.  split.
  - exact (@Init unit_loop rho_pre H0 (Mk_region_handle 0%nat) Hno).
  - unfold fl_has_behavior.
    eapply HB_return.
    + eapply Steps_tau.
      * cbn.  eapply OS_LetCont_Rec.  reflexivity.
      * eapply Steps_tau.
        { eapply OS_ApplyCont.
          - eapply AAC_expr.  reflexivity.
          - eapply HE_rec.
            + reflexivity.
            + reflexivity.
          - reflexivity. }
        eapply Steps_tau.
        { cbn.  eapply OS_Switch.
          - reflexivity.
          - reflexivity. }
        eapply Steps_tau.
        { eapply OS_ApplyCont.
          - eapply AAC_expr.  reflexivity.
          - eapply HE_rec.
            + reflexivity.
            + reflexivity.
          - reflexivity. }
        eapply Steps_tau.
        { cbn.  eapply OS_Switch.
          - reflexivity.
          - reflexivity. }
        apply Steps_refl.
    + cbn.  eapply AAC_expr.  reflexivity.
    + reflexivity.
Qed.

(* ================================================================ *)
(* 5. Increment 2: the const-fold micro-instance of                 *)
(*    INV.Simplify.Preserves                                        *)
(* ================================================================ *)

(* The pair: [let x = 1 +naked 2 in k_ret x] and its constant fold
   [let x = 3 in k_ret x].  S.Rewrite.Prim.ConstFold relates the
   two (section 5.2); both units have exactly the behavior
   Beh_return [] H0 from any starting state (5.3-5.5), and the
   obs_equiv conclusion of INV.Simplify.Preserves holds for them
   outright (5.7). *)

Definition c_one : const := Const_naked_immediate 1%Z.
Definition c_two : const := Const_naked_immediate 2%Z.
Definition c_three : const := Const_naked_immediate 3%Z.

Definition fold_prim : prim :=
  P_binary (BP_int_arith SI_naked_immediate BIA_add)
    (Simple_const c_one) (Simple_const c_two).

Definition halt_expr : expr :=
  E_apply_cont (Mk_apply_cont pk_ret
    [Simple_name (Name_var pv_x) Coercion_id] None).

Definition body_prim : expr :=
  E_let (BPat_singleton (Mk_bound_var pv_x NM_normal))
    (N_prim fold_prim) halt_expr.

Definition body_folded : expr :=
  E_let (BPat_singleton (Mk_bound_var pv_x NM_normal))
    (N_simple (Simple_const c_three)) halt_expr.

Definition unit_prim : flambda_unit := pilot_unit body_prim.
Definition unit_folded : flambda_unit := pilot_unit body_folded.

(* The continuation environment OS.Unit.Init builds for any
   pilot_unit (definitionally equal to Init's kenv). *)
Definition K0 : kenv :=
  kenv_upd (kenv_upd fempty pk_ret CE_halt_return) pk_exn
    CE_halt_exn.

(* ---------------------------------------------------------------- *)
(* 5.1 The denotation fact: [[+]](1, 2; H) = (3, H)                 *)
(* ---------------------------------------------------------------- *)

Lemma fold_denot :
  forall Hh : heap,
    denot_prim (prim_op_of fold_prim)
      [V_naked_imm 1; V_naked_imm 2] Hh
      (PR_ok (V_naked_imm 3) Hh).
Proof.
  intro Hh.
  left.
  change (V_naked_imm 1) with (val_si SI_naked_immediate 1).
  change (V_naked_imm 2) with (val_si SI_naked_immediate 2).
  change (V_naked_imm 3)
    with (val_si SI_naked_immediate
            (int_arith_op_sem (si_width SI_naked_immediate)
               BIA_add 1 2)).
  apply P_Binary_IntArith_Total.
  - unfold in_width, si_width, machine_width.  cbn.  lia.
  - unfold in_width, si_width, machine_width.  cbn.  lia.
  - exact I.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.2 The Simplify side: rw_prim / rewrites / simplifies           *)
(* ---------------------------------------------------------------- *)

(* A constant simple proves equal to itself whenever it is its own
   canonical (T.Env.Canonical.Least: constants always are, in any
   real E; stated as a hypothesis because nothing else about E is
   needed). *)
Lemma proven_const_self :
  forall (E : tenv) (k : kind) (c : const),
    canonical E (Simple_const c) = Simple_const c ->
    proven_const E k (Simple_const c) c.
Proof.
  intros E k c Hcan.
  unfold proven_const, type_simple_in_term.
  cbn [fst].
  rewrite Hcan.
  eapply PES_alias.
  - destruct k as [| n | |]; try destruct n; reflexivity.
  - destruct k as [| n | |]; try destruct n; reflexivity.
  - exact Hcan.
  - left.  reflexivity.
Qed.

(* The prim_arg_kinds hypothesis is one row of the sanctioned
   arg-kind-table oracle (RewritesPrim.v's Parameter): the ch. 05
   row for naked-immediate binary arithmetic. *)
Lemma pilot_constfold_rw_prim :
  forall (fl : eff_flags) (C : code_env) (E : tenv),
    canonical E (Simple_const c_one) = Simple_const c_one ->
    canonical E (Simple_const c_two) = Simple_const c_two ->
    prim_arg_kinds (prim_op_of fold_prim)
      [K_naked_immediate; K_naked_immediate] ->
    rw_prim fl C E body_prim body_folded.
Proof.
  intros fl C E Hc1 Hc2 Hks.
  unfold body_prim, body_folded.
  apply S_Rewrite_Prim_ConstFold with
      (ks := [K_naked_immediate; K_naked_immediate])
      (cs := [c_one; c_two])
      (vs := [V_naked_imm 1; V_naked_imm 2])
      (v := V_naked_imm 3).
  - reflexivity.
  - reflexivity.
  - exact Hks.
  - cbn.
    constructor.
    + apply proven_const_self.  exact Hc1.
    + constructor.
      * apply proven_const_self.  exact Hc2.
      * constructor.
  - constructor.
    + reflexivity.
    + constructor.
      * reflexivity.
      * constructor.
  - intro Hh.  exact (fold_denot Hh).
  - reflexivity.
Qed.

Lemma pilot_constfold_simplifies :
  forall (fl : eff_flags) (C : code_env) (E : tenv),
    canonical E (Simple_const c_one) = Simple_const c_one ->
    canonical E (Simple_const c_two) = Simple_const c_two ->
    prim_arg_kinds (prim_op_of fold_prim)
      [K_naked_immediate; K_naked_immediate] ->
    simplifies fl C E body_prim body_folded.
Proof.
  intros fl C E Hc1 Hc2 Hks.
  apply S_step.
  apply RW_prim.
  apply pilot_constfold_rw_prim; assumption.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.3 Determinism at the two reachable configurations              *)
(* ---------------------------------------------------------------- *)

(* The halting configuration steps nowhere: pk_ret is bound to
   CE_halt_return, which no handler_entry or boundary entry
   matches. *)
Lemma halt_no_step :
  forall (flags : eff_flags) (rho : env) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) lbl c',
    fl_step flags
      (mk_config (Ctl_expr halt_expr) rho K0 Hh Tt Rr) lbl c' ->
    False.
Proof.
  intros flags rho Hh Tt Rr lbl c' Hstep.
  unfold fl_step, halt_expr in Hstep.
  inversion Hstep; subst;
  repeat match goal with
  | Ha : at_apply_cont _ _ _ _ _ |- _ =>
      inversion Ha; subst; clear Ha
  end;
  repeat match goal with
  | He : handler_entry _ _ _ _ _ _ _ |- _ =>
      inversion He; subst; clear He
  end;
  match goal with
  | Hk : K0 _ = Some _ |- _ => cbv in Hk; discriminate Hk
  end.
Qed.

(* The one step out of body_prim: OS.Let.Prim.Pure fires (the
   Effect classification is refuted by ch. 06's EC_pure row; the
   denotation is pinned by inversion through the union to
   P.Binary.IntArith.Total, which leaves the heap and region stack
   untouched). *)
Lemma step_from_prim :
  forall (flags : eff_flags) (rho : env) (K : kenv) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) lbl c',
    fl_step flags
      (mk_config (Ctl_expr body_prim) rho K Hh Tt Rr) lbl c' ->
    lbl = L_tau /\
    exists v,
      c' = mk_config (Ctl_expr halt_expr)
             (env_upd_var rho pv_x v) K Hh Tt Rr.
Proof.
  intros flags rho K Hh Tt Rr lbl c' Hstep.
  unfold fl_step, body_prim in Hstep.
  inversion Hstep; subst;
  try match goal with
  | Ha : at_apply_cont _ _ _ _ _ |- _ => inversion Ha
  end.
  - (* OS_Let_Prim_Pure *)
    match goal with
    | Hd : denot_R _ _ _ _ _ _ _ |- _ => inversion Hd; subst
    end.
    + (* DR_plain_ok *)
      match goal with
      | Hp : denot_prim _ _ _ _ |- _ =>
          destruct Hp as [Hsc | [Hma | Hmb]]
      end.
      * cbn in Hsc.  inversion Hsc; subst.
        split; [reflexivity | eexists; reflexivity].
      * cbn in Hma.  inversion Hma.
      * cbn in Hmb.  inversion Hmb.
    + (* DR_region *)
      match goal with
      | Hr : denot_prim_r _ _ _ _ _ |- _ =>
          unfold denot_prim_r in Hr; cbn in Hr; inversion Hr
      end.
  - (* OS_Let_Prim_Effect *)
    exfalso.
    match goal with
    | Hef : fl_effectful_prim _ _ |- _ =>
        unfold fl_effectful_prim in Hef; cbv in Hef;
        discriminate Hef
    end.
Qed.

(* The one step out of body_folded: OS.Let.Simple. *)
Lemma step_from_folded :
  forall (flags : eff_flags) (rho : env) (K : kenv) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) lbl c',
    fl_step flags
      (mk_config (Ctl_expr body_folded) rho K Hh Tt Rr) lbl c' ->
    lbl = L_tau /\
    c' = mk_config (Ctl_expr halt_expr)
           (env_upd_var rho pv_x (V_naked_imm 3)) K Hh Tt Rr.
Proof.
  intros flags rho K Hh Tt Rr lbl c' Hstep.
  unfold fl_step, body_folded in Hstep.
  inversion Hstep; subst;
  try match goal with
  | Ha : at_apply_cont _ _ _ _ _ |- _ => inversion Ha
  end.
  (* OS_Let_Simple *)
  match goal with
  | Hv : simple_eval _ _ = Some _ |- _ =>
      cbn in Hv; inversion Hv; subst
  end.
  split; reflexivity.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.4 Run inventories: everything reachable, with an empty trace   *)
(* ---------------------------------------------------------------- *)

Lemma steps_from_prim :
  forall (flags : eff_flags) (rho : env) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) tr cf,
    steps (fl_step flags)
      (mk_config (Ctl_expr body_prim) rho K0 Hh Tt Rr) tr cf ->
    tr = [] /\
    (cf = mk_config (Ctl_expr body_prim) rho K0 Hh Tt Rr \/
     exists v,
       cf = mk_config (Ctl_expr halt_expr)
              (env_upd_var rho pv_x v) K0 Hh Tt Rr).
Proof.
  intros flags rho Hh Tt Rr tr cf Hsteps.
  inversion Hsteps; subst; clear Hsteps.
  - split; [reflexivity | left; reflexivity].
  - (* Steps_tau *)
    match goal with
    | Hs : fl_step _ _ L_tau _ |- _ =>
        apply step_from_prim in Hs;
        destruct Hs as [_ [v Hc2]]
    end.
    subst.
    match goal with
    | Hr : steps _ _ _ _ |- _ => inversion Hr; subst; clear Hr
    end.
    + split; [reflexivity | right; eexists; reflexivity].
    + exfalso.  eapply halt_no_step; eassumption.
    + exfalso.  eapply halt_no_step; eassumption.
  - (* Steps_event *)
    match goal with
    | Hs : fl_step _ _ (L_event _) _ |- _ =>
        apply step_from_prim in Hs;
        destruct Hs as [Hl _]; discriminate Hl
    end.
Qed.

Lemma steps_from_folded :
  forall (flags : eff_flags) (rho : env) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) tr cf,
    steps (fl_step flags)
      (mk_config (Ctl_expr body_folded) rho K0 Hh Tt Rr) tr cf ->
    tr = [] /\
    (cf = mk_config (Ctl_expr body_folded) rho K0 Hh Tt Rr \/
     cf = mk_config (Ctl_expr halt_expr)
            (env_upd_var rho pv_x (V_naked_imm 3)) K0 Hh Tt Rr).
Proof.
  intros flags rho Hh Tt Rr tr cf Hsteps.
  inversion Hsteps; subst; clear Hsteps.
  - split; [reflexivity | left; reflexivity].
  - match goal with
    | Hs : fl_step _ _ L_tau _ |- _ =>
        apply step_from_folded in Hs;
        destruct Hs as [_ Hc2]
    end.
    subst.
    match goal with
    | Hr : steps _ _ _ _ |- _ => inversion Hr; subst; clear Hr
    end.
    + split; [reflexivity | right; reflexivity].
    + exfalso.  eapply halt_no_step; eassumption.
    + exfalso.  eapply halt_no_step; eassumption.
  - match goal with
    | Hs : fl_step _ _ (L_event _) _ |- _ =>
        apply step_from_folded in Hs;
        destruct Hs as [Hl _]; discriminate Hl
    end.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.5 Behavior-set uniqueness: the singleton {Beh_return [] H0}    *)
(* ---------------------------------------------------------------- *)

Lemma prim_not_undef :
  forall (rho : env) (Hh : heap) (Tt : trap_stack)
         (Rr : region_stack),
    fl_undef_next
      (mk_config (Ctl_expr body_prim) rho K0 Hh Tt Rr) ->
    False.
Proof.
  intros rho Hh Tt Rr Hun.
  unfold fl_undef_next, body_prim in Hun.
  inversion Hun; subst.
  match goal with
  | Hd : denot_R _ _ _ _ _ _ PRr_undef |- _ =>
      inversion Hd; subst; clear Hd
  end.
  - match goal with
    | Hp : denot_prim _ _ _ _ |- _ =>
        destruct Hp as [Hsc | [Hma | Hmb]];
        [ cbn in Hsc; inversion Hsc
        | cbn in Hma; inversion Hma
        | cbn in Hmb; inversion Hmb ]
    end.
  - match goal with
    | Hr : denot_prim_r _ _ _ _ _ |- _ =>
        unfold denot_prim_r in Hr; cbn in Hr; inversion Hr
    end.
Qed.

Lemma folded_not_undef :
  forall (rho : env) (Hh : heap) (Tt : trap_stack)
         (Rr : region_stack),
    fl_undef_next
      (mk_config (Ctl_expr body_folded) rho K0 Hh Tt Rr) ->
    False.
Proof.
  intros rho Hh Tt Rr Hun.
  unfold fl_undef_next, body_folded in Hun.
  inversion Hun.
Qed.

Lemma halt_not_undef :
  forall (rho : env) (Hh : heap) (Tt : trap_stack)
         (Rr : region_stack),
    fl_undef_next
      (mk_config (Ctl_expr halt_expr) rho K0 Hh Tt Rr) ->
    False.
Proof.
  intros rho Hh Tt Rr Hun.
  unfold fl_undef_next, halt_expr in Hun.
  inversion Hun.
Qed.

Lemma prim_can_step :
  forall (flags : eff_flags) (rho : env) (K : kenv) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack),
    fl_step flags
      (mk_config (Ctl_expr body_prim) rho K Hh Tt Rr) L_tau
      (mk_config (Ctl_expr halt_expr)
         (env_upd_var rho pv_x (V_naked_imm 3)) K Hh Tt Rr).
Proof.
  intros flags rho K Hh Tt Rr.
  unfold fl_step, body_prim.
  eapply OS_Let_Prim_Pure.
  - unfold fl_pure_prim.  cbv.  exact I.
  - reflexivity.
  - apply DR_plain_ok.  apply fold_denot.
Qed.

Lemma folded_can_step :
  forall (flags : eff_flags) (rho : env) (K : kenv) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack),
    fl_step flags
      (mk_config (Ctl_expr body_folded) rho K Hh Tt Rr) L_tau
      (mk_config (Ctl_expr halt_expr)
         (env_upd_var rho pv_x (V_naked_imm 3)) K Hh Tt Rr).
Proof.
  intros flags rho K Hh Tt Rr.
  unfold fl_step, body_folded.
  eapply OS_Let_Simple.
  reflexivity.
Qed.

Lemma prim_behavior_unique :
  forall (flags : eff_flags) (rho : env) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) b,
    fl_has_behavior flags
      (mk_config (Ctl_expr body_prim) rho K0 Hh Tt Rr) b ->
    b = Beh_return [] Hh.
Proof.
  intros flags rho Hh Tt Rr b Hb.
  unfold fl_has_behavior in Hb.
  inversion Hb; subst; clear Hb.
  - (* HB_return *)
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_prim in Hs;
        destruct Hs as [-> [Hcf | [v Hcf]]];
        inversion Hcf; subst; clear Hcf
    end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold body_prim in Ha; inversion Ha
      end.
    + reflexivity.
  - (* HB_exn *)
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_prim in Hs;
        destruct Hs as [-> [Hcf | [v Hcf]]];
        inversion Hcf; subst; clear Hcf
    end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold body_prim in Ha; inversion Ha
      end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold halt_expr in Ha; inversion Ha; subst; clear Ha
      end.
      match goal with
      | Hk : K0 _ = Some CE_halt_exn |- _ =>
          cbv in Hk; discriminate Hk
      end.
  - (* HB_diverge *)
    exfalso.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_prim in Hs;
        destruct Hs as [_ [Hcf | [v Hcf]]]; subst
    end.
    + match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      match goal with
      | Hs : step _ _ _ _ _ _ L_tau _ |- _ =>
          apply step_from_prim in Hs;
          destruct Hs as [_ [v Hc2]]
      end.
      subst.
      match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      eapply halt_no_step; eassumption.
    + match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      eapply halt_no_step; eassumption.
  - (* HB_react *)
    exfalso.
    match goal with
    | Hre : reacts _ _ _ |- _ => inversion Hre; subst; clear Hre
    end.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_prim in Hs;
        destruct Hs as [_ [Hcf | [v Hcf]]]; subst
    end.
    + match goal with
      | He : step _ _ _ _ _ _ (L_event _) _ |- _ =>
          apply step_from_prim in He;
          destruct He as [Hl _]; discriminate Hl
      end.
    + eapply halt_no_step; eassumption.
  - (* HB_undef *)
    exfalso.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_prim in Hs;
        destruct Hs as [_ [Hcf | [v Hcf]]]; subst
    end.
    + match goal with
      | Hu : _ \/ _ |- _ => destruct Hu as [Hun | Hstuck]
      end.
      * eapply prim_not_undef.  exact Hun.
      * destruct Hstuck as [_ Hns].
        eapply Hns.  eapply prim_can_step.
    + match goal with
      | Hu : _ \/ _ |- _ => destruct Hu as [Hun | Hstuck]
      end.
      * eapply halt_not_undef.  exact Hun.
      * destruct Hstuck as [Hnf _].
        apply Hnf.
        unfold halt_expr.
        eapply Final_return.
        -- eapply AAC_expr.  reflexivity.
        -- reflexivity.
Qed.

Lemma folded_behavior_unique :
  forall (flags : eff_flags) (rho : env) (Hh : heap)
         (Tt : trap_stack) (Rr : region_stack) b,
    fl_has_behavior flags
      (mk_config (Ctl_expr body_folded) rho K0 Hh Tt Rr) b ->
    b = Beh_return [] Hh.
Proof.
  intros flags rho Hh Tt Rr b Hb.
  unfold fl_has_behavior in Hb.
  inversion Hb; subst; clear Hb.
  - (* HB_return *)
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_folded in Hs;
        destruct Hs as [-> [Hcf | Hcf]];
        inversion Hcf; subst; clear Hcf
    end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold body_folded in Ha; inversion Ha
      end.
    + reflexivity.
  - (* HB_exn *)
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_folded in Hs;
        destruct Hs as [-> [Hcf | Hcf]];
        inversion Hcf; subst; clear Hcf
    end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold body_folded in Ha; inversion Ha
      end.
    + exfalso.
      match goal with
      | Ha : at_apply_cont _ _ _ _ _ |- _ =>
          unfold halt_expr in Ha; inversion Ha; subst; clear Ha
      end.
      match goal with
      | Hk : K0 _ = Some CE_halt_exn |- _ =>
          cbv in Hk; discriminate Hk
      end.
  - (* HB_diverge *)
    exfalso.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_folded in Hs;
        destruct Hs as [_ [Hcf | Hcf]]; subst
    end.
    + match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      match goal with
      | Hs : step _ _ _ _ _ _ L_tau _ |- _ =>
          apply step_from_folded in Hs;
          destruct Hs as [_ Hc2]
      end.
      subst.
      match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      eapply halt_no_step; eassumption.
    + match goal with
      | Hd : diverges_silently _ _ |- _ =>
          inversion Hd; subst; clear Hd
      end.
      eapply halt_no_step; eassumption.
  - (* HB_react *)
    exfalso.
    match goal with
    | Hre : reacts _ _ _ |- _ => inversion Hre; subst; clear Hre
    end.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_folded in Hs;
        destruct Hs as [_ [Hcf | Hcf]]; subst
    end.
    + match goal with
      | He : step _ _ _ _ _ _ (L_event _) _ |- _ =>
          apply step_from_folded in He;
          destruct He as [Hl _]; discriminate Hl
      end.
    + eapply halt_no_step; eassumption.
  - (* HB_undef *)
    exfalso.
    match goal with
    | Hs : steps _ _ _ _ |- _ =>
        apply steps_from_folded in Hs;
        destruct Hs as [_ [Hcf | Hcf]]; subst
    end.
    + match goal with
      | Hu : _ \/ _ |- _ => destruct Hu as [Hun | Hstuck]
      end.
      * eapply folded_not_undef.  exact Hun.
      * destruct Hstuck as [_ Hns].
        eapply Hns.  eapply folded_can_step.
    + match goal with
      | Hu : _ \/ _ |- _ => destruct Hu as [Hun | Hstuck]
      end.
      * eapply halt_not_undef.  exact Hun.
      * destruct Hstuck as [Hnf _].
        apply Hnf.
        unfold halt_expr.
        eapply Final_return.
        -- eapply AAC_expr.  reflexivity.
        -- reflexivity.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.6 Behavior existence (as in sections 2-4)                      *)
(* ---------------------------------------------------------------- *)

Theorem pilot_constfold_prim_behavior :
  forall (flags : eff_flags) (rho_pre : env) (H0 : heap),
    (forall x, env_get_var rho_pre x = None) ->
    fl_unit_behavior flags unit_prim rho_pre H0 (Beh_return [] H0).
Proof.
  intros flags rho_pre H0 Hno.
  unfold fl_unit_behavior.
  eexists.  split.
  - exact (@Init unit_prim rho_pre H0 (Mk_region_handle 0%nat) Hno).
  - unfold fl_has_behavior.
    eapply HB_return.
    + eapply Steps_tau.
      * cbn.  eapply OS_Let_Prim_Pure.
        -- unfold fl_pure_prim.  cbv.  exact I.
        -- reflexivity.
        -- apply DR_plain_ok.  apply fold_denot.
      * apply Steps_refl.
    + cbn.  eapply AAC_expr.  reflexivity.
    + reflexivity.
Qed.

Theorem pilot_constfold_folded_behavior :
  forall (flags : eff_flags) (rho_pre : env) (H0 : heap),
    (forall x, env_get_var rho_pre x = None) ->
    fl_unit_behavior flags unit_folded rho_pre H0
      (Beh_return [] H0).
Proof.
  intros flags rho_pre H0 Hno.
  unfold fl_unit_behavior.
  eexists.  split.
  - exact (@Init unit_folded rho_pre H0 (Mk_region_handle 0%nat)
             Hno).
  - unfold fl_has_behavior.
    eapply HB_return.
    + eapply Steps_tau.
      * cbn.  eapply OS_Let_Simple.  reflexivity.
      * apply Steps_refl.
    + cbn.  eapply AAC_expr.  reflexivity.
    + reflexivity.
Qed.

(* ---------------------------------------------------------------- *)
(* 5.7 The obs_equiv conclusion, proved outright                    *)
(* ---------------------------------------------------------------- *)

(* DISCOVERY (increment 2): beh_sim observes the final heap at
   sym_mod through the bijection (Bsim_return seeds b at
   Addr_sym sym_mod, and heap_sim requires every b-related address
   populated on both sides).  The doc's "same final module block
   value at sym_mod" presupposes the unit defines its module
   block; these pilot units never store one, so the equivalence is
   stated from a starting heap that already populates psym -- the
   smallest such heap. *)
Definition H_mod : heap :=
  fun hk =>
    match hk with
    | HK_addr (Addr_sym (Mk_symbol O)) =>
        Some (HO_Block (Mk_tag 0%nat) Immutable [])
    | _ => None
    end.

(* The identity bijection on the one populated address. *)
Definition bij_mod : address -> address -> Prop :=
  fun a a' => a = Addr_sym psym /\ a' = Addr_sym psym.

Lemma bij_mod_addr_bij : addr_bij bij_mod.
Proof.
  constructor.
  - intros a a1 a2 Ha1 Ha2.
    destruct Ha1 as [_ ->].  destruct Ha2 as [_ ->].
    reflexivity.
  - intros a1 a2 a Ha1 Ha2.
    destruct Ha1 as [-> _].  destruct Ha2 as [-> _].
    reflexivity.
  - intros s a Hb.  destruct Hb as [Hs ->].
    exact (eq_sym Hs).
  - intros a s Hb.  destruct Hb as [-> Hs].
    exact (eq_sym Hs).
Qed.

Lemma bij_mod_pins : pins H_mod bij_mod.
Proof.
  intros a o Hget.
  destruct a as [l | s].
  - cbv in Hget.  discriminate Hget.
  - destruct s as [n].  destruct n as [| n'].
    + split; reflexivity.
    + cbv in Hget.  discriminate Hget.
Qed.

Lemma beh_sim_mod_return :
  beh_sim psym H_mod (Beh_return [] H_mod) (Beh_return [] H_mod).
Proof.
  apply Bsim_return with (b := bij_mod).
  - exact bij_mod_addr_bij.
  - exact bij_mod_pins.
  - split; reflexivity.
  - constructor.
  - intros a a' Hb.  destruct Hb as [-> ->].
    exists (HO_Block (Mk_tag 0%nat) Immutable []).
    exists (HO_Block (Mk_tag 0%nat) Immutable []).
    split; [reflexivity |].
    split; [reflexivity |].
    constructor.
    constructor.
Qed.

(* The micro-instance of INV.Simplify.Preserves: the conclusion of
   the headline theorem, proved outright for the concrete
   const-fold pair.  Both units' behavior sets are the singleton
   {Beh_return [] H_mod} (5.4-5.6), and the singleton is
   self-similar under bij_mod (5.7).  Together with
   pilot_constfold_simplifies (5.2) this exhibits, end to end, one
   derivable Simplify step whose obs_equiv conclusion is a
   theorem. *)
Theorem pilot_constfold_preserves :
  forall (flags : eff_flags) (rho_pre : env),
    (forall x, env_get_var rho_pre x = None) ->
    obs_equiv psym H_mod
      (fl_unit_behavior flags unit_prim rho_pre H_mod)
      (fl_unit_behavior flags unit_folded rho_pre H_mod).
Proof.
  intros flags rho_pre Hno.
  split.
  - intros b1 Hb1.
    exists (Beh_return [] H_mod).
    split.
    + apply pilot_constfold_folded_behavior.  exact Hno.
    + destruct Hb1 as (c0 & Hinit & Hbeh).
      inversion Hinit; subst.
      apply prim_behavior_unique in Hbeh.
      subst b1.
      exact beh_sim_mod_return.
  - intros b2 Hb2.
    exists (Beh_return [] H_mod).
    split.
    + apply pilot_constfold_prim_behavior.  exact Hno.
    + destruct Hb2 as (c0 & Hinit & Hbeh).
      inversion Hinit; subst.
      apply folded_behavior_unique in Hbeh.
      subst b2.
      exact beh_sim_mod_return.
Qed.
