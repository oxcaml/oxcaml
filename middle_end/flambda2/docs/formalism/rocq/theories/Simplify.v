(* Simplify.v -- the rewrite judgment's union and its congruent
   closure; the whole-run relation (task #23, wave 6).
   Owner: Scott.
   Imports: Base, Syntax, TypeGrammar, PrimMemoryA, MeetJoin,
     SimplifyStructure, RewritesPrim, RewritesControl, Inlining,
     Unboxing.

   Contents:
   - Section 1: rewrites, the union of the four rewrite families
     (the doc's one-step judgment E |- e ~> e').
   - Section 2: simplifies, the congruent reflexive-transitive
     closure (one congruence constructor per expression-position
     frame of Syntax.v's mutual block), defined mutually with the
     per-code-binding rebuild pipeline simplify_code (loopify wrap,
     scope-stopped self-tail-call redirection, final recursive-flag
     recompute -- the two W-24 composition contracts from the
     RewritesControl.v review), and with the CSE composition
     (table-threading traversal cse_deep over RewritesPrim.v's
     rw_cse / cse_extend side judgments).
   - Section 3: simplify_unit, the whole-run relation (model
     counterpart of Simplify.run, per the S.Struct.Run comment in
     SimplifyStructure.v), conjoining the ch. 09 output contract.
   - Section 4: pilot lemmas.

   This file hosts no rule ids: the ch. 10/11/12 rewrite rules live
   in the four imported rw_* files, and INV.Rewrite.Local -- stated
   over one-hole contexts and observational equivalence -- lives
   with that apparatus in Soundness.v (ownership moved from this
   file's original brief; coordinator informed). *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax TypeGrammar PrimMemoryA.
From Flambda2 Require Import MeetJoin SimplifyStructure.
From Flambda2 Require Import RewritesPrim RewritesControl Inlining
  Unboxing.
Import ListNotations.

(* ================================================================== *)
(* 1. The rewrite union                                               *)
(* ================================================================== *)

(* The doc's one-step rewrite judgment E |- e ~> e' (ch. 10 intro;
   13-soundness.md section 2 quantifies over "a rule in S.Rewrite.*,
   S.Inline.*, S.Unbox.*"): the union of the four families.  Each
   arm is opaque here (rw_unbox has two constructors; no arm's
   constructor inventory is relied upon anywhere in this file).
   The side judgments rw_code_loopify / rw_self_tail_call
   (RewritesControl.v) are deliberately NOT arms: they are not root
   rewrites on expressions -- rw_code_loopify rebuilds a code0 and
   rw_self_tail_call needs the ambient loopify state -- so they are
   composed by simplify_code below, per RewritesControl.v's own
   sec. 7 comments delegating that composition here (W-24).
   Likewise rw_cse / cse_extend (RewritesPrim.v sec. 5): the CSE
   table is per-position denv state the frozen arm type cannot
   carry; they are composed by cse_deep below. *)
Inductive rewrites (fl : eff_flags) (C : code_env) (E : tenv)
  : expr -> expr -> Prop :=
  | RW_prim : forall e e',
      rw_prim fl C E e e' -> rewrites fl C E e e'
  | RW_control : forall e e',
      rw_control fl C E e e' -> rewrites fl C E e e'
  | RW_inline : forall e e',
      rw_inline fl C E e e' -> rewrites fl C E e e'
  | RW_unbox : forall e e',
      rw_unbox fl C E e e' -> rewrites fl C E e e'.

(* ================================================================== *)
(* 2. The congruent reflexive-transitive closure                      *)
(* ================================================================== *)

(* Rebuild a code0 with a new body (all other fields preserved). *)
Definition code0_with_body (c : code0) (b : expr) : code0 :=
  match c with
  | Mk_code0 rk ek ps _ mc md am pa ra rm tup =>
      Mk_code0 rk ek ps b mc md am pa ra rm tup
  end.

(* One self-tail-call redirection somewhere inside a loopified
   function body: the congruence closure of rw_self_tail_call
   (RewritesControl.v sec. 7), which that file delegates here.
   The traversal is scope-stopped (W-24 contract (a)): descent past
   a continuation binder that rebinds k, kret or kexn is disallowed
   -- k because the emitted apply_cont k would be captured by the
   inner binder, kret/kexn because the rewrite's premises compare
   those continuation names syntactically (continuations are not
   E-mediated).  A nonrec handler does not see its own binder, so
   the handler frame is unconditional.  Rebinding of my_closure is
   NOT stopped: the my_closure premise reads names through the fixed
   E like every other rw_* premise (the closure-wide fixed-E
   convention in the simplifies comment below).  There is
   deliberately no code-body frame: an inner function has its own
   loopify state (the pass resets loopify_state per function body),
   so redirects never cross into nested code. *)
Inductive stc_deep (C : code_env) (E : tenv) (k : continuation)
    (mc : variable) (kret kexn : continuation)
  : expr -> expr -> Prop :=
  | STC_here : forall e e',
      rw_self_tail_call C E k mc kret kexn e e' ->
      stc_deep C E k mc kret kexn e e'
  | STC_let_body : forall p d b b',
      stc_deep C E k mc kret kexn b b' ->
      stc_deep C E k mc kret kexn (E_let p d b) (E_let p d b')
  | STC_cont_handler : forall k0 ps h h' exn cold b,
      stc_deep C E k mc kret kexn h h' ->
      stc_deep C E k mc kret kexn
        (E_let_cont_nonrec k0 (Mk_cont_handler ps h exn cold) b)
        (E_let_cont_nonrec k0 (Mk_cont_handler ps h' exn cold) b)
  | STC_cont_body : forall k0 h b b',
      k0 <> k -> k0 <> kret -> k0 <> kexn ->
      stc_deep C E k mc kret kexn b b' ->
      stc_deep C E k mc kret kexn
        (E_let_cont_nonrec k0 h b) (E_let_cont_nonrec k0 h b')
  | STC_cont_rec_handler :
      forall inv g1 g2 k0 ps h h' exn cold b,
      (forall k1 h1,
         In (k1, h1)
           (g1 ++ (k0, Mk_cont_handler ps h exn cold) :: g2) ->
         k1 <> k /\ k1 <> kret /\ k1 <> kexn) ->
      stc_deep C E k mc kret kexn h h' ->
      stc_deep C E k mc kret kexn
        (E_let_cont_rec inv
           (g1 ++ (k0, Mk_cont_handler ps h exn cold) :: g2) b)
        (E_let_cont_rec inv
           (g1 ++ (k0, Mk_cont_handler ps h' exn cold) :: g2) b)
  | STC_cont_rec_body : forall inv hs b b',
      (forall k1 h1,
         In (k1, h1) hs ->
         k1 <> k /\ k1 <> kret /\ k1 <> kexn) ->
      stc_deep C E k mc kret kexn b b' ->
      stc_deep C E k mc kret kexn
        (E_let_cont_rec inv hs b) (E_let_cont_rec inv hs b').

(* The CSE composition (interface flagged by RewritesPrim.v sec. 5):
   the table is per-position denv state threaded DOWN the traversal,
   so like the loopify pipeline it cannot be a union arm.  Unlike
   tenv_extends (see the fixed-E note below), cse_extend validates
   each entry against the defining expression it descends past, so
   threading is sound: in any table reachable from fempty, every
   entry records an actual dominating binding.  cse_deep performs
   ONE replacement at the end of a descent path that accumulates the
   table through the lets it crosses; repeated replacement is
   recovered by refiring S_cse through S_trans (each firing
   re-accumulates from fempty at its node, so firing at an ancestor
   recovers the pass's real table lineage).  Skipping an available
   extension is allowed (the CSE_let frame): a smaller table only
   removes replacement opportunities -- sound side.  Handler frames
   thread the table unchanged: entries name let-bound variables of
   enclosing scope, valid inside handlers; the code's
   by_scope/combined join-point filtering is dacc machinery outside
   the model (RewritesPrim.v's ENCODING NOTE there), and parameter
   shadowing of a mapped name falls under the closure-wide fixed-E
   name-semantics convention.  No code-body frame: denv and its
   table are rebuilt per function body, and outer let-bound names
   are not in scope inside code. *)
Inductive cse_deep (fl : eff_flags) (C : code_env) (E : tenv)
  : cse_table -> expr -> expr -> Prop :=
  | CSE_here : forall tbl e e',
      rw_cse fl C tbl E e e' ->
      cse_deep fl C E tbl e e'
  | CSE_let_extend : forall tbl tbl' p d b b',
      cse_extend fl E tbl (E_let p d b) tbl' ->
      cse_deep fl C E tbl' b b' ->
      cse_deep fl C E tbl (E_let p d b) (E_let p d b')
  | CSE_let : forall tbl p d b b',
      cse_deep fl C E tbl b b' ->
      cse_deep fl C E tbl (E_let p d b) (E_let p d b')
  | CSE_cont_handler : forall tbl k ps h h' exn cold b,
      cse_deep fl C E tbl h h' ->
      cse_deep fl C E tbl
        (E_let_cont_nonrec k (Mk_cont_handler ps h exn cold) b)
        (E_let_cont_nonrec k (Mk_cont_handler ps h' exn cold) b)
  | CSE_cont_body : forall tbl k h b b',
      cse_deep fl C E tbl b b' ->
      cse_deep fl C E tbl
        (E_let_cont_nonrec k h b) (E_let_cont_nonrec k h b')
  | CSE_cont_rec_handler :
      forall tbl inv g1 g2 k ps h h' exn cold b,
      cse_deep fl C E tbl h h' ->
      cse_deep fl C E tbl
        (E_let_cont_rec inv
           (g1 ++ (k, Mk_cont_handler ps h exn cold) :: g2) b)
        (E_let_cont_rec inv
           (g1 ++ (k, Mk_cont_handler ps h' exn cold) :: g2) b)
  | CSE_cont_rec_body : forall tbl inv hs b b',
      cse_deep fl C E tbl b b' ->
      cse_deep fl C E tbl
        (E_let_cont_rec inv hs b) (E_let_cont_rec inv hs b').

(* simplifies: rewrites may fire at any expression position, any
   number of times, as the traversal rebuilds the term
   (13-soundness.md section 2: "Simplify's rewrites are applied
   compositionally as the traversal rebuilds the term").  The
   congruence constructors are one per expression-position frame of
   Syntax.v's mutual block -- let bodies, code bodies inside a
   static-consts binding, continuation-handler bodies (nonrec and
   rec), let-cont bodies -- the same frame inventory as ch. 13's
   one-hole contexts (Soundness.v).  Apply / apply_cont / switch /
   invalid have no expr subterm and contribute no frame.  A frame
   rewrites ONE subterm at a time; simultaneous and repeated
   rewriting is recovered through transitivity (see the pilot
   lemmas).
   ENCODING NOTE: the typing environment E is held FIXED through
   congruence.  The real traversal's environment evolves downward
   (S.Struct.TypesMonotoneDown / S.Struct.EnvRefineOnly; join-point
   envs per S.Struct.JoinParams), but that evolution is dacc
   machinery outside the model, and the descent lineage relation
   (tenv_descends, MeetJoin.v) cannot stand in for it here: its
   tenv_extends step applies an arbitrary recorded extension, with
   no validation against the defining term (that validation is the
   out-of-model machinery), so weaving it into the closure would
   admit derivations under unsound environments and falsify ch. 13's
   statements over this relation.  Holding E fixed under-
   approximates the pass (facts established deeper in the term are
   unavailable to deeper rewrites), on the sound side: E's facts
   persist into every descendant position, since refinement is
   monotone along scope.
   simplifies is mutual with loopify_simplifies and simplify_code
   below: the S_code_rebuild frame runs the full per-code rebuild
   pipeline at a code binding, whose loopified case in turn rewrites
   the function body with this closure plus redirects.
   S_cong_let_code remains alongside it as the plain one-subterm
   congruence into a code body (no rebuild metadata).  S_cse fires
   the CSE composition (cse_deep above) from the EMPTY table -- the
   only sound start, since table validity is exactly reachability
   from fempty by descent.
   ENCODING NOTE (S_code_rebuild): the loopify attribute a is
   closure-conversion metadata not stored on the frozen code0
   (coordinator ruling: no resurrected cache fields), so the frame
   quantifies it -- the relation admits a rebuild under ANY
   attribute, each of which is a sound rewrite; the frontend-side
   derivation of the intended attribute is
   S_Rewrite_Loopify_Attribute (RewritesControl.v), conjoinable by a
   consumer that models the from_lambda boundary.  The emitted
   attribute a' and recursive flag r likewise have no term
   representation and are existential at this frame. *)
Inductive simplifies (fl : eff_flags) (C : code_env) (E : tenv)
  : expr -> expr -> Prop :=
  | S_refl : forall e,
      simplifies fl C E e e
  | S_trans : forall e1 e2 e3,
      simplifies fl C E e1 e2 ->
      simplifies fl C E e2 e3 ->
      simplifies fl C E e1 e3
  | S_step : forall e e',
      rewrites fl C E e e' ->
      simplifies fl C E e e'
  | S_cong_let_body : forall p d b b',
      simplifies fl C E b b' ->
      simplifies fl C E (E_let p d b) (E_let p d b')
  | S_cong_let_code :
      forall p g1 g2 rk ek ps bd bd' mc md am pa ra rm tup b,
      simplifies fl C E bd bd' ->
      simplifies fl C E
        (E_let p
           (N_static_consts
              (g1 ++ SCC_code
                 (Mk_code0 rk ek ps bd mc md am pa ra rm tup)
                 :: g2))
           b)
        (E_let p
           (N_static_consts
              (g1 ++ SCC_code
                 (Mk_code0 rk ek ps bd' mc md am pa ra rm tup)
                 :: g2))
           b)
  | S_cong_cont_handler : forall k ps h h' exn cold b,
      simplifies fl C E h h' ->
      simplifies fl C E
        (E_let_cont_nonrec k (Mk_cont_handler ps h exn cold) b)
        (E_let_cont_nonrec k (Mk_cont_handler ps h' exn cold) b)
  | S_cong_cont_body : forall k h b b',
      simplifies fl C E b b' ->
      simplifies fl C E
        (E_let_cont_nonrec k h b)
        (E_let_cont_nonrec k h b')
  | S_cong_cont_rec_handler :
      forall inv g1 g2 k ps h h' exn cold b,
      simplifies fl C E h h' ->
      simplifies fl C E
        (E_let_cont_rec inv
           (g1 ++ (k, Mk_cont_handler ps h exn cold) :: g2) b)
        (E_let_cont_rec inv
           (g1 ++ (k, Mk_cont_handler ps h' exn cold) :: g2) b)
  | S_cong_cont_rec_body : forall inv hs b b',
      simplifies fl C E b b' ->
      simplifies fl C E
        (E_let_cont_rec inv hs b)
        (E_let_cont_rec inv hs b')
  | S_code_rebuild :
      forall p g1 g2 a a' r c c' b,
      simplify_code fl C E a c a' r c' ->
      simplifies fl C E
        (E_let p (N_static_consts (g1 ++ SCC_code c :: g2)) b)
        (E_let p (N_static_consts (g1 ++ SCC_code c' :: g2)) b)
  | S_cse : forall e e',
      cse_deep fl C E fempty e e' ->
      simplifies fl C E e e'

(* Interleaving of plain simplification and scope-stopped
   self-tail-call redirection, inside a loopified function body
   (loopify_state = Loopify k; my_closure and the return/exn
   continuations come from the enclosing code). *)
with loopify_simplifies (fl : eff_flags) (C : code_env) (E : tenv)
  : continuation -> variable -> continuation -> continuation
    -> expr -> expr -> Prop :=
  | LS_plain : forall k mc kret kexn e e',
      simplifies fl C E e e' ->
      loopify_simplifies fl C E k mc kret kexn e e'
  | LS_redirect : forall k mc kret kexn e e',
      stc_deep C E k mc kret kexn e e' ->
      loopify_simplifies fl C E k mc kret kexn e e'
  | LS_trans : forall k mc kret kexn e1 e2 e3,
      loopify_simplifies fl C E k mc kret kexn e1 e2 ->
      loopify_simplifies fl C E k mc kret kexn e2 e3 ->
      loopify_simplifies fl C E k mc kret kexn e1 e3

(* The per-code rebuild pipeline (W-24): wrap (or not) via
   rw_code_loopify, rewrite the body, then re-run
   S_Rewrite_Code_RecursiveRecompute on the FINAL body for the
   emitted flag.  The r0 produced at wrap time is deliberately
   discarded (W-24 contract (b)): the wrap-time recompute always
   sees my_depth free in a genuinely self-recursive body, while the
   doc runs the recompute on the simplified body, after SelfTailCall
   redirections have dropped the rec_info coercions -- which is what
   makes a Non_recursive result (and hence Small_function inlining
   eligibility) reachable.
   The loopified case is staged inside-out, matching the pass: the
   redirect-bearing stage rewrites the handler body e0 -- under the
   wrap's own binding of k, the one binder redirects must NOT be
   stopped by -- and plain simplification then finishes on the whole
   wrapped body.  The wrap-shape premise extracts k, the handler and
   the entry jump from rw_code_loopify's conclusion without
   inverting it; on the should_loopify = true path only the
   S_Rewrite_Loopify_Body constructor can have produced c1, and its
   output body has exactly this shape. *)
with simplify_code (fl : eff_flags) (C : code_env) (E : tenv)
  : loopify_attribute -> code0 -> loopify_attribute
    -> recursiveness -> code0 -> Prop :=
  | SC_loopified : forall a c a' r0 c1 k ps e0 j e0' body' r,
      should_loopify a = true ->
      rw_code_loopify C E a c a' r0 c1 ->
      c0_body c1
        = E_let_cont_rec nil
            ((k, Mk_cont_handler ps e0 false false) :: nil) j ->
      loopify_simplifies fl C E k (c0_my_closure c1)
        (c0_return_continuation c1) (c0_exn_continuation c1)
        e0 e0' ->
      simplifies fl C E
        (E_let_cont_rec nil
           ((k, Mk_cont_handler ps e0' false false) :: nil) j)
        body' ->
      S_Rewrite_Code_RecursiveRecompute body' (c0_my_depth c1) r ->
      simplify_code fl C E a c a' r (code0_with_body c1 body')
  | SC_plain : forall a c a' r0 c1 body' r,
      should_loopify a = false ->
      rw_code_loopify C E a c a' r0 c1 ->
      simplifies fl C E (c0_body c1) body' ->
      S_Rewrite_Code_RecursiveRecompute body' (c0_my_depth c1) r ->
      simplify_code fl C E a c a' r (code0_with_body c1 body').

(* ================================================================== *)
(* 3. The whole-run relation                                          *)
(* ================================================================== *)

(* simplify_unit: the model counterpart of Simplify.run (the
   S.Struct.Run comment in SimplifyStructure.v points here).  The
   unit's body simplifies under the initial environment E0 (the env
   run builds from imports before traversal) and the unit's code
   table C; the unit's continuations, region parameters and module
   symbol are preserved; and the OUTPUT unit satisfies the ch. 09
   contract -- closed (S_Struct_Run_ClosedResult) and every static
   binding placed at toplevel (S_Struct_Run_NoPendingConstants /
   S_Struct_Lift_EmptyAtEnd).
   ENCODING NOTE: run's side results (free_names, final_typing_env,
   all_code, slot_offsets) are pass outputs consumed by later
   pipeline stages, not part of the unit-to-unit meaning; they are
   not modeled. *)
Inductive simplify_unit (fl : eff_flags) (C : code_env) (E0 : tenv)
  : flambda_unit -> flambda_unit -> Prop :=
  | SU_intro : forall rc ec r gr sym b b',
      simplifies fl C E0 b b' ->
      S_Struct_Run_ClosedResult
        (Mk_flambda_unit rc ec r gr sym b') ->
      S_Struct_Run_NoPendingConstants
        (Mk_flambda_unit rc ec r gr sym b') ->
      S_Struct_Lift_EmptyAtEnd
        (Mk_flambda_unit rc ec r gr sym b') ->
      simplify_unit fl C E0
        (Mk_flambda_unit rc ec r gr sym b)
        (Mk_flambda_unit rc ec r gr sym b').

(* ================================================================== *)
(* 4. Pilot lemmas                                                    *)
(* ================================================================== *)

(* A one-step rewrite lifts under a let body. *)
Lemma simplifies_step_in_let_body :
  forall fl C E p d b b',
    rewrites fl C E b b' ->
    simplifies fl C E (E_let p d b) (E_let p d b').
Proof.
  intros fl C E p d b b' Hrw.
  apply S_cong_let_body. apply S_step. assumption.
Qed.

(* Congruence frames nest: a rewrite inside a nonrec handler under a
   let. *)
Lemma simplifies_step_in_nested_handler :
  forall fl C E p d k ps h h' exn cold b,
    rewrites fl C E h h' ->
    simplifies fl C E
      (E_let p d
         (E_let_cont_nonrec k (Mk_cont_handler ps h exn cold) b))
      (E_let p d
         (E_let_cont_nonrec k (Mk_cont_handler ps h' exn cold) b)).
Proof.
  intros fl C E p d k ps h h' exn cold b Hrw.
  apply S_cong_let_body. apply S_cong_cont_handler.
  apply S_step. assumption.
Qed.

(* Group-wide handler simplification is derivable from the
   one-at-a-time recursive-group congruence through transitivity
   (two-handler instance). *)
Lemma simplifies_rec_group_two :
  forall fl C E inv k1 ps1 h1 h1' x1 c1 k2 ps2 h2 h2' x2 c2 b,
    simplifies fl C E h1 h1' ->
    simplifies fl C E h2 h2' ->
    simplifies fl C E
      (E_let_cont_rec inv
         ((k1, Mk_cont_handler ps1 h1 x1 c1)
          :: (k2, Mk_cont_handler ps2 h2 x2 c2) :: nil) b)
      (E_let_cont_rec inv
         ((k1, Mk_cont_handler ps1 h1' x1 c1)
          :: (k2, Mk_cont_handler ps2 h2' x2 c2) :: nil) b).
Proof.
  intros fl C E inv k1 ps1 h1 h1' x1 c1 k2 ps2 h2 h2' x2 c2 b H1 H2.
  apply S_trans with
    (e2 := E_let_cont_rec inv
       ((k1, Mk_cont_handler ps1 h1' x1 c1)
        :: (k2, Mk_cont_handler ps2 h2 x2 c2) :: nil) b).
  - apply (S_cong_cont_rec_handler fl C E inv nil
      ((k2, Mk_cont_handler ps2 h2 x2 c2) :: nil)
      k1 ps1 h1 h1' x1 c1 b).
    assumption.
  - apply (S_cong_cont_rec_handler fl C E inv
      ((k1, Mk_cont_handler ps1 h1' x1 c1) :: nil) nil
      k2 ps2 h2 h2' x2 c2 b).
    assumption.
Qed.

(* The no-loopify pipeline instance: attribute update and the final
   recursive-flag recompute go through with an unchanged body. *)
Lemma simplify_code_plain_refl :
  forall fl C E c r,
    S_Rewrite_Code_RecursiveRecompute (c0_body c) (c0_my_depth c)
      r ->
    simplify_code fl C E Never_loopify c Never_loopify r
      (code0_with_body c (c0_body c)).
Proof.
  intros fl C E c r Hr.
  eapply SC_plain.
  - reflexivity.
  - apply rw_code_no_loopify; [reflexivity | exact Hr].
  - apply S_refl.
  - exact Hr.
Qed.
