(* RewritesControl.v -- 10-simplify-rewrites.md, control-side rewrites
   (owner: Church).

   The control-side half of ch. 10: the rw_control relation over
   code_env and tenv covering Switch simplification, Let dead code,
   Let_cont
   inline/shortcut/param rules, Apply arity rewrites, loopification,
   and Invalid propagation (35 rules, everything from
   S.Rewrite.Switch.ArmPrune down).  The prim-side half (Prim.*,
   Alias.Canonicalize, CSE.*, Share.StaticDynamicSplit) is
   RewritesPrim.v (owner: Curry).

   Increment 2 (this file is now feature-complete for its 35 rules):
   the rw_control relation (secs. 8-9), the code-level loopification
   judgment rw_code_loopify and the in-body self-tail-call rewrite
   rw_self_tail_call (sec. 7; Simplify.v unions all of these), and
   the documented anchors (sec. 10).  Compiles only once MeetJoin.v
   exists: the premises below use its frozen names
   meet : tenv -> ftype -> ftype -> meet_res -> Prop (CORRESPONDENCE
   encoding table) and prove_single_closures_entry
   : tenv -> simple -> function_slot -> closures_entry -> Prop
   (prove_* family; reconcile at compile). *)

From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Syntax TypeGrammar PrimMemoryA
  MeetJoin.
Import ListNotations.

Local Open Scope nat_scope.

(* ================================================================== *)
(* 1. Counting free continuation uses                                 *)
(* ================================================================== *)

(* Ch. 10's Let_cont rules state premises over the USES of a
   continuation k in a term: "exactly one use, of kind Inlinable"
   (S.Rewrite.LetCont.Inline), "zero free occurrences ... counting
   excludes trap actions" (S.Rewrite.LetCont.DeadHandler).  The
   implementation reads these off the simplifier's recorded use lists
   and free-name sets; per the sanctioned elision of Let_cont's
   num_free_occurrences/is_applied_with_traps hints, we state them as
   syntactic counts over the term.

   A "use" of k is an occurrence as: the target of an apply_cont (in
   expression position or as a switch arm), or an Apply's
   return/exception continuation.  Occurrences INSIDE trap actions
   (Trap_push k / Trap_pop k) are NOT uses -- the doc's occurrence
   counting excludes trap actions.  Counting is scope-aware exactly
   like replace_apply_cont (Syntax.v): a rebinding of k (Let_cont or a
   code0's return/exn continuations) stops the count.

   The traversal is parameterized by per-position weight functions so
   the "all uses" and "inlinable uses" counters share one Fixpoint. *)

Section ContUseCounting.

Variable k : continuation.
(* Weight of an apply_cont in expression position. *)
Variable w_ac_expr : apply_cont_expr -> nat.
(* Weight of an apply_cont in switch-arm position. *)
Variable w_ac_arm : apply_cont_expr -> nat.
(* Weight of an Apply's result continuation. *)
Variable w_rc : result_continuation -> nat.
(* Weight of an Apply's exception continuation. *)
Variable w_ec : exn_continuation -> nat.

Fixpoint cont_uses (e : expr) : nat :=
  match e with
  | E_let _ n body => cu_named n + cont_uses body
  | E_let_cont_nonrec k' h body =>
      (* k' scopes over the body only (non-recursive). *)
      cu_handler h
      + (if continuation_eqb k' k then 0 else cont_uses body)
  | E_let_cont_rec _ handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then 0  (* k rebound over both body and handlers *)
      else
        (fix go (hs : list (continuation * cont_handler)) : nat :=
           match hs with
           | [] => 0
           | (_, h) :: tl => cu_handler h + go tl
           end) handlers
        + cont_uses body
  | E_apply ap =>
      w_rc (ap_result_continuation ap) + w_ec (ap_exn_continuation ap)
  | E_apply_cont ac => w_ac_expr ac
  | E_switch sw =>
      (fix go (arms : list (target_ocaml_int * apply_cont_expr)) : nat :=
         match arms with
         | [] => 0
         | (_, ac) :: tl => w_ac_arm ac + go tl
         end) (sw_arms sw)
  | E_invalid _ => 0
  end

with cu_named (n : named) : nat :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ => 0
  | N_static_consts scg =>
      (fix go (l : list static_const_or_code) : nat :=
         match l with
         | [] => 0
         | s :: tl => cu_scc s + go tl
         end) scg
  end

with cu_handler (h : cont_handler) : nat :=
  match h with Mk_cont_handler _ body _ _ => cont_uses body end

with cu_scc (s : static_const_or_code) : nat :=
  match s with
  | SCC_code c => cu_code c
  | SCC_deleted_code | SCC_static_const _ => 0
  end

with cu_code (c : code0) : nat :=
  match c with
  | Mk_code0 kret kexn _ body _ _ _ _ _ _ _ =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then 0
      else cont_uses body
  end.

End ContUseCounting.

(* Total number of free uses of k in e (trap actions excluded). *)
Definition count_cont_uses (k : continuation) (e : expr) : nat :=
  cont_uses k
    (fun ac => if continuation_eqb (ac_continuation ac) k then 1 else 0)
    (fun ac => if continuation_eqb (ac_continuation ac) k then 1 else 0)
    (fun rc =>
       match rc with
       | RC_return k' => if continuation_eqb k' k then 1 else 0
       | RC_never_returns => 0
       end)
    (fun ec =>
       if continuation_eqb (ec_exn_handler ec) k then 1 else 0)
    e.

(* Number of free INLINABLE uses of k in e: expression-position
   apply_conts targeting k that carry no trap action.  Switch-arm,
   trap-action-bearing, and Apply return/exn uses are Non_inlinable
   (simplify_common.ml#apply_cont_use_kind) and weigh zero here. *)
Definition count_inlinable_uses (k : continuation) (e : expr) : nat :=
  cont_uses k
    (fun ac =>
       if continuation_eqb (ac_continuation ac) k
       then match ac_trap_action ac with
            | None => 1
            | Some _ => 0
            end
       else 0)
    (fun _ => 0) (fun _ => 0) (fun _ => 0)
    e.

(* ================================================================== *)
(* 2. Substitution on simples and apply_cont retargeting              *)
(* ================================================================== *)

(* S.Rewrite.LetCont.Shortcut's conclusion substitutes the shortcut
   handler's argument list: apply_cont k bbar becomes
   apply_cont k' (abar[pbar |-> bbar]).  abar are simples over the
   parameters pbar; the substitution maps variables to simples. *)

Definition subst_map := fmap variable simple.

(* Composition of coercions, for substituting under a coerced name
   occurrence.  At the formalism's granularity the depth-change
   endpoints compose end-to-end. *)
Definition coercion_compose (c1 c2 : coercion) : coercion :=
  match c1, c2 with
  | Coercion_id, c => c
  | c, Coercion_id => c
  | Coercion_change_depth f _, Coercion_change_depth _ t =>
      Coercion_change_depth f t
  end.

Definition subst_simple (m : subst_map) (s : simple) : simple :=
  match s with
  | Simple_name (Name_var x) co =>
      match m x with
      | None => s
      | Some s' =>
          match s' with
          | Simple_name n' co' => Simple_name n' (coercion_compose co' co)
          | Simple_const c => Simple_const c
          end
      end
  | _ => s
  end.

(* pbar |-> bbar as a subst_map (parameters are unarized, so
   positional pairing is exact when the lengths agree -- the rules'
   arity premises guarantee that). *)
Definition param_subst
    (ps : list (variable * kind_ws)) (bs : list simple) : subst_map :=
  fupd_list variable_eqb fempty (combine (map fst ps) bs).

(* Retargeting: rewrite every free apply_cont targeting k -- in
   expression position AND in switch arms, with or without a site
   trap action -- to the apply_cont produced by [retarget] from the
   site's argument list, PRESERVING the site's trap action.  This is
   the substitution of S.Rewrite.LetCont.Shortcut, which differs from
   replace_apply_cont (Syntax.v) exactly per the rule's NOTES: "site
   trap actions survive retargeting (the shortcut condition is on the
   HANDLER's apply_cont having no trap action, not the site's)", and
   shortcut application also rewrites switch-arm uses (the arms-level
   variant anticipated when replace_apply_cont was scoped to
   expression positions).  Scope-aware like replace_apply_cont. *)

Section RetargetApplyCont.

Variable k : continuation.
(* Given the site's argument list bbar, the replacement jump
   (trap-action-free; the site's own trap action is re-attached). *)
Variable retarget : list simple -> apply_cont_expr.

Definition retarget_ac (ac : apply_cont_expr) : apply_cont_expr :=
  if continuation_eqb (ac_continuation ac) k
  then let ac' := retarget (ac_args ac) in
       Mk_apply_cont (ac_continuation ac') (ac_args ac')
         (ac_trap_action ac)
  else ac.

Fixpoint retarget_apply_cont (e : expr) : expr :=
  match e with
  | E_let p n body =>
      E_let p (rt_named n) (retarget_apply_cont body)
  | E_let_cont_nonrec k' h body =>
      E_let_cont_nonrec k' (rt_handler h)
        (if continuation_eqb k' k then body
         else retarget_apply_cont body)
  | E_let_cont_rec inv handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then e  (* k rebound over both body and handlers *)
      else
        E_let_cont_rec inv
          ((fix go (hs : list (continuation * cont_handler)) :=
              match hs with
              | [] => []
              | (k', h) :: tl => (k', rt_handler h) :: go tl
              end) handlers)
          (retarget_apply_cont body)
  | E_apply _ => e
  | E_apply_cont ac => E_apply_cont (retarget_ac ac)
  | E_switch sw =>
      E_switch
        (Mk_switch (sw_scrutinee sw)
           ((fix go (arms : list (target_ocaml_int * apply_cont_expr)) :=
               match arms with
               | [] => []
               | (d, ac) :: tl => (d, retarget_ac ac) :: go tl
               end) (sw_arms sw)))
  | E_invalid _ => e
  end

with rt_named (n : named) : named :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ => n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => rt_scc s :: go tl
            end) scg)
  end

with rt_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (retarget_apply_cont body) exn cold
  end

with rt_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (rt_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with rt_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then c
      else Mk_code0 kret kexn ps (retarget_apply_cont body)
             clo dep am pa ra rm tup
  end.

End RetargetApplyCont.

(* The Shortcut substitution proper:
   e[ apply_cont k bbar |-> apply_cont k' (abar[pbar |-> bbar]) ]. *)
Definition shortcut_apply_cont
    (k : continuation) (ps : list (variable * kind_ws))
    (k' : continuation) (a_args : list simple) (e : expr) : expr :=
  retarget_apply_cont k
    (fun bs =>
       Mk_apply_cont k'
         (map (subst_simple (param_subst ps bs)) a_args) None)
    e.

(* The pure-alias retarget (S.Rewrite.LetCont.Shortcut's to_alias
   case): rewrite free uses of k in Apply RETURN/EXN position to
   k2.  Only the pure-alias shortcut can move these uses -- a
   general shortcut has no apply_cont site to substitute into
   (expr_builder.ml#apply_continuation_aliases and
   #apply_exn_continuation_aliases consult to_alias).  Scope-aware
   like retarget_apply_cont; apply_cont sites are untouched here
   (shortcut_apply_cont's business). *)

Section RetargetApplyAliases.

Variable k k2 : continuation.

Definition alias_apply (ap : apply_expr) : apply_expr :=
  Mk_apply (ap_callee ap) (ap_args ap)
    (match ap_result_continuation ap with
     | RC_return kr =>
         if continuation_eqb kr k then RC_return k2
         else RC_return kr
     | RC_never_returns => RC_never_returns
     end)
    (match ap_exn_continuation ap with
     | Mk_exn_continuation ke eargs =>
         if continuation_eqb ke k
         then Mk_exn_continuation k2 eargs
         else Mk_exn_continuation ke eargs
     end)
    (ap_args_arity ap) (ap_return_arity ap) (ap_call_kind ap)
    (ap_alloc_mode ap).

Fixpoint retarget_apply_aliases (e : expr) : expr :=
  match e with
  | E_let p n body =>
      E_let p (ra_named n) (retarget_apply_aliases body)
  | E_let_cont_nonrec k0 h body =>
      E_let_cont_nonrec k0 (ra_handler h)
        (if continuation_eqb k0 k then body
         else retarget_apply_aliases body)
  | E_let_cont_rec inv handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then e  (* k rebound over both body and handlers *)
      else
        E_let_cont_rec inv
          ((fix go (hs : list (continuation * cont_handler)) :=
              match hs with
              | [] => []
              | (k0, h) :: tl => (k0, ra_handler h) :: go tl
              end) handlers)
          (retarget_apply_aliases body)
  | E_apply ap => E_apply (alias_apply ap)
  | E_apply_cont _ | E_switch _ | E_invalid _ => e
  end

with ra_named (n : named) : named :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ => n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => ra_scc s :: go tl
            end) scg)
  end

with ra_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (retarget_apply_aliases body) exn cold
  end

with ra_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (ra_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with ra_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then c
      else Mk_code0 kret kexn ps (retarget_apply_aliases body)
             clo dep am pa ra rm tup
  end.

End RetargetApplyAliases.

(* ================================================================== *)
(* 3. Loopify metadata                                                *)
(* ================================================================== *)

(* Loopify_attribute.t.  The Default_* values exist only between
   closure conversion and the first simplification of the code
   (S.Rewrite.Loopify.AttributeUpdate). *)
Inductive loopify_attribute :=
  Always_loopify
| Never_loopify
| Already_loopified
| Default_loopify_and_tailrec
| Default_loopify_and_not_tailrec.

(* Loopify_attribute.should_loopify: true exactly for Always_loopify
   and Default_loopify_and_tailrec (S.Rewrite.Loopify.Attribute
   NOTES). *)
Definition should_loopify (a : loopify_attribute) : bool :=
  match a with
  | Always_loopify | Default_loopify_and_tailrec => true
  | Never_loopify | Already_loopified
  | Default_loopify_and_not_tailrec => false
  end.

(* Code_metadata.result: the recursion flag recomputed by
   S.Rewrite.Code.RecursiveRecompute. *)
Inductive recursiveness :=
  Recursive
| Non_recursive.

(* ================================================================== *)
(* 4. Universally quantified conditions over free uses                *)
(* ================================================================== *)

(* Prop-valued companion of ContUseCounting: [uses_ok] holds when
   every free use position in k's scope satisfies the corresponding
   predicate.  Unlike the counters, the expression-position and
   switch-arm predicates are applied to EVERY apply_cont in k's scope
   (not only those targeting k): S.Rewrite.LetCont.DemoteExn inspects
   trap actions naming k that sit on jumps to other continuations.
   Scope-stopping mirrors cont_uses. *)

Section AllFreeUses.

Variable k : continuation.
Variable P_ac_expr : apply_cont_expr -> Prop.
Variable P_ac_arm : apply_cont_expr -> Prop.
Variable P_rc : result_continuation -> Prop.
Variable P_ec : exn_continuation -> Prop.

Fixpoint uses_ok (e : expr) : Prop :=
  match e with
  | E_let _ n body => uo_named n /\ uses_ok body
  | E_let_cont_nonrec k' h body =>
      uo_handler h
      /\ (if continuation_eqb k' k then True else uses_ok body)
  | E_let_cont_rec _ handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then True
      else
        (fix go (hs : list (continuation * cont_handler)) : Prop :=
           match hs with
           | [] => True
           | (_, h) :: tl => uo_handler h /\ go tl
           end) handlers
        /\ uses_ok body
  | E_apply ap =>
      P_rc (ap_result_continuation ap)
      /\ P_ec (ap_exn_continuation ap)
  | E_apply_cont ac => P_ac_expr ac
  | E_switch sw =>
      (fix go (arms : list (target_ocaml_int * apply_cont_expr))
           : Prop :=
         match arms with
         | [] => True
         | (_, ac) :: tl => P_ac_arm ac /\ go tl
         end) (sw_arms sw)
  | E_invalid _ => True
  end

with uo_named (n : named) : Prop :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ =>
      True
  | N_static_consts scg =>
      (fix go (l : list static_const_or_code) : Prop :=
         match l with
         | [] => True
         | s :: tl => uo_scc s /\ go tl
         end) scg
  end

with uo_handler (h : cont_handler) : Prop :=
  match h with Mk_cont_handler _ body _ _ => uses_ok body end

with uo_scc (s : static_const_or_code) : Prop :=
  match s with
  | SCC_code c => uo_code c
  | SCC_deleted_code | SCC_static_const _ => True
  end

with uo_code (c : code0) : Prop :=
  match c with
  | Mk_code0 kret kexn _ body _ _ _ _ _ _ _ =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then True
      else uses_ok body
  end.

End AllFreeUses.

(* ================================================================== *)
(* 5. Mapping over free apply_cont sites                              *)
(* ================================================================== *)

(* Rewrites every apply_cont in k's scope: expression-position sites
   through [f_expr] (which may produce a non-apply_cont expression,
   e.g. Invalid), switch arms through [f_arm] (None drops the arm).
   Used by S.Rewrite.LetCont.InvalidHandler and .DemoteExn.
   Scope-stopping mirrors retarget_apply_cont. *)

Section MapFreeApplyCont.

Variable k : continuation.
Variable f_expr : apply_cont_expr -> expr.
Variable f_arm : target_ocaml_int * apply_cont_expr
                 -> option (target_ocaml_int * apply_cont_expr).

Fixpoint map_free_ac (e : expr) : expr :=
  match e with
  | E_let p n body => E_let p (mfa_named n) (map_free_ac body)
  | E_let_cont_nonrec k' h body =>
      E_let_cont_nonrec k' (mfa_handler h)
        (if continuation_eqb k' k then body else map_free_ac body)
  | E_let_cont_rec inv handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then e
      else
        E_let_cont_rec inv
          ((fix go (hs : list (continuation * cont_handler)) :=
              match hs with
              | [] => []
              | (k', h) :: tl => (k', mfa_handler h) :: go tl
              end) handlers)
          (map_free_ac body)
  | E_apply _ => e
  | E_apply_cont ac => f_expr ac
  | E_switch sw =>
      E_switch
        (Mk_switch (sw_scrutinee sw)
           ((fix go (arms : list (target_ocaml_int * apply_cont_expr))
                 :=
               match arms with
               | [] => []
               | arm :: tl =>
                   match f_arm arm with
                   | Some arm' => arm' :: go tl
                   | None => go tl
                   end
               end) (sw_arms sw)))
  | E_invalid _ => e
  end

with mfa_named (n : named) : named :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ =>
      n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => mfa_scc s :: go tl
            end) scg)
  end

with mfa_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (map_free_ac body) exn cold
  end

with mfa_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (mfa_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with mfa_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then c
      else Mk_code0 kret kexn ps (map_free_ac body)
             clo dep am pa ra rm tup
  end.

End MapFreeApplyCont.

(* ================================================================== *)
(* 6. Shared helpers for the rewrite rules                            *)
(* ================================================================== *)

Fixpoint remove_nth {A : Type} (j : nat) (l : list A) : list A :=
  match j, l with
  | _, [] => []
  | O, _ :: tl => tl
  | S j', hd :: tl => hd :: remove_nth j' tl
  end.

(* Drop the j-th argument of every free apply_cont targeting k
   (expression positions and switch arms; retarget_ac preserves the
   site's trap action). *)
Definition drop_arg_at (k : continuation) (j : nat) (e : expr)
    : expr :=
  retarget_apply_cont k
    (fun bs => Mk_apply_cont k (remove_nth j bs) None) e.

(* let p1 = a1 in ... let pn = an in body
   (S.Rewrite.LetCont.Inline's parameter binding). *)
Fixpoint bind_params (ps : list (variable * kind_ws))
    (args : list simple) (body : expr) : expr :=
  match ps, args with
  | (x, _) :: ps', a :: args' =>
      E_let (BPat_singleton (Mk_bound_var x NM_normal)) (N_simple a)
        (bind_params ps' args' body)
  | _, _ => body
  end.

Definition strip_coercion (s : simple) : simple :=
  match s with
  | Simple_name n _ => Simple_name n Coercion_id
  | Simple_const _ => s
  end.

(* The parameters of a handler, passed back as arguments. *)
Definition params_as_args (ps : list (variable * kind_ws))
    : list simple :=
  map (fun p => simple_of_name (Name_var (fst p))) ps.

(* S.Rewrite.LetCont.DemoteExn's all-or-nothing erasure: every Push
   and Pop trap action naming k, at every site in k's scope, is
   cleared (simplify_common.ml#clear_demoted_trap_action). *)
Definition strip_traps_naming (k : continuation)
    (ac : apply_cont_expr) : apply_cont_expr :=
  match ac_trap_action ac with
  | Some (Trap_push k') =>
      if continuation_eqb k' k
      then Mk_apply_cont (ac_continuation ac) (ac_args ac) None
      else ac
  | Some (Trap_pop k' _) =>
      if continuation_eqb k' k
      then Mk_apply_cont (ac_continuation ac) (ac_args ac) None
      else ac
  | None => ac
  end.

Definition clear_demoted_traps (k : continuation) (e : expr)
    : expr :=
  map_free_ac k
    (fun ac => E_apply_cont (strip_traps_naming k ac))
    (fun arm => Some (fst arm, strip_traps_naming k (snd arm)))
    e.

(* S.Rewrite.LetCont.InvalidHandler's use rewrite: every jump to k
   becomes Invalid.  ENCODING NOTE: a switch ARM targeting k cannot
   become an inline Invalid (arms are apply_conts), so such arms are
   dropped instead -- the implementation reaches the same state by
   rebuilding the arm's apply_cont as unreachable and letting the
   switch rebuild discard it.  Apply uses of k (return/exn position)
   are not rewritten: the call itself may still run. *)
Definition invalidate_jumps_to (k : continuation) (e : expr)
    : expr :=
  map_free_ac k
    (fun ac =>
       if continuation_eqb (ac_continuation ac) k
       then E_invalid "Apply_cont_of_unreachable_continuation"%string
       else E_apply_cont ac)
    (fun arm =>
       if continuation_eqb (ac_continuation (snd arm)) k
       then None
       else Some arm)
    e.

(* Effects.at_most_generative_effects on the effects axis. *)
Definition at_most_generative_effects (eff : effects) : Prop :=
  match eff with
  | No_effects | Only_generative_effects _ => True
  | Arbitrary_effects => False
  end.

(* Named.at_most_generative_effects: simples, rec_infos, set-of-
   closures allocations, and static consts qualify; a prim qualifies
   by its effects classification (PrimMemoryA.effects_of) under the
   ambient effect flags fl.  (A forall-fl variant was considered and
   rejected as stronger than the doc's rule: the pass runs under one
   flag setting, threaded via rw_control's Section variable.) *)
Definition named_at_most_generative (fl : eff_flags) (n : named)
    : Prop :=
  match n with
  | N_prim p =>
      at_most_generative_effects
        (ece_effects (effects_of fl (prim_op_of p)))
  | N_simple _ | N_set_of_closures _ _ | N_static_consts _
  | N_rec_info _ => True
  end.

(* A type is (semantically) Bottom iff its meet with itself is
   Bottom; states "gamma_E(T) = empty" using only the frozen meet
   relation (MeetJoin.v). *)
Definition type_is_bottom (E : tenv) (T : ftype) : Prop :=
  meet E T T Meet_bottom.

(* S.Rewrite.Apply.IndirectToDirect's consistency premise: the proven
   code id must be consistent with the call kind's code ids under the
   code-age relation.  Direction: cid newer-eq a call-kind id is the
   case where Code_age_relation.meet_set resolves the pair to cid
   itself -- the id the rule's conclusion pins.  The converse
   resolution (a call-kind id strictly newer than cid; the code
   redirects to THAT id) is deliberately not covered
   (under-approximation). *)
Definition indirect_code_ids_consistent (E : tenv) (cid : code_id)
    (fc : function_call) : Prop :=
  match fc with
  | FC_indirect_known_arity (Ou_known cids) =>
      exists cid0, In cid0 cids /\ code_age_newer_eq E cid cid0
  | _ => True
  end.

(* S.Rewrite.Apply.Invalid's argument-kind check, mirroring
   simplify_apply_expr.ml#arity_mismatch (KF-025): the shorter
   arity's parameters are compared against the same-length prefix of
   the other, per parameter, on unarized kinds; a mismatch is a
   common-prefix parameter whose unarized components differ in length
   or kind.  A plain length difference between the two arities is
   NOT a mismatch -- that is over/partial application, licensed by
   its own rules. *)
Fixpoint arity_prefix_kind_mismatch (a1 a2 : arity) : Prop :=
  match a1, a2 with
  | [], _ | _, [] => False
  | c1 :: tl1, c2 :: tl2 =>
      ~ equal_ignoring_subkinds (unarize_component c1)
          (unarize_component c2)
      \/ arity_prefix_kind_mismatch tl1 tl2
  end.

(* An empty recursive group degenerates to its body
   (S.Rewrite.LetCont.Demote's residual group). *)
Definition rebuild_rec_group (inv : list (variable * kind_ws))
    (hs : list (continuation * cont_handler)) (body : expr)
    : expr :=
  match hs with
  | [] => body
  | _ :: _ => E_let_cont_rec inv hs body
  end.

(* let x1 = Project_value_slot(fs, w1) my_closure in ... body
   (the captured-variable prelude of
   S.Rewrite.Apply.PartialApplication's wrapper stub). *)
Fixpoint bind_projections (fs : function_slot) (myc : variable)
    (cs : list (variable * value_slot)) (body : expr) : expr :=
  match cs with
  | [] => body
  | (x, w) :: tl =>
      E_let (BPat_singleton (Mk_bound_var x NM_normal))
        (N_prim (P_unary (UP_project_value_slot fs w)
           (simple_of_name (Name_var myc))))
        (bind_projections fs myc tl body)
  end.

(* ================================================================== *)
(* 7. Loopification (10-simplify-rewrites.md, Loopification)          *)
(* ================================================================== *)

(* The source-level [@loop] attribute (Lambda's loop_attribute), the
   input to the closure-conversion decision. *)
Inductive loop_attribute :=
  Always_loop
| Never_loop
| Default_loop.

(* The syntactic "purely tail-recursive" test.  Every free occurrence
   of x (my_closure) must be either the callee of a full tail call
   (result continuation = the function's return continuation, exn
   continuation = the function's, no extra args) or the argument of a
   Project_value_slot primitive (the value-slot exemption,
   S.Rewrite.Loopify.Attribute.ValueSlotExempt).  Encoded by ERASING
   exactly those two occurrence forms and asking that x is not free
   in the residue.  Per the ValueSlotExempt NOTES, the exemption
   matches the prim SHAPE (any Project_value_slot argument), so
   exempt_prim rewrites the argument unconditionally. *)

Section EraseExemptUses.

Variable x : variable.            (* my_closure *)
Variable kret kexn : continuation.

Definition exempt_prim (p : prim) : prim :=
  match p with
  | P_unary (UP_project_value_slot fs vs) _ =>
      P_unary (UP_project_value_slot fs vs)
        (Simple_const (Const_tagged_immediate 0))
  | _ => p
  end.

Definition erase_tail_callee (ap : apply_expr) : apply_expr :=
  match ap_callee ap, ap_result_continuation ap,
        ap_exn_continuation ap with
  | Some (Simple_name (Name_var y) _), RC_return kr,
    Mk_exn_continuation ke nil =>
      if variable_eqb y x
         && continuation_eqb kr kret
         && continuation_eqb ke kexn
      then Mk_apply None (ap_args ap) (ap_result_continuation ap)
             (ap_exn_continuation ap) (ap_args_arity ap)
             (ap_return_arity ap) (ap_call_kind ap)
             (ap_alloc_mode ap)
      else ap
  | _, _, _ => ap
  end.

Fixpoint erase_exempt (e : expr) : expr :=
  match e with
  | E_let p n body => E_let p (ee_named n) (erase_exempt body)
  | E_let_cont_nonrec k' h body =>
      E_let_cont_nonrec k' (ee_handler h) (erase_exempt body)
  | E_let_cont_rec inv handlers body =>
      E_let_cont_rec inv
        ((fix go (hs : list (continuation * cont_handler)) :=
            match hs with
            | [] => []
            | (k', h) :: tl => (k', ee_handler h) :: go tl
            end) handlers)
        (erase_exempt body)
  | E_apply ap => E_apply (erase_tail_callee ap)
  | E_apply_cont _ | E_switch _ | E_invalid _ => e
  end

with ee_named (n : named) : named :=
  match n with
  | N_prim p => N_prim (exempt_prim p)
  | N_simple _ | N_set_of_closures _ _ | N_rec_info _ => n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => ee_scc s :: go tl
            end) scg)
  end

with ee_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (erase_exempt body) exn cold
  end

with ee_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (ee_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with ee_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kr ke ps body clo dep am pa ra rm tup =>
      Mk_code0 kr ke ps (erase_exempt body) clo dep am pa ra rm tup
  end.

End EraseExemptUses.

Definition is_purely_tailrec (kret kexn : continuation)
    (x : variable) (e : expr) : Prop :=
  ~ free_vars (erase_exempt x kret kexn e) x.

(** RULE S.Rewrite.Loopify.Attribute (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function
    CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Acc.add_name_to_free_names *)
(* ENCODING NOTE (out-of-scope-frontend boundary, per coordinator
   ruling): a rule is ENCODED when its content is statable over
   in-scope syntax -- here, the 4-way attribute table over the
   syntactic is_purely_tailrec above -- even though WHO computes it
   (closure_conversion.ml, the out-of-scope from_lambda frontend) is
   frontend behavior; a rule is merely ANCHORED when its content is
   itself about the out-of-scope component (contrast
   P.Unchecked.FrontendInsertsChecks, whose content IS the Lambda
   lowering).  sole_in_group is the "sole function of its recursive
   group" premise, a from_lambda-side fact about the source recursive
   group, taken as an input here. *)
Definition S_Rewrite_Loopify_Attribute (la : loop_attribute)
    (sole_in_group : bool) (c : code0)
    (result : loopify_attribute) : Prop :=
  match la with
  | Always_loop => result = Always_loopify
  | Never_loop => result = Never_loopify
  | Default_loop =>
      (sole_in_group = true
       /\ is_purely_tailrec (c0_return_continuation c)
            (c0_exn_continuation c) (c0_my_closure c) (c0_body c)
       /\ result = Default_loopify_and_tailrec)
      \/
      ((sole_in_group = false
        \/ ~ is_purely_tailrec (c0_return_continuation c)
               (c0_exn_continuation c) (c0_my_closure c)
               (c0_body c))
       /\ result = Default_loopify_and_not_tailrec)
  end.

(** RULE S.Rewrite.Loopify.Attribute.ValueSlotExempt (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Let_with_acc.create
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function *)
(* A `let v = Project_value_slot(fs, w) s` binding does not falsify
   is_purely_tailrec, whatever its argument s (in particular
   my_closure): captured variables read from the closure are exempt.
   Stated as: prepending such a binding leaves is_purely_tailrec
   unchanged.  The binding must not rebind x itself (KF-027: if p
   binds my_closure, the left side holds vacuously by shadowing
   while the right side can fail; the doc's premise is about
   captured variables, which are never my_closure). *)
Theorem S_Rewrite_Loopify_Attribute_ValueSlotExempt :
  forall kret kexn x p fs vs s body,
    ~ In x (bound_pattern_vars p) ->
    (is_purely_tailrec kret kexn x
      (E_let p (N_prim (P_unary (UP_project_value_slot fs vs) s))
         body)
    <-> is_purely_tailrec kret kexn x body).
Admitted.

(** RULE S.Rewrite.Loopify.AttributeUpdate (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0 *)
Definition S_Rewrite_Loopify_AttributeUpdate
    (a : loopify_attribute) : loopify_attribute :=
  match a with
  | Always_loopify | Never_loopify | Already_loopified => a
  | Default_loopify_and_tailrec => Already_loopified
  | Default_loopify_and_not_tailrec => Never_loopify
  end.

(** RULE S.Rewrite.Code.RecursiveRecompute (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body *)
(* Not loopify-specific: every rebuilt code's recursive flag is
   recomputed from whether my_depth occurs free in the simplified
   body (self-Applys' rec_info coercions are its only mentions). *)
Definition S_Rewrite_Code_RecursiveRecompute (body' : expr)
    (my_depth : variable) (r : recursiveness) : Prop :=
  match r with
  | Recursive => free_vars body' my_depth
  | Non_recursive => ~ free_vars body' my_depth
  end.

(* The Loopify.Body wrapper:
   Code c = lambda<ret,exn> pbar <my_closure,...>. e
   becomes  let_cont rec k pbar = e in apply_cont k pbar
   (handler re-binds pbar, shadowing; the entry jump's arguments
   refer to the function's parameters). *)
Definition wrap_loopified (k : continuation) (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      Mk_code0 kret kexn ps
        (E_let_cont_rec []
           [(k, Mk_cont_handler ps body false false)]
           (E_apply_cont (Mk_apply_cont k (params_as_args ps) None)))
        clo dep am pa ra rm tup
  end.

(* The code-level judgment: Simplify rebuilds Code c (input loopify
   attribute a) into c', emitting attribute a' and recursive flag r.
   Loopify metadata flows as explicit arguments (coordinator ruling:
   no resurrected cache fields on the frozen code0).  Body-internal
   rewrites (including rw_self_tail_call below) compose at
   Simplify.v's closure, so the Body constructor's conclusion is
   exactly the one-step wrap.  This judgment and rw_self_tail_call
   are deliberately flag-free (ruled): neither consults effects_of,
   so eff_flags is not threaded, unlike rw_control's Section. *)
Inductive rw_code_loopify (C : code_env) (E : tenv)
    : loopify_attribute -> code0
      -> loopify_attribute -> recursiveness -> code0 -> Prop :=

(** RULE S.Rewrite.Loopify.Body (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
    CODE middle_end/flambda2/simplify/loopify_state.mli
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body *)
(* Unconditional given the attribute (no cost model or oracle).  The
   "terms are being rebuilt (not speculative inlining)" premise is
   pass machinery (loopify_decision_for_call's do_not_rebuild_terms
   case) and is carried in prose only.  The RecursiveRecompute
   premise is evaluated on the wrap-time body DELIBERATELY (per-step
   staging; my_depth is still free there): the doc recomputes on the
   final body, so Simplify.v's closure must re-run the recompute
   rather than treat this r as the emitted flag (W-24). *)
| S_Rewrite_Loopify_Body :
    forall a c k r,
      should_loopify a = true ->
      (* k fresh (named "self") *)
      ~ free_conts (c0_body c) k ->
      k <> c0_return_continuation c ->
      k <> c0_exn_continuation c ->
      S_Rewrite_Code_RecursiveRecompute
        (c0_body (wrap_loopified k c)) (c0_my_depth c) r ->
      rw_code_loopify C E a c
        (S_Rewrite_Loopify_AttributeUpdate a) r (wrap_loopified k c)

(* Plumbing (no rule id): the non-loopified rebuild still updates the
   attribute (AttributeUpdate) and recomputes the recursive flag
   (RecursiveRecompute). *)
| rw_code_no_loopify :
    forall a c r,
      should_loopify a = false ->
      S_Rewrite_Code_RecursiveRecompute (c0_body c) (c0_my_depth c)
        r ->
      rw_code_loopify C E a c
        (S_Rewrite_Loopify_AttributeUpdate a) r c.

(* The in-body redirection, parameterized by the ambient loopify
   state (loopify_state(denv) = Loopify k) and the enclosing
   function's my_closure and return/exn continuations. *)
Inductive rw_self_tail_call (C : code_env) (E : tenv)
    (k : continuation)
    (my_closure : variable) (kret kexn : continuation)
    : expr -> expr -> Prop :=

(** RULE S.Rewrite.Loopify.SelfTailCall (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call *)
(* The self-call test is semantic: callee and my_closure are compared
   after canonicalization through E with coercions stripped.  The
   discarded callee coercion (rec_info) is what removes my_depth from
   the body's free names (S.Rewrite.Code.RecursiveRecompute).  The
   "terms are being rebuilt" premise is pass machinery, in prose
   only. *)
| S_Rewrite_Loopify_SelfTailCall :
    forall ap f,
      ap_callee ap = Some f ->
      strip_coercion (canonical E f)
        = strip_coercion
            (canonical E (simple_of_name (Name_var my_closure))) ->
      (ap_result_continuation ap = RC_return kret
       \/ ap_result_continuation ap = RC_never_returns) ->
      (* exn continuation matches, with no extra args *)
      ap_exn_continuation ap = Mk_exn_continuation kexn [] ->
      rw_self_tail_call C E k my_closure kret kexn
        (E_apply ap)
        (E_apply_cont (Mk_apply_cont k (ap_args ap) None)).

(* ================================================================== *)
(* 8. The control-side rewrite relation                               *)
(* ================================================================== *)

(* C; E |- e ~> e'.  One constructor per rule, premises in doc order.
   C is the code table of the unit under compilation (code_env,
   Syntax.v) -- what find_code_exn consults.  ENCODING NOTE: every
   rw_* relation takes C as its leading argument (coordinator
   ruling): code is defined BY the unit under compilation, not an
   external oracle, so a global Parameter would sever the link
   INV.Simplify.Preserves needs; and tenv rightly excludes code,
   since Simplify's denv carries code separately from the types env.
   Here C supplies the callee-arity facts of the three Apply arity
   rules (OverApplication's split point, PartialApplication's
   remaining parameters, Invalid's kind/arity-mismatch disjunct) and
   IndirectToDirect's code-availability premise; the other rules
   ignore it.

   flags is the ambient effect-flag setting the pass runs under
   (coordinator ruling, matching Machine.v's Section): the closed
   type is eff_flags -> code_env -> tenv -> expr -> expr -> Prop.
   Only Let.DeadBinding's effects premise reads it. *)

Section RwControlRelation.

Variable flags : eff_flags.

Inductive rw_control (C : code_env) (E : tenv)
    : expr -> expr -> Prop :=

(** RULE S.Rewrite.Switch.ArmPrune (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm *)
(* The meet against `this_naked_immediate d` is Bottom, so the arm is
   unreachable and dropped. *)
| S_Rewrite_Switch_ArmPrune :
    forall x arms1 arms2 d ac,
      meet E (fst (type_simple_in_term E K_naked_immediate x))
        (FT_naked_immediate
           (Oub_ok (No_alias (Naked_immediates [d]))))
        Meet_bottom ->
      rw_control C E
        (E_switch (Mk_switch x (arms1 ++ (d, ac) :: arms2)))
        (E_switch (Mk_switch x (arms1 ++ arms2)))

(** RULE S.Rewrite.Switch.Merge (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_arm *)
(* "After pruning" is composition with ArmPrune.  ENCODING NOTE: the
   implementation's alias-SET intersection across arms is encoded
   pointwise: every arm's argument list canonicalizes (in E) to the
   canonical image of one common list abar, which the rewrite emits.
   Covers both the single-surviving-arm and all-arms-identical
   cases. *)
| S_Rewrite_Switch_Merge :
    forall sw k abar,
      sw_arms sw <> [] ->
      Forall
        (fun arm =>
           ac_continuation (snd arm) = k
           /\ ac_trap_action (snd arm) = None
           /\ map (canonical E) (ac_args (snd arm))
              = map (canonical E) abar)
        (sw_arms sw) ->
      rw_control C E (E_switch sw)
        (E_apply_cont (Mk_apply_cont k abar None))

(** RULE S.Rewrite.Switch.Identity (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch *)
(* Every arm passes its own discriminant, tagged, to one k; the
   switch is an identity map through tagging. *)
| S_Rewrite_Switch_Identity :
    forall sw k t,
      sw_arms sw <> [] ->
      Forall
        (fun arm =>
           ac_continuation (snd arm) = k
           /\ ac_trap_action (snd arm) = None
           /\ ac_args (snd arm)
              = [Simple_const (Const_tagged_immediate (fst arm))])
        (sw_arms sw) ->
      ~ free_vars (E_switch sw) t ->     (* t fresh *)
      rw_control C E (E_switch sw)
        (E_let (BPat_singleton (Mk_bound_var t NM_normal))
           (N_prim (P_unary UP_tag_immediate (sw_scrutinee sw)))
           (E_apply_cont
              (Mk_apply_cont k [simple_of_name (Name_var t)] None)))

(** RULE S.Rewrite.Switch.BooleanNot (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch *)
(* Discriminants exactly {0, 1}, arm 0 -> k [1] and arm 1 -> k [0]
   (either association-list order). *)
| S_Rewrite_Switch_BooleanNot :
    forall x arms k t n,
      (arms =
         [(0%Z, Mk_apply_cont k
                  [Simple_const (Const_tagged_immediate 1)] None);
          (1%Z, Mk_apply_cont k
                  [Simple_const (Const_tagged_immediate 0)] None)]
       \/
       arms =
         [(1%Z, Mk_apply_cont k
                  [Simple_const (Const_tagged_immediate 0)] None);
          (0%Z, Mk_apply_cont k
                  [Simple_const (Const_tagged_immediate 1)] None)])
      ->
      ~ free_vars (E_switch (Mk_switch x arms)) t ->
      ~ free_vars (E_switch (Mk_switch x arms)) n ->
      t <> n ->                          (* t, n fresh *)
      rw_control C E (E_switch (Mk_switch x arms))
        (E_let (BPat_singleton (Mk_bound_var t NM_normal))
           (N_prim (P_unary UP_tag_immediate x))
           (E_let (BPat_singleton (Mk_bound_var n NM_normal))
              (N_prim (P_unary UP_boolean_not
                 (simple_of_name (Name_var t))))
              (E_apply_cont
                 (Mk_apply_cont k [simple_of_name (Name_var n)]
                    None))))

(** RULE S.Rewrite.Switch.Invalid (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch *)
(* "After pruning, zero arms survive": composition with ArmPrune
   (which also covers the Bottom-scrutinee case arm by arm). *)
| S_Rewrite_Switch_Invalid :
    forall x,
      rw_control C E (E_switch (Mk_switch x []))
        (E_invalid "Zero_switch_arms"%string)

(** RULE S.Rewrite.Let.DeadBinding (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/terms/flambda_primitive.ml#at_most_generative_effects *)
(* "No phantom binding is required" (the generate_phantom_lets
   alternative, S.Rewrite.Let.Phantom) is a debug-flag fact carried
   in prose only. *)
| S_Rewrite_Let_DeadBinding :
    forall p n e,
      named_at_most_generative flags n ->
      (forall x, In x (bound_pattern_vars p) -> ~ free_vars e x) ->
      rw_control C E (E_let p n e) e

(** RULE S.Rewrite.Let.DeadRegion (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region *)
(* ENCODING NOTE: the flow-analysis premise "rho not in
   required_names" is encoded as its syntactic residue: rho has no
   further occurrence in e.  The doc allows the bound unit variable
   to have surviving uses (sound because simplify_end_region pins the
   result type to tagged 0, so uses canonicalize to that constant
   first); we require the post-canonicalization state, ~ free_vars e
   x, keeping the conclusion well-scoped -- the constant substitution
   is S.Rewrite.Alias.Canonicalize (RewritesPrim.v). *)
| S_Rewrite_Let_DeadRegion :
    forall p ghost rho co e,
      ~ free_vars e rho ->
      (forall x, In x (bound_pattern_vars p) -> ~ free_vars e x) ->
      rw_control C E
        (E_let p
           (N_prim (P_unary (UP_end_region ghost)
              (Simple_name (Name_var rho) co)))
           e)
        e

(** RULE S.Rewrite.Let.Invalid (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#simplify_let0
    CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0 *)
(* ENCODING NOTE: "simplification of n yields SPR.create_invalid" is
   encoded by its dominant trigger, an argument whose type is Bottom
   (the argument's kind kk is existential); the further create_invalid
   sources are prim-side (RewritesPrim.v). *)
| S_Rewrite_Let_Invalid :
    forall p pr e kk s,
      In s (prim_args pr) ->
      type_is_bottom E (fst (type_simple_in_term E kk s)) ->
      rw_control C E (E_let p (N_prim pr) e)
        (E_invalid "Defining_expr_of_let"%string)

(** RULE S.Rewrite.LetCont.Inline (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation *)
(* The two counts together say: exactly one use, and that use is of
   kind Inlinable (a trap-free expression-position apply_cont; switch
   -arm, trap-bearing, and Apply return/exn uses are Non_inlinable
   and weigh zero in the second counter).  Per fidelity watch item
   W-13, replace_apply_cont (Syntax.v) already embeds ch. 10's
   use-kind filtering -- expression position only, trap-action-free
   sites only -- so no further use-kind premise is stated here. *)
| S_Rewrite_LetCont_Inline :
    forall k h e,
      count_cont_uses k e = 1 ->
      count_inlinable_uses k e = 1 ->
      ch_is_cold h = false ->
      rw_control C E (E_let_cont_nonrec k h e)
        (replace_apply_cont k
           (fun args => bind_params (ch_params h) args (ch_handler h))
           e)

(** RULE S.Rewrite.LetCont.DeadHandler (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont *)
(* count_cont_uses excludes trap actions by construction, matching
   "counting excludes trap actions". *)
| S_Rewrite_LetCont_DeadHandler :
    forall k h e,
      count_cont_uses k e = 0 ->
      rw_control C E (E_let_cont_nonrec k h e) e

(** RULE S.Rewrite.LetCont.Shortcut (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/continuation_shortcut.ml#apply
    CODE middle_end/flambda2/simplify/continuation_shortcut.ml#to_alias
    CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts *)
(* The handler is a single trap-free apply_cont; the substitution
   rewrites every apply_cont use of k -- including switch arms and
   sites with trap actions, whose SITE trap action survives (the
   no-trap condition is on the handler's jump, not the site's; see
   .ShortcutFlat NOTES).  The is_exn_handler = false literal reflects
   that shortcuts are never registered for (undemoted) exception
   handlers (S.Rewrite.LetCont.DemoteExn conclusion (3)).  The
   binder is KEPT (KF-024, coordinator ruling (b): the doc's
   conclusion is amended to match rebuild_let_cont's keep-until-
   zero-occurrences behavior): the substitution cannot reach uses
   of k in Apply return/exn position, so dropping the binder here
   would leave them dangling or captured; once no uses survive,
   S.Rewrite.LetCont.DeadHandler composes to drop it, recovering
   the old conclusion. *)
| S_Rewrite_LetCont_Shortcut :
    forall k ps k' aargs cold e,
      rw_control C E
        (E_let_cont_nonrec k
           (Mk_cont_handler ps
              (E_apply_cont (Mk_apply_cont k' aargs None)) false
              cold)
           e)
        (E_let_cont_nonrec k
           (Mk_cont_handler ps
              (E_apply_cont (Mk_apply_cont k' aargs None)) false
              cold)
           (shortcut_apply_cont k ps k' aargs e))

(* Pure-alias companion under the same rule id (the doc NOTES' "when
   abar is exactly pbar this is a pure alias", to_alias): only in
   this case does the code ALSO retarget uses of k in Apply
   return/exn position to k'
   (expr_builder.ml#apply_continuation_aliases and
   #apply_exn_continuation_aliases consult to_alias); the general
   substitution above cannot reach those positions.  Binder kept,
   as above; when the alias retarget removes the last use,
   DeadHandler composes.  Two constructors under one id follows the
   rw_unbox precedent. *)
| S_Rewrite_LetCont_Shortcut_Alias :
    forall k ps k' cold e,
      rw_control C E
        (E_let_cont_nonrec k
           (Mk_cont_handler ps
              (E_apply_cont
                 (Mk_apply_cont k' (params_as_args ps) None))
              false cold)
           e)
        (E_let_cont_nonrec k
           (Mk_cont_handler ps
              (E_apply_cont
                 (Mk_apply_cont k' (params_as_args ps) None))
              false cold)
           (retarget_apply_aliases k k'
              (shortcut_apply_cont k ps k' (params_as_args ps)
                 e)))

(** RULE S.Rewrite.LetCont.UnusedParam (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
    CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze *)
(* Encodes the non-recursive case, where the rebuilt handler's free
   names are authoritative (the recursive case reads the whole-body
   flow analysis, pass machinery).  ENCODING NOTE: the phantom-
   parameter fallback (Optimised_out) is not modeled; uses of k in
   Apply return/exn position are excluded by premise (their argument
   positions are call results and cannot be dropped).  The j <> 0
   guard for exception handlers is the exn-bucket pinning of
   S.Struct.Flow.ExnFirstParam / S.Rewrite.LetCont.DemoteExn NOTES:
   for a kept exn handler the bucket param is unconditionally
   required. *)
| S_Rewrite_LetCont_UnusedParam :
    forall k ps body exn cold e j x kw,
      nth_error ps j = Some (x, kw) ->
      ~ free_vars body x ->
      (exn = true -> j <> 0) ->
      uses_ok k (fun _ => True) (fun _ => True)
        (fun rc => rc <> RC_return k)
        (fun ec => ec_exn_handler ec <> k)
        e ->
      rw_control C E
        (E_let_cont_nonrec k (Mk_cont_handler ps body exn cold) e)
        (E_let_cont_nonrec k
           (Mk_cont_handler (remove_nth j ps) body exn cold)
           (drop_arg_at k j e))

(** RULE S.Rewrite.LetCont.AliasedParam (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler *)
(* The invariant/constant-parameter optimization: pj always receives
   (a value canonically equal to) the in-scope s, so it is
   re-materialized inside the handler and dropped at every call
   site.  s is a constant, symbol, or another parameter of k (doc
   parenthetical).  The j <> 0 exn guard is
   S.Struct.Flow.ExnFirstParam's content, stated here as the side
   condition of the aliased-param rewrite (per its owner's
   assignment). *)
| S_Rewrite_LetCont_AliasedParam :
    forall k ps body exn cold e j x kw s,
      nth_error ps j = Some (x, kw) ->
      (match s with
       | Simple_const _ => True
       | Simple_name (Name_sym _) _ => True
       | Simple_name (Name_var v) _ =>
           v <> x /\ In v (map fst ps)
       end) ->
      uses_ok k
        (fun ac =>
           ac_continuation ac = k ->
           exists b, nth_error (ac_args ac) j = Some b
                     /\ canonical E b = canonical E s)
        (fun ac =>
           ac_continuation ac = k ->
           exists b, nth_error (ac_args ac) j = Some b
                     /\ canonical E b = canonical E s)
        (fun rc => rc <> RC_return k)
        (fun ec => ec_exn_handler ec <> k)
        e ->
      (exn = true -> j <> 0) ->
      rw_control C E
        (E_let_cont_nonrec k (Mk_cont_handler ps body exn cold) e)
        (E_let_cont_nonrec k
           (Mk_cont_handler (remove_nth j ps)
              (E_let (BPat_singleton (Mk_bound_var x NM_normal))
                 (N_simple s) body)
              exn cold)
           (drop_arg_at k j e))

(** RULE S.Rewrite.LetCont.InvalidHandler (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont *)
(* The handler is Invalid ("removable as Invalid" -- a handler that
   simplifies to Invalid reaches this shape first); every jump to k
   becomes Invalid (see invalidate_jumps_to's note for switch
   arms). *)
| S_Rewrite_LetCont_InvalidHandler :
    forall k ps msg exn cold e,
      rw_control C E
        (E_let_cont_nonrec k
           (Mk_cont_handler ps (E_invalid msg) exn cold) e)
        (E_let_cont_nonrec k
           (Mk_cont_handler ps (E_invalid msg) exn cold)
           (invalidate_jumps_to k e))

(** RULE S.Rewrite.LetCont.DemoteExn (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#demote_exn_handler
    CODE middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind
    CODE middle_end/flambda2/simplify/simplify_common.ml#clear_demoted_trap_action
    CODE middle_end/flambda2/simplify/simplify_common.ml#patch_unused_exn_bucket
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env *)
(* No use of k is escaping: (a) no Apply carries k as its exn
   continuation, and (b) every raise to k (a Pop k trap action) has
   raise_kind No_trace.  ENCODING NOTE: the doc's alternative "(b) OR
   Flambda_features.debug () is false" needs a debug-flag oracle the
   term does not carry; we encode the flag-independent core
   (raise_notrace demotes regardless of -g).  Push-carrying uses
   never block demotion (the predicate ignores Push).  Conclusion:
   is_exn_handler flips to false and EVERY Push/Pop naming k is
   erased, all-or-nothing (clear_demoted_traps); the unlocking of the
   normal-continuation rule set, conclusion (3), is composition --
   the demoted binder now matches the Inline/Shortcut/param-rule
   constructors above. *)
| S_Rewrite_LetCont_DemoteExn :
    forall k ps body cold e,
      uses_ok k
        (fun ac =>
           forall rk, ac_trap_action ac = Some (Trap_pop k rk)
                      -> rk = Some RK_no_trace)
        (fun ac =>
           forall rk, ac_trap_action ac = Some (Trap_pop k rk)
                      -> rk = Some RK_no_trace)
        (fun _ => True)
        (fun ec => ec_exn_handler ec <> k)
        e ->
      rw_control C E
        (E_let_cont_nonrec k (Mk_cont_handler ps body true cold) e)
        (E_let_cont_nonrec k (Mk_cont_handler ps body false cold)
           (clear_demoted_traps k e))

(** RULE S.Rewrite.Apply.IndirectToDirect (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_function_call
    CODE middle_end/flambda2/types/provers.ml#meet_single_closures_entry *)
(* The prover pins the callee's type to a single closure whose
   function slot carries a known Function_type; cid's code must be
   available (find_code_exn, i.e. C cid is defined) and consistent
   with the call kind's code ids under code aging.  The
   tupled-callee adaptation (simplify_direct_tuple_application) is
   NOTES-level and elided. *)
| S_Rewrite_Apply_IndirectToDirect :
    forall ap f fc fs fts cts vts cid ri,
      ap_callee ap = Some f ->
      ap_call_kind ap = CK_function fc ->
      (fc = FC_indirect_unknown_arity
       \/ exists cids, fc = FC_indirect_known_arity cids) ->
      prove_single_closures_entry E f fs
        (Mk_closures_entry fts cts vts) ->
      fts fs = Some (Ou_known (Mk_function_type cid ri)) ->
      C cid <> None ->
      indirect_code_ids_consistent E cid fc ->
      rw_control C E (E_apply ap)
        (E_apply (Mk_apply (Some f) (ap_args ap)
           (ap_result_continuation ap) (ap_exn_continuation ap)
           (ap_args_arity ap) (ap_return_arity ap)
           (CK_function (FC_direct cid)) (ap_alloc_mode ap)))

(** RULE S.Rewrite.Apply.OverApplication (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_over_application
    CODE middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application *)
(* Split at the callee's arity: a full direct application producing
   an intermediate closure g (received by fresh continuation kg),
   then an indirect application of g to the rest.  "cid takes n
   params and m > n" is the code-table lookup C cid = Some cd; the
   trigger m > n is measured in NON-unarized params (the code
   asserts num_params args_arity > num_params params_arity), while
   the split point is the callee's full unarized width,
   length (c0_params cd) (well-formedness ties it to
   cardinal_unarized of c0_params_arity;
   split_direct_over_application splits the args at that index).
   The
   intermediate arities and the second application's alloc mode are
   existential.  The Begin/End_region pair inserted around the pair
   when the full application returns a local closure is NOTES-level
   and elided. *)
| S_Rewrite_Apply_OverApplication :
    forall ap f cid cd args1 args2 g kg kwg ar1 rar1 ar2 amode2,
      ap_callee ap = Some f ->
      ap_call_kind ap = CK_function (FC_direct cid) ->
      C cid = Some cd ->
      ap_args ap = args1 ++ args2 ->
      length args1 = length (c0_params cd) ->
      num_params (c0_params_arity cd)
        < num_params (ap_args_arity ap) ->
      ~ free_vars (E_apply ap) g ->
      ~ free_conts (E_apply ap) kg ->    (* g, kg fresh *)
      rw_control C E (E_apply ap)
        (E_let_cont_nonrec kg
           (Mk_cont_handler [(g, kwg)]
              (E_apply (Mk_apply
                 (Some (simple_of_name (Name_var g))) args2
                 (ap_result_continuation ap)
                 (ap_exn_continuation ap)
                 ar2 (ap_return_arity ap)
                 (CK_function FC_indirect_unknown_arity) amode2))
              false false)
           (E_apply (Mk_apply (Some f) args1
              (RC_return kg) (ap_exn_continuation ap)
              ar1 rar1
              (CK_function (FC_direct cid)) (ap_alloc_mode ap))))

(** RULE S.Rewrite.Apply.PartialApplication (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application *)
(* A wrapper stub is built: fresh code cid' whose parameters ps_rem
   are the callee's remaining params.  "cid takes n params and
   0 < m < n" is measured in NON-unarized params: the code compares
   num_params on the arities and splits with partially_apply at the
   component level (Base.v's arity_partially_apply is exactly that
   operation), so ps_rem's kinds are the unarization of the
   remaining components and the supplied args must cover whole
   leading components -- an unboxed product is never split
   mid-application.  The coverage premise is stated explicitly here
   (the doc's rule text is silent on the measure and leaves the
   coverage to WF.Apply.DirectArity prefix agreement; flagged as a
   doc-precision candidate).  The stub's body projects the
   captured callee f (slot wf, bound to xf) and each supplied
   argument (slots map snd cs) out of its own closure and performs
   the full application of f; the rewrite binds the stub code
   statically, allocates its closure g capturing f and the supplied
   args, and returns g to the apply's continuation.  ENCODING NOTE:
   the code-metadata facts (~stub:true, inlining_decision Stub, the
   alloc-mode forcing table and its dead-GADT-case Invalid with
   reason Partial_application_mode_mismatch) are elided metadata;
   freshness of cid', fs', kret' and kexn' is prose (the stub is
   closed).  The stub-internal variable and value-slot names are
   pairwise distinct by premise (NoDup; KF-026 -- e.g. a ps_rem
   name colliding with xf would make bind_projections shadow a stub
   parameter and full-apply the wrong value). *)
| S_Rewrite_Apply_PartialApplication :
    forall ap f cid cd kk cid' fs' g xf wf cs ps_rem myc mydep
           kret' kexn' stub_body stub am_full ar_full ra_full
           am_c pa_c ra_c rm_c am_soc,
      ap_callee ap = Some f ->
      ap_call_kind ap = CK_function (FC_direct cid) ->
      C cid = Some cd ->
      ap_result_continuation ap = RC_return kk ->
      ap_args ap <> [] ->
      num_params (ap_args_arity ap)
        < num_params (c0_params_arity cd) ->
      (* the supplied args are whole leading components *)
      length (ap_args ap)
        = cardinal_unarized
            (firstn (num_params (ap_args_arity ap))
               (c0_params_arity cd)) ->
      map snd ps_rem
        = unarize
            (arity_partially_apply (c0_params_arity cd)
               (num_params (ap_args_arity ap))) ->
      length cs = length (ap_args ap) ->
      NoDup (myc :: mydep :: xf
               :: map fst cs ++ map fst ps_rem) ->
      NoDup (wf :: map snd cs) ->
      ~ free_vars (E_apply ap) g ->      (* g fresh *)
      stub_body
        = bind_projections fs' myc ((xf, wf) :: cs)
            (E_apply (Mk_apply
               (Some (simple_of_name (Name_var xf)))
               (map (fun c => simple_of_name (Name_var (fst c))) cs
                ++ params_as_args ps_rem)
               (RC_return kret')
               (Mk_exn_continuation kexn' [])
               ar_full ra_full (CK_function (FC_direct cid))
               am_full)) ->
      stub = Mk_code0 kret' kexn' ps_rem stub_body myc mydep
               am_c pa_c ra_c rm_c false ->
      rw_control C E (E_apply ap)
        (E_let (BPat_static [BSP_code cid'])
           (N_static_consts [SCC_code stub])
           (E_let
              (BPat_set_of_closures [Mk_bound_var g NM_normal])
              (N_set_of_closures
                 (Mk_set_of_closures
                    [(fs', FD_code_id cid' false)]
                    ((wf, f)
                     :: combine (map snd cs) (ap_args ap)))
                 am_soc)
              (E_apply_cont
                 (Mk_apply_cont kk [simple_of_name (Name_var g)]
                    None))))

(** RULE S.Rewrite.Apply.Invalid (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#replace_apply_by_invalid
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call *)
(* Either the callee's type is Bottom (Closure_type_was_invalid), or
   the proven code id is inconsistent with the call kind's code ids
   under code aging, or a direct callee's kinds provably mismatch
   its code (KF-025): some common-prefix parameter's unarized kinds
   disagree (arity_prefix_kind_mismatch above -- a plain arity-
   length difference is over/partial application, never Invalid),
   or, for an EXACT application only (equal non-unarized param
   counts, mirroring the code's provided_num_args = num_params
   guard), the return arities disagree after unarization and
   subkind erasure.  ENCODING NOTE: the doc conditions the third
   disjunct on kind checks being off (with them on, the mismatch is
   a fatal compiler error, not a rewrite); that flag is carried in
   prose. *)
| S_Rewrite_Apply_Invalid :
    forall ap f,
      ap_callee ap = Some f ->
      (type_is_bottom E (fst (type_simple_in_term E K_value f))
       \/
       (exists fc fs fts cts vts cid ri,
          ap_call_kind ap = CK_function fc
          /\ prove_single_closures_entry E f fs
               (Mk_closures_entry fts cts vts)
          /\ fts fs = Some (Ou_known (Mk_function_type cid ri))
          /\ ~ indirect_code_ids_consistent E cid fc)
       \/
       (exists cid cd,
          ap_call_kind ap = CK_function (FC_direct cid)
          /\ C cid = Some cd
          /\ (arity_prefix_kind_mismatch
                (c0_params_arity cd) (ap_args_arity ap)
              \/ (num_params (ap_args_arity ap)
                    = num_params (c0_params_arity cd)
                  /\ ~ equal_ignoring_subkinds
                        (unarize (ap_return_arity ap))
                        (unarize (c0_result_arity cd)))))) ->
      rw_control C E (E_apply ap)
        (E_invalid "Closure_type_was_invalid"%string)

(** RULE S.Rewrite.LetCont.Demote (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers *)
(* ENCODING NOTE: sort_handlers' full No_loop classification (a
   dependency-ordered rebuild of the whole group) is narrowed to its
   generating step: a handler that references no continuation of the
   group (in particular not itself) is in no SCC loop and is
   extracted as a non-recursive let_cont ahead of the residual group;
   iterated application peels acyclic chains.  This covers the chief
   client -- a loopified body whose self continuation lost its
   recursive uses (singleton group, no self-jump) collapses to the
   non-recursive form and then to LetCont.Inline.  The extracted
   handler must not use the group's invariant parameters (they stay
   with the group). *)
| S_Rewrite_LetCont_Demote :
    forall inv handlers k h e,
      In (k, h) handlers ->
      Forall
        (fun kh => count_cont_uses (fst kh) (ch_handler h) = 0)
        handlers ->
      Forall (fun p => ~ free_vars (ch_handler h) (fst p)) inv ->
      rw_control C E
        (E_let_cont_rec inv handlers e)
        (E_let_cont_nonrec k h
           (rebuild_rec_group inv
              (filter
                 (fun kh => negb (continuation_eqb (fst kh) k))
                 handlers)
              e))

(** RULE S.Rewrite.Invalid.Propagate (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/expr_builder.ml#rebuild_invalid
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr *)
(* The umbrella rule.  Its five listed sources are the specific
   constructors: Switch.Invalid, Let.Invalid, Apply.Invalid,
   LetCont.InvalidHandler (all above) and SPR.create_invalid
   (RewritesPrim.v).  ENCODING NOTE: the doc's five-source
   disjunction is encoded as the unifying license those sources
   instantiate: when the abstract domain proves the ambient
   environment unsatisfiable (te_is_bottom -- gamma_E is empty, no
   runtime state reaches e), any expression may be replaced by
   Invalid, with the message free.  A source Invalid { message } in
   the input is preserved as-is (no rewrite consumes it). *)
| S_Rewrite_Invalid_Propagate :
    forall e msg,
      te_is_bottom E = true ->
      rw_control C E e (E_invalid msg).

End RwControlRelation.

(* ================================================================== *)
(* 9. Documented anchors                                              *)
(* ================================================================== *)

(* Descriptive rules, and conjectured rules whose content is pass
   machinery (whole-pass guarantees over the upwards environment, the
   flow/dominator fixpoints, or the resimplification driver) rather
   than anything statable over rw_control's term-level premises
   (catalog entry 37's decision rule: rw_control-statable conjectured
   rules become Admitted theorems -- see ValueSlotExempt above --
   pass-machinery ones become anchors with their true STATUS). *)

(** RULE S.Rewrite.Let.Phantom (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let *)
(* With generate_phantom_lets on, a dead user-visible binding is kept
   at phantom name mode (for debuggers) instead of deleted by
   Let.DeadBinding.  Depends on the debug-info flag; phantom bindings
   have no runtime meaning (ch. 04). *)
Definition S_Rewrite_Let_Phantom_documented : Prop := True.

(** RULE S.Rewrite.LetCont.InlineForcesElimination (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind
    CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont *)
(* A must-disappear theorem about the whole upwards rebuild: a
   non-recursive, non-exn, non-cold k whose recorded uses list has
   length exactly 1 of kind Inlinable NEVER survives as a Let_cont in
   the output -- either the use site survives and is spliced
   (LetCont.Inline), or it dies during rebuild; both leave zero
   occurrences and rebuild_let_cont drops the binder.  The content is
   the agreement between the downwards use-list length and the
   upwards syntactic occurrence count, plus "shortcuts are never
   created for these continuations".  Pass machinery (CUE lists +
   rebuild order); anchored.  Composes: S.Rewrite.LetCont.Inline,
   S.Rewrite.LetCont.DeadHandler, S.Struct.SingleInlinableUse. *)
Definition S_Rewrite_LetCont_InlineForcesElimination_documented
  : Prop := True.

(** RULE S.Rewrite.LetCont.ShortcutFlat (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/continuation_shortcut.ml#apply
    CODE middle_end/flambda2/simplify/env/upwards_env.ml#add_continuation_shortcuts *)
(* Flatness invariant of the upwards environment's shortcut map: a
   registered shortcut's target is never itself a shortcut (by
   scoping-acyclicity and handlers-before-uses registration order),
   so single-step application achieves full path compression: an
   n-long trampoline chain collapses to direct jumps in one pass and
   the intermediates die by DeadHandler.  Non-local (a global-map
   invariant across the whole rebuild); anchored.  Known precision
   (not soundness) leak: retargeting can lose kind/subkind info of
   intermediate params (CR gbury).  Composes:
   S.Rewrite.LetCont.Shortcut, S.Rewrite.LetCont.DeadHandler,
   S.Rewrite.LetCont.InlineForcesElimination. *)
Definition S_Rewrite_LetCont_ShortcutFlat_documented : Prop := True.

(** RULE S.Rewrite.LetCont.Specialize (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#specialize_continuation_if_needed
    CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont *)
(* Per-use specialization of a multi-use non-recursive continuation
   (fresh handler copy per use, re-simplified under that use's
   types).  Heuristic and budget-limited (Specialization_cost);
   recursive continuations are never specialized. *)
Definition S_Rewrite_LetCont_Specialize_documented : Prop := True.

(** RULE S.Rewrite.Loopify.TailrecEmitsNonRecursive (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#create_apply
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body *)
(* Universal output guarantee: code entering Simplify (Normal mode)
   with Default_loopify_and_tailrec is rebuilt with my_depth not free
   in its body -- hence Non_recursive by Code.RecursiveRecompute --
   unconditionally, including through unrolling/inlining of the self
   tail calls (every my_closure occurrence is a redirectable tail
   callee or an exempt value-slot projection; induction on bounded
   unroll depth).  Quantifies over the whole pass (Simplify.v's
   closure, wave 6); anchored.  Composes: S.Rewrite.Loopify
   .Attribute, .Body, .SelfTailCall, S.Rewrite.Code
   .RecursiveRecompute. *)
Definition S_Rewrite_Loopify_TailrecEmitsNonRecursive_documented
  : Prop := True.

(** RULE S.Rewrite.Loopify.InvariantArgElim (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/flow/dominator_graph.ml#dominator_analysis
    CODE middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call *)
(* In a loopified body, a position-j argument that every self tail
   call passes as (an alias of) the function's j-th parameter is
   eliminated from the residual loop by the whole-body dominator
   fixpoint (entry use = pj, recursive uses = pj'; pj, edgeless,
   dominates), landing in the AliasedParam machinery -- impossible in
   Apply-recursive form.  The dominator analysis is pass machinery;
   the term-level residue is S.Rewrite.LetCont.AliasedParam above.
   Anchored.  Composes: S.Rewrite.Loopify.Body, .SelfTailCall,
   S.Struct.Flow.Aliases, S.Rewrite.LetCont.AliasedParam. *)
Definition S_Rewrite_Loopify_InvariantArgElim_documented
  : Prop := True.

(** RULE S.Rewrite.Loopify.SimplifyExposed (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0
    CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0 *)
(* Phase-ordering completeness boundary: a call that only BECOMES a
   self tail call during simplification is redirected iff
   should_loopify held at closure conversion ([@loop], or already
   purely tail-recursive).  NEGATIVE: an unmarked function never
   acquires a continuation loop from Simplify (loopify_state is set
   only by Loopify.Body; AttributeUpdate freezes the Default_*;
   single round).  POSITIVE: with [@loop], SelfTailCall's semantic
   canon test catches mid-pass-exposed calls.  A property of the
   pass's phase ordering; anchored.  Composes: S.Rewrite.Loopify
   .Body, .SelfTailCall, .AttributeUpdate. *)
Definition S_Rewrite_Loopify_SimplifyExposed_documented
  : Prop := True.

(** RULE S.Rewrite.Loopify.ResimplifyIdempotent (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers *)
(* Across simplify_function's bounded resimplification loop: run
   n+1's input attribute is run n's OUTPUT attribute, so only
   Always_loopify is ever re-wrapped (Default_loopify_and_tailrec
   became Already_loopified after run 1); a re-wrap exposing no new
   redirectable self call collapses exactly (Demote -> Inline); live
   wrappers are bounded by max_function_simplify_run.  A property of
   the resimplification driver; anchored.  Composes:
   S.Rewrite.Loopify.Body, S.Rewrite.LetCont.Demote,
   S.Rewrite.LetCont.Inline, S.Struct.Resimplify. *)
Definition S_Rewrite_Loopify_ResimplifyIdempotent_documented
  : Prop := True.
