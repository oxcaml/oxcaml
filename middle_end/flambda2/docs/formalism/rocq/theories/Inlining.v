(* Inlining.v -- ch. 11 (11-inlining.md): inlining.  All 10
   S.Inline.* rules.
   Owner: Girard (wave 4).
   Imports: Base, Syntax, TypeGrammar.

   Contents:
   - Section 1: the decision oracle -- verdict type and the Inline?
     Parameter (sanctioned oracle).
   - Section 2: the transformation -- rw_inline, the four normative
     rules (S.Inline.ModeMismatchInvalid / Substitute /
     Substitute.Region / Substitute.ExnExtraArgs).
   - Section 3: the oracle's internals and the unrolling machinery --
     six descriptive anchors (the doc itself marks sections 2-3
     descriptive: "they document the current heuristic and its knobs,
     which the code may legitimately change"). *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax TypeGrammar PrimMemoryA.
Import ListNotations.

Local Open Scope Z_scope.

(* ================================================================== *)
(* 1. The decision oracle                                             *)
(* ================================================================== *)

(* The oracle's verdict, collapsed exactly as
   Call_site_inlining_decision_type.can_inline collapses the rich
   decision type: keep, or inline { unroll_to; was_inline_always }.
   unroll_to = Some n is S.Inline.Unroll.Begin's step 1; None is the
   ordinary case. *)
Inductive inline_verdict :=
  IV_keep
| IV_inline (unroll_to : option Z) (was_inline_always : bool).

(* The doc's Inline?(apply, code_metadata(cid), E).  Sanctioned
   oracle Parameter (CORRESPONDENCE encoding table): chs. 11 secs.
   2-3 document the decision procedure descriptively (anchors in
   section 3 below); the model treats the verdict as opaque.  The
   code-metadata argument is folded into the (apply, E) pair: the
   metadata is obtained from the callee's code id, which the apply
   determines in the environment. *)
Parameter inline_dec : tenv -> apply_expr -> inline_verdict.

(* ================================================================== *)
(* 2. The transformation (rw_inline)                                  *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(* 2.1 Renaming machinery                                             *)
(* ------------------------------------------------------------------ *)

(* ENCODING NOTE: theta and rho_region are genuine renamings on the
   nominal syntax (S.Inline.Substitute NOTES: "theta renames only
   continuations and region variables"; CORRESPONDENCE, Binders).
   rename_cont/rename_var below are TOTAL naive renames: they rename
   every occurrence, binding occurrences included.  Under the
   freshness premises the rw_inline constructors carry -- the TARGET
   name has no occurrence at all in the callee body -- a total rename
   coincides with capture-avoiding substitution for the free
   occurrences of the source: bound occurrences of the source are
   alpha-renamed consistently along with their binders, and no binder
   can capture the target.  Variable-target freshness is stated with
   the sanctioned free_vars/bound_vars Parameters; continuation-target
   freshness needs raw non-occurrence, which free_conts alone cannot
   express (a BOUND occurrence of the target inside the body would
   capture), so cont_occurs below computes raw syntactic occurrence.
   It is a file-local boolean, not a new nominal Parameter: raw
   occurrence is binder-insensitive, so opacity buys nothing. *)

Section RenameCont.

Variable k_from k_to : continuation.

Definition rnc (k : continuation) : continuation :=
  if continuation_eqb k k_from then k_to else k.

Definition rnc_result (rc : result_continuation)
    : result_continuation :=
  match rc with
  | RC_return k => RC_return (rnc k)
  | RC_never_returns => RC_never_returns
  end.

Definition rnc_exn (ec : exn_continuation) : exn_continuation :=
  Mk_exn_continuation (rnc (ec_exn_handler ec)) (ec_extra_args ec).

Definition rnc_trap (t : trap_action) : trap_action :=
  match t with
  | Trap_push k => Trap_push (rnc k)
  | Trap_pop k rk => Trap_pop (rnc k) rk
  end.

Definition rnc_apply_cont (ac : apply_cont_expr) : apply_cont_expr :=
  Mk_apply_cont (rnc (ac_continuation ac)) (ac_args ac)
    (option_map rnc_trap (ac_trap_action ac)).

Definition rnc_switch (sw : switch_expr) : switch_expr :=
  Mk_switch (sw_scrutinee sw)
    (map (fun arm => (fst arm, rnc_apply_cont (snd arm)))
       (sw_arms sw)).

Definition rnc_apply (ap : apply_expr) : apply_expr :=
  Mk_apply (ap_callee ap) (ap_args ap)
    (rnc_result (ap_result_continuation ap))
    (rnc_exn (ap_exn_continuation ap))
    (ap_args_arity ap) (ap_return_arity ap)
    (ap_call_kind ap) (ap_alloc_mode ap).

Fixpoint rename_cont (e : expr) : expr :=
  match e with
  | E_let p n body => E_let p (rnc_named n) (rename_cont body)
  | E_let_cont_nonrec k h body =>
      E_let_cont_nonrec (rnc k) (rnc_handler h) (rename_cont body)
  | E_let_cont_rec inv hs body =>
      E_let_cont_rec inv
        ((fix go (l : list (continuation * cont_handler)) :=
            match l with
            | [] => []
            | kh :: tl =>
                (rnc (fst kh), rnc_handler (snd kh)) :: go tl
            end) hs)
        (rename_cont body)
  | E_apply ap => E_apply (rnc_apply ap)
  | E_apply_cont ac => E_apply_cont (rnc_apply_cont ac)
  | E_switch sw => E_switch (rnc_switch sw)
  | E_invalid m => E_invalid m
  end

with rnc_named (n : named) : named :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ => n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => rnc_scc s :: go tl
            end) scg)
  end

with rnc_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (rename_cont body) exn cold
  end

with rnc_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (rnc_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with rnc_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      Mk_code0 (rnc kret) (rnc kexn) ps (rename_cont body)
        clo dep am pa ra rm tup
  end.

End RenameCont.

Section ContOccurs.

Variable k0 : continuation.

Definition co_k (k : continuation) : bool := continuation_eqb k k0.

Definition co_trap (t : trap_action) : bool :=
  match t with
  | Trap_push k => co_k k
  | Trap_pop k _ => co_k k
  end.

Definition co_apply_cont (ac : apply_cont_expr) : bool :=
  co_k (ac_continuation ac)
  || match ac_trap_action ac with
     | Some t => co_trap t
     | None => false
     end.

Definition co_switch (sw : switch_expr) : bool :=
  existsb (fun arm => co_apply_cont (snd arm)) (sw_arms sw).

Definition co_result (rc : result_continuation) : bool :=
  match rc with
  | RC_return k => co_k k
  | RC_never_returns => false
  end.

Definition co_apply (ap : apply_expr) : bool :=
  co_result (ap_result_continuation ap)
  || co_k (ec_exn_handler (ap_exn_continuation ap)).

Fixpoint cont_occurs (e : expr) : bool :=
  match e with
  | E_let _ n body => co_named n || cont_occurs body
  | E_let_cont_nonrec k h body =>
      co_k k || co_handler h || cont_occurs body
  | E_let_cont_rec _ hs body =>
      (fix go (l : list (continuation * cont_handler)) : bool :=
         match l with
         | [] => false
         | kh :: tl =>
             co_k (fst kh) || co_handler (snd kh) || go tl
         end) hs
      || cont_occurs body
  | E_apply ap => co_apply ap
  | E_apply_cont ac => co_apply_cont ac
  | E_switch sw => co_switch sw
  | E_invalid _ => false
  end

with co_named (n : named) : bool :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ =>
      false
  | N_static_consts scg =>
      (fix go (l : list static_const_or_code) : bool :=
         match l with
         | [] => false
         | s :: tl => co_scc s || go tl
         end) scg
  end

with co_handler (h : cont_handler) : bool :=
  match h with
  | Mk_cont_handler _ body _ _ => cont_occurs body
  end

with co_scc (s : static_const_or_code) : bool :=
  match s with
  | SCC_code c => co_code c
  | SCC_deleted_code | SCC_static_const _ => false
  end

with co_code (c : code0) : bool :=
  match c with
  | Mk_code0 kret kexn _ body _ _ _ _ _ _ _ =>
      co_k kret || co_k kexn || cont_occurs body
  end.

End ContOccurs.

Section RenameVar.

Variable x_from x_to : variable.

Definition rnv (x : variable) : variable :=
  if variable_eqb x x_from then x_to else x.

Definition rnv_name (n : name) : name :=
  match n with
  | Name_var x => Name_var (rnv x)
  | Name_sym _ => n
  end.

Fixpoint rnv_ri (d : rec_info_expr) : rec_info_expr :=
  match d with
  | RI_const _ _ => d
  | RI_var x => RI_var (rnv x)
  | RI_succ d' => RI_succ (rnv_ri d')
  | RI_unroll_to n d' => RI_unroll_to n (rnv_ri d')
  end.

Definition rnv_coercion (co : coercion) : coercion :=
  match co with
  | Coercion_id => co
  | Coercion_change_depth f t =>
      Coercion_change_depth (rnv_ri f) (rnv_ri t)
  end.

Definition rnv_simple (s : simple) : simple :=
  match s with
  | Simple_name n co => Simple_name (rnv_name n) (rnv_coercion co)
  | Simple_const _ => s
  end.

Definition rnv_alloc (am : alloc_mode_alloc) : alloc_mode_alloc :=
  match am with
  | Alloc_heap => am
  | Alloc_local r => Alloc_local (rnv r)
  end.

Definition rnv_alloc_app (am : alloc_mode_app) : alloc_mode_app :=
  match am with
  | App_heap => am
  | App_local r g => App_local (rnv r) (rnv g)
  end.

(* Only two unary and two variadic descriptors carry a variable
   (their allocation mode's region); every other descriptor payload
   is a variable-free enum/kind. *)
Definition rnv_unary_op (op : unary_primitive) : unary_primitive :=
  match op with
  | UP_int_as_pointer am => UP_int_as_pointer (rnv_alloc am)
  | UP_box_number bn am => UP_box_number bn (rnv_alloc am)
  | _ => op
  end.

Definition rnv_variadic_op (op : variadic_primitive)
    : variadic_primitive :=
  match op with
  | VP_make_block bk mut am => VP_make_block bk mut (rnv_alloc am)
  | VP_make_array ak mut am => VP_make_array ak mut (rnv_alloc am)
  | _ => op
  end.

Definition rnv_prim (p : prim) : prim :=
  match p with
  | P_nullary _ => p
  | P_unary op s => P_unary (rnv_unary_op op) (rnv_simple s)
  | P_binary op s1 s2 =>
      P_binary op (rnv_simple s1) (rnv_simple s2)
  | P_ternary op s1 s2 s3 =>
      P_ternary op (rnv_simple s1) (rnv_simple s2) (rnv_simple s3)
  | P_quaternary op s1 s2 s3 s4 =>
      P_quaternary op (rnv_simple s1) (rnv_simple s2)
        (rnv_simple s3) (rnv_simple s4)
  | P_variadic op args =>
      P_variadic (rnv_variadic_op op) (map rnv_simple args)
  end.

Definition rnv_effect (eo : effect_op) : effect_op :=
  match eo with
  | Eff_perform e => Eff_perform (rnv_simple e)
  | Eff_reperform e c lf =>
      Eff_reperform (rnv_simple e) (rnv_simple c) (rnv_simple lf)
  | Eff_with_stack vc xc ec f a =>
      Eff_with_stack (rnv_simple vc) (rnv_simple xc)
        (rnv_simple ec) (rnv_simple f) (rnv_simple a)
  | Eff_with_stack_preemptible vc xc ec ht f a =>
      Eff_with_stack_preemptible (rnv_simple vc) (rnv_simple xc)
        (rnv_simple ec) (rnv_simple ht) (rnv_simple f)
        (rnv_simple a)
  | Eff_resume c f a =>
      Eff_resume (rnv_simple c) (rnv_simple f) (rnv_simple a)
  end.

Definition rnv_call_kind (ck : call_kind) : call_kind :=
  match ck with
  | CK_function _ | CK_c_call _ _ _ _ => ck
  | CK_method mk obj => CK_method mk (rnv_simple obj)
  | CK_effect eo => CK_effect (rnv_effect eo)
  end.

Definition rnv_exn (ec : exn_continuation) : exn_continuation :=
  Mk_exn_continuation (ec_exn_handler ec)
    (map (fun a => (rnv_simple (fst a), snd a)) (ec_extra_args ec)).

Definition rnv_apply_cont (ac : apply_cont_expr) : apply_cont_expr :=
  Mk_apply_cont (ac_continuation ac)
    (map rnv_simple (ac_args ac)) (ac_trap_action ac).

Definition rnv_switch (sw : switch_expr) : switch_expr :=
  Mk_switch (rnv_simple (sw_scrutinee sw))
    (map (fun arm => (fst arm, rnv_apply_cont (snd arm)))
       (sw_arms sw)).

Definition rnv_apply (ap : apply_expr) : apply_expr :=
  Mk_apply (option_map rnv_simple (ap_callee ap))
    (map rnv_simple (ap_args ap))
    (ap_result_continuation ap)
    (rnv_exn (ap_exn_continuation ap))
    (ap_args_arity ap) (ap_return_arity ap)
    (rnv_call_kind (ap_call_kind ap))
    (rnv_alloc_app (ap_alloc_mode ap)).

Definition rnv_soc (soc : set_of_closures) : set_of_closures :=
  Mk_set_of_closures (soc_function_decls soc)
    (map (fun vs => (fst vs, rnv_simple (snd vs)))
       (soc_value_slots soc)).

Definition rnv_ov {A} (v : or_variable A) : or_variable A :=
  match v with
  | OV_const _ => v
  | OV_var x => OV_var (rnv x)
  end.

Definition rnv_static_const (sc : static_const) : static_const :=
  match sc with
  | SC_set_of_closures soc => SC_set_of_closures (rnv_soc soc)
  | SC_block t mut shape fields =>
      SC_block t mut shape (map rnv_simple fields)
  | SC_boxed_float32 v => SC_boxed_float32 (rnv_ov v)
  | SC_boxed_float v => SC_boxed_float (rnv_ov v)
  | SC_boxed_int32 v => SC_boxed_int32 (rnv_ov v)
  | SC_boxed_int64 v => SC_boxed_int64 (rnv_ov v)
  | SC_boxed_nativeint v => SC_boxed_nativeint (rnv_ov v)
  | SC_boxed_vec128 v => SC_boxed_vec128 (rnv_ov v)
  | SC_boxed_vec256 v => SC_boxed_vec256 (rnv_ov v)
  | SC_boxed_vec512 v => SC_boxed_vec512 (rnv_ov v)
  | SC_immutable_float_block fs =>
      SC_immutable_float_block (map rnv_ov fs)
  | SC_immutable_float_array fs =>
      SC_immutable_float_array (map rnv_ov fs)
  | SC_immutable_float32_array fs =>
      SC_immutable_float32_array (map rnv_ov fs)
  | SC_immutable_int_array fs =>
      SC_immutable_int_array (map rnv_ov fs)
  | SC_immutable_int8_array fs =>
      SC_immutable_int8_array (map rnv_ov fs)
  | SC_immutable_int16_array fs =>
      SC_immutable_int16_array (map rnv_ov fs)
  | SC_immutable_int32_array fs =>
      SC_immutable_int32_array (map rnv_ov fs)
  | SC_immutable_int64_array fs =>
      SC_immutable_int64_array (map rnv_ov fs)
  | SC_immutable_nativeint_array fs =>
      SC_immutable_nativeint_array (map rnv_ov fs)
  | SC_immutable_vec128_array fs =>
      SC_immutable_vec128_array (map rnv_ov fs)
  | SC_immutable_vec256_array fs =>
      SC_immutable_vec256_array (map rnv_ov fs)
  | SC_immutable_vec512_array fs =>
      SC_immutable_vec512_array (map rnv_ov fs)
  | SC_immutable_value_array fs =>
      SC_immutable_value_array (map rnv_simple fs)
  | SC_empty_array _ | SC_mutable_string _
  | SC_immutable_string _ => sc
  end.

Definition rnv_bound_var (bv : bound_var) : bound_var :=
  Mk_bound_var (rnv (bv_var bv)) (bv_name_mode bv).

Definition rnv_pattern (p : bound_pattern) : bound_pattern :=
  match p with
  | BPat_singleton bv => BPat_singleton (rnv_bound_var bv)
  | BPat_set_of_closures bvs =>
      BPat_set_of_closures (map rnv_bound_var bvs)
  | BPat_static _ => p
  end.

Definition rnv_params (ps : list (variable * kind_ws))
    : list (variable * kind_ws) :=
  map (fun p => (rnv (fst p), snd p)) ps.

Fixpoint rename_var (e : expr) : expr :=
  match e with
  | E_let p n body =>
      E_let (rnv_pattern p) (rnv_named n) (rename_var body)
  | E_let_cont_nonrec k h body =>
      E_let_cont_nonrec k (rnv_handler h) (rename_var body)
  | E_let_cont_rec inv hs body =>
      E_let_cont_rec (rnv_params inv)
        ((fix go (l : list (continuation * cont_handler)) :=
            match l with
            | [] => []
            | kh :: tl => (fst kh, rnv_handler (snd kh)) :: go tl
            end) hs)
        (rename_var body)
  | E_apply ap => E_apply (rnv_apply ap)
  | E_apply_cont ac => E_apply_cont (rnv_apply_cont ac)
  | E_switch sw => E_switch (rnv_switch sw)
  | E_invalid _ => e
  end

with rnv_named (n : named) : named :=
  match n with
  | N_simple s => N_simple (rnv_simple s)
  | N_prim p => N_prim (rnv_prim p)
  | N_set_of_closures soc am =>
      N_set_of_closures (rnv_soc soc) (rnv_alloc am)
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => rnv_scc s :: go tl
            end) scg)
  | N_rec_info ri => N_rec_info (rnv_ri ri)
  end

with rnv_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler (rnv_params ps) (rename_var body) exn cold
  end

with rnv_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (rnv_code c)
  | SCC_deleted_code => s
  | SCC_static_const sc => SCC_static_const (rnv_static_const sc)
  end

with rnv_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      Mk_code0 kret kexn (rnv_params ps) (rename_var body)
        (rnv clo) (rnv dep) (rnv_alloc_app am) pa ra rm tup
  end.

End RenameVar.

(* ------------------------------------------------------------------ *)
(* 2.2 The right-hand side of the substitution                        *)
(* ------------------------------------------------------------------ *)

Definition var_simple (x : variable) : simple :=
  Simple_name (Name_var x) Coercion_id.

(* let x = s in body (a normal-mode singleton let of a simple). *)
Definition let_simple (x : variable) (s : simple) (body : expr)
    : expr :=
  E_let (BPat_singleton (Mk_bound_var x NM_normal)) (N_simple s)
    body.

(* let x1 = s1 in ... let xn = sn in body: pairs the params with the
   argument simples positionally (parameters innermost). *)
Definition bind_params (params : list (variable * kind_ws))
    (args : list simple) (body : expr) : expr :=
  fold_right (fun pa acc => let_simple (fst (fst pa)) (snd pa) acc)
    body (combine params args).

(* Absent callee => no x_myclos let (doc, "Callee binding and
   depth"). *)
Definition bind_callee (myclos : variable) (callee : option simple)
    (body : expr) : expr :=
  match callee with
  | Some s => let_simple myclos s body
  | None => body
  end.

(* do_not_inline = Const{depth = infinity; unrolling = Do_not_unroll}
   (doc section 3, distinguished constants). *)
Definition do_not_inline : rec_info_expr :=
  RI_const Inf Do_not_unroll.

(* Absent callee => my_depth is bound to do_not_inline, preventing
   further inlining of self-calls in the copied body. *)
Definition depth_for (callee : option simple) (d0 : rec_info_expr)
    : rec_info_expr :=
  match callee with
  | Some _ => d0
  | None => do_not_inline
  end.

Definition let_depth (mydepth : variable) (d : rec_info_expr)
    (body : expr) : expr :=
  E_let (BPat_singleton (Mk_bound_var mydepth NM_normal))
    (N_rec_info d) body.

(** RULE S.Inline.Substitute.Region (CLAIM normative)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body
    rho_region, by cases on (alpha, alpha^c) in doc order: empty if
    alpha = Heap; [x_myregion |-> region, x_myghost |-> ghost_region]
    if alpha = Local { region; ghost_region } and alpha^c =
    Local { x_myregion; x_myghost }; empty if alpha = Local and
    alpha^c = Heap.  A genuine renaming, not a let-binding: the
    callee's region variables are known fresh for e_body, so a
    permutation cannot capture an existing occurrence (the rw_inline
    constructors carry the corresponding freshness premises for the
    caller-side targets). *)
Definition region_rename (alpha alpha_c : alloc_mode_app) (e : expr)
    : expr :=
  match alpha, alpha_c with
  | App_local region ghost, App_local x_myregion x_myghost =>
      rename_var x_myghost ghost
        (rename_var x_myregion region e)
  | _, _ => e
  end.

(* theta(e_body) with the depth/callee/parameter lets wrapped around
   it: the complete right-hand side of S.Inline.Substitute.
   kret_target = Some k encodes kappa_ret = Return k; None encodes
   Never_returns (theta's k_ret^c clause is empty). *)
Definition inlined_body (code : code0) (callee : option simple)
    (args : list simple) (d0 : rec_info_expr)
    (alpha : alloc_mode_app) (kret_target : option continuation)
    (kexn_target : continuation) : expr :=
  let body_r :=
    region_rename alpha (c0_my_alloc_mode code) (c0_body code) in
  let body_e :=
    rename_cont (c0_exn_continuation code) kexn_target body_r in
  let body_t :=
    match kret_target with
    | Some k => rename_cont (c0_return_continuation code) k body_e
    | None => body_e
    end in
  let_depth (c0_my_depth code) (depth_for callee d0)
    (bind_callee (c0_my_closure code) callee
       (bind_params (c0_params code) args body_t)).

(* Capture-avoidance freshness (see the 2.1 ENCODING NOTE). *)

Definition var_fresh_in (e : expr) (x : variable) : Prop :=
  ~ free_vars e x /\ ~ bound_vars e x.

Definition region_targets_fresh (alpha alpha_c : alloc_mode_app)
    (e : expr) : Prop :=
  match alpha, alpha_c with
  | App_local region ghost, App_local _ _ =>
      var_fresh_in e region /\ var_fresh_in e ghost
  | _, _ => True
  end.

Definition cont_target_fresh (kt : option continuation) (e : expr)
    : Prop :=
  match kt with
  | Some k => cont_occurs k e = false
  | None => True
  end.

(* kappa_ret as a renaming target. *)
Definition ret_target (rc : result_continuation)
    : option continuation :=
  match rc with
  | RC_return k => Some k
  | RC_never_returns => None
  end.

(* The k_pop wrapper of S.Inline.Substitute.ExnExtraArgs; omitted
   when kappa_ret = Never_returns (rule NOTES). *)
Definition exn_wrap_ret (rc : result_continuation)
    (k1 k_pop : continuation) (rs : list (variable * kind_ws))
    (inner : expr) : expr :=
  match rc with
  | RC_return k =>
      E_let_cont_nonrec k_pop
        (Mk_cont_handler rs
           (E_apply_cont (Mk_apply_cont k
              (map (fun r => var_simple (fst r)) rs)
              (Some (Trap_pop k1 None))))
           false false)
        inner
  | RC_never_returns => inner
  end.

(* Inside the wrapper the body's return target is k_pop (when there
   is one). *)
Definition pop_target (rc : result_continuation)
    (k_pop : continuation) : option continuation :=
  match rc with
  | RC_return _ => Some k_pop
  | RC_never_returns => None
  end.

(* A simple mentions x iff it is Name_var x under some coercion. *)
Definition simple_mentions_var (x : variable) (s : simple) : Prop :=
  match s with
  | Simple_name (Name_var y) _ => y = x
  | _ => False
  end.

(* Coercion-aware occurrence in a simple: a variable can also reach
   a caller-side simple through the rec-info payloads of a
   Change_depth coercion (RI_var), which simple_mentions_var does
   not see. *)
Fixpoint ri_mentions_var (x : variable) (ri : rec_info_expr)
    : Prop :=
  match ri with
  | RI_const _ _ => False
  | RI_var y => y = x
  | RI_succ ri' => ri_mentions_var x ri'
  | RI_unroll_to _ ri' => ri_mentions_var x ri'
  end.

Definition simple_uses_var (x : variable) (s : simple) : Prop :=
  match s with
  | Simple_name n co =>
      match n with
      | Name_var y => y = x
      | _ => False
      end
      \/ match co with
         | Coercion_id => False
         | Coercion_change_depth f t =>
             ri_mentions_var x f \/ ri_mentions_var x t
         end
  | Simple_const _ => False
  end.

(* The callee binder names that inlined_body's wrapper lets bind
   around caller-side material (x_mydepth outermost needs no check
   against d0, which sits at its own binding position). *)
Definition callee_binders (code : code0) : list variable :=
  c0_my_depth code :: c0_my_closure code
    :: map fst (c0_params code).

(* Capture-avoidance for the wrapper lets: no callee binder occurs
   in the caller's callee/argument simples (coercion payloads
   included), nor equals a caller region variable -- region_rename
   writes region/ghost into the body, where an unused parameter's
   let would capture them (free_vars cannot block that: an unused
   parameter is not free in the pre-rename body).  Over-approximates
   the per-position scoping (argument s_i is only capturable by
   binders left of position i): uniform freshness is simpler and
   loses no doc instances (same caveat as the 2.1 ENCODING NOTE). *)
Definition callee_binders_fresh (code : code0)
    (callee : option simple) (args : list simple)
    (alpha : alloc_mode_app) : Prop :=
  Forall (fun b =>
      (forall s, callee = Some s -> ~ simple_uses_var b s)
      /\ Forall (fun s => ~ simple_uses_var b s) args
      /\ match alpha with
         | App_local region ghost => b <> region /\ b <> ghost
         | _ => True
         end)
    (callee_binders code).

(* ------------------------------------------------------------------ *)
(* 2.3 The rewrite relation                                           *)
(* ------------------------------------------------------------------ *)

(* Typed with the leading code_env per the approved frozen-interface
   amendment (all rw_* relations share this shape): code is defined
   BY the unit under compilation (its SCC_code bindings), not by an
   external oracle, and ch. 13 ties the quantified code_env to those
   bindings by a WF premise.  tenv (ch. 07's object) rightly excludes
   code, mirroring Simplify's denv carrying code separately from the
   types env.
   ENCODING NOTE: per the fifth frozen-interface amendment all rw_*
   relations share the closed type
   eff_flags -> code_env -> tenv -> expr -> expr -> Prop (the ambient
   effect-flag setting the pass runs under). No ch. 11 rule reads the
   flags, so [flags] is bound as a direct (unused) inductive parameter
   rather than a Section Variable: Rocq prunes section variables that
   do not occur in an Inductive, which would silently drop the
   argument from the closed type here.
   Callee code well-formedness (kret <> kexn, NoDup parameter names,
   body free conts bounded by the two) is deliberately NOT premised
   in these constructors: the doc discharges it as syntax WF, so it
   lives in WellFormed.v's code0_wf, conjoined by ch. 13's WF
   hypothesis tying C to the unit's SCC_code bindings. *)
Inductive rw_inline (flags : eff_flags) (C : code_env) (E : tenv)
    : expr -> expr -> Prop :=

(** RULE S.Inline.ModeMismatchInvalid (CLAIM normative)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline
    Premises in doc order: call kind Function, direct to cid;
    code(cid) available; alloc_mode(apply) = Heap;
    result_mode(code(cid)) = Alloc_local.
    ENCODING NOTE: the doc's Invalid payload
    (Calling_local_returning_closure_with_normal_apply apply) is a
    diagnostic; E_invalid carries an uninterpreted message string, so
    the constructor permits any message. *)
| RW_Inline_ModeMismatchInvalid :
    forall ap cid code msg,
      ap_call_kind ap = CK_function (FC_direct cid) ->
      C cid = Some code ->
      ap_alloc_mode ap = App_heap ->
      c0_result_mode code = LM_local ->
      rw_inline flags C E (E_apply ap) (E_invalid msg)

(** RULE S.Inline.Substitute (CLAIM normative)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
    CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
    VERIFIED 14-validation/new-05-inline-fold.md
    Premises in doc order: the apply (call kind Function, direct to
    cid); code(cid) binds <k_ret^c, k_exn^c, xbar, x_myclos, alpha^c,
    x_mydepth, e_body> (the c0_* accessors of [code]); the oracle
    verdict inline{unroll_to = bottom, was_inline_always}; theta
    (computed by [inlined_body]: k_ret^c |-> k iff kappa_ret =
    Return k, k_exn^c |-> exn_handler(kappa_exn), then rho_region).
    Encoding-side premises follow: full application (the section-1
    preamble: "the arity matches exactly"); empty exn extra args
    (the nonempty case is S.Inline.Substitute.ExnExtraArgs); the
    mode-mismatch combination excluded (ModeMismatchInvalid NOTES:
    "the remaining three combinations ... proceed to
    S.Inline.Substitute"); capture-avoidance freshness for the
    renaming targets (2.1 ENCODING NOTE); capture-avoidance for the
    wrapper-let binders against the caller's simples and region
    variables (callee_binders_fresh); and the callee's exn target
    distinct from k_ret^c -- inlined_body applies the exn rename
    before the ret rename, so if the exn target WERE k_ret^c the two
    sequential renames would chain (raises rerouted to the return
    target), where the doc's theta is simultaneous.
    ENCODING NOTE: d0 -- the callee's rec-info as known at the call
    site, "obtained by meeting the closure type's rec-info in the
    environment" -- is left unconstrained (universally quantified):
    the meeting procedure is section-3 descriptive machinery folded
    into the oracle side, and the substitution's meaning does not
    depend on which rec-info it produced.  The unroll_to = Some n
    verdict (S.Inline.Unroll.Begin, descriptive) is not encoded. *)
| RW_Inline_Substitute :
    forall ap cid code was_ia d0 kret_t,
      ap_call_kind ap = CK_function (FC_direct cid) ->
      C cid = Some code ->
      inline_dec E ap = IV_inline None was_ia ->
      kret_t = ret_target (ap_result_continuation ap) ->
      length (ap_args ap) = length (c0_params code) ->
      ec_extra_args (ap_exn_continuation ap) = [] ->
      ~ (ap_alloc_mode ap = App_heap /\
         c0_result_mode code = LM_local) ->
      cont_target_fresh kret_t (c0_body code) ->
      cont_occurs (ec_exn_handler (ap_exn_continuation ap))
        (c0_body code) = false ->
      ec_exn_handler (ap_exn_continuation ap)
        <> c0_return_continuation code ->
      region_targets_fresh (ap_alloc_mode ap)
        (c0_my_alloc_mode code) (c0_body code) ->
      callee_binders_fresh code (ap_callee ap) (ap_args ap)
        (ap_alloc_mode ap) ->
      rw_inline flags C E (E_apply ap)
        (inlined_body code (ap_callee ap) (ap_args ap) d0
           (ap_alloc_mode ap) kret_t
           (ec_exn_handler (ap_exn_continuation ap)))

(** RULE S.Inline.Substitute.ExnExtraArgs (CLAIM normative)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#wrap_inlined_body_for_exn_extra_args
    Premises in doc order: the oracle verdict inline;
    extra_args(kappa_exn) = abar, nonempty; k1 fresh (the wrapper exn
    handler), k_pop fresh, k_push fresh.  "Fresh" is spelled as raw
    non-occurrence in e_body plus pairwise distinctness plus
    distinctness from the original handler, from k_ret^c (as in
    RW_Inline_Substitute: k1 is the exn rename's target, applied
    before the ret rename), and from the caller's return
    continuation k when kappa_ret = Return k (the k_pop handler's
    jump to k sits inside k1's binder scope) (a local rule can state
    freshness only against the names it can see; same caveat as the
    2.1 ENCODING NOTE).  The wrapper's own binders are fresh too:
    x_exn (the k1 handler's exception parameter, kind Value) must not
    collide with the extra-args simples, and the k_pop parameters
    rbar carry the unarized return arity, pairwise distinct.  The
    remaining premises are shared with RW_Inline_Substitute, since
    this constructor performs the whole substitution with
    kappa_exn |-> k1 and kappa_ret |-> Return k_pop (via pop_target);
    when kappa_ret = Never_returns the k_pop wrapper is omitted
    (exn_wrap_ret, rule NOTES).  The is_cold flags on the wrapper
    handlers are false (optimizer hints, not semantic); k1's handler
    is marked is_exn_handler. *)
| RW_Inline_Substitute_ExnExtraArgs :
    forall ap cid code was_ia d0 abar k1 k_pop k_push x_exn rs,
      ap_call_kind ap = CK_function (FC_direct cid) ->
      C cid = Some code ->
      inline_dec E ap = IV_inline None was_ia ->
      ec_extra_args (ap_exn_continuation ap) = abar ->
      abar <> [] ->
      cont_occurs k1 (c0_body code) = false ->
      cont_occurs k_pop (c0_body code) = false ->
      cont_occurs k_push (c0_body code) = false ->
      k1 <> k_pop -> k1 <> k_push -> k_pop <> k_push ->
      k1 <> ec_exn_handler (ap_exn_continuation ap) ->
      k1 <> c0_return_continuation code ->
      (forall k,
         ap_result_continuation ap = RC_return k -> k1 <> k) ->
      Forall (fun a => ~ simple_mentions_var x_exn (fst a)) abar ->
      map snd rs = unarize (ap_return_arity ap) ->
      NoDup (map fst rs) ->
      length (ap_args ap) = length (c0_params code) ->
      ~ (ap_alloc_mode ap = App_heap /\
         c0_result_mode code = LM_local) ->
      region_targets_fresh (ap_alloc_mode ap)
        (c0_my_alloc_mode code) (c0_body code) ->
      callee_binders_fresh code (ap_callee ap) (ap_args ap)
        (ap_alloc_mode ap) ->
      rw_inline flags C E (E_apply ap)
        (E_let_cont_nonrec k1
           (Mk_cont_handler [(x_exn, ws_of_kind K_value)]
              (E_apply_cont (Mk_apply_cont
                 (ec_exn_handler (ap_exn_continuation ap))
                 (var_simple x_exn :: map fst abar)
                 (Some (Trap_pop
                          (ec_exn_handler (ap_exn_continuation ap))
                          (Some RK_reraise)))))
              true false)
           (exn_wrap_ret (ap_result_continuation ap) k1 k_pop rs
              (E_let_cont_nonrec k_push
                 (Mk_cont_handler []
                    (inlined_body code (ap_callee ap) (ap_args ap)
                       d0 (ap_alloc_mode ap)
                       (pop_target (ap_result_continuation ap)
                          k_pop)
                       k1)
                    false false)
                 (E_apply_cont
                    (Mk_apply_cont k_push []
                       (Some (Trap_push k1))))))).

(* ================================================================== *)
(* 3. Oracle internals and unrolling (doc sections 2-3)               *)
(* ================================================================== *)

(** RULE S.Inline.DeclDecision (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify_shared/function_decl_inlining_decision.ml#make_decision0
    CODE middle_end/flambda2/terms/function_decl_inlining_decision_type.ml#behaviour
    VERIFIED 14-validation/code_size_of_single_arg_switch.md

    Per-declaration classification, computed once when the code is
    defined, from the inline attribute, stub flag, cost metrics
    (size), functor flag, recursive flag, and the inlining arguments
    in force at the definition: Never_inline attribute => cannot;
    Always_inline => must; stub => must; recursive without unrolling
    or -flambda2-expert-can-inline-recursive-functions => cannot;
    size >= large_size (functor thresholds when is_a_functor) without
    Available_inline => cannot; size <= small_size => must;
    otherwise Speculatively_inlinable (could).  A declaration
    classified cannot is not exported in the .cmx
    (S.Inline.Decision's Missing_code path is the cross-module
    consequence).  Anchor: heuristic knobs, folded into the
    inline_dec oracle. *)
Definition S_Inline_DeclDecision_documented : Prop := True.

(** RULE S.Inline.Decision (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision
    CODE middle_end/flambda2/simplify_shared/call_site_inlining_decision_type.ml#can_inline
    VERIFIED 14-validation/missing_code.md

    The per-call-site procedure, short-circuiting in order: jsir mode
    => keep; Never_inlined attribute => keep; code missing /
    metadata-only => keep (Missing_code -- the only cross-module-
    specific outcome, doc section 5); remaining unrolling depth of
    the call-site rec-info d known: 0 => keep, positive => inline
    (S.Inline.Unroll.Continue); unrolling state unknown:
    inlining-state depth exceeded => keep; then by attribute policy
    (Default => heuristic; Unroll n => begin unrolling if can_unroll;
    Always/Hint => unconditional for non-recursive, Unroll 1 for
    recursive), the heuristic path checking the recursion-depth
    bound, the replay history, and finally might_inline
    (S.Inline.Speculative).  Anchor: the procedure IS inline_dec's
    implementation; the model keeps only the verdict. *)
Definition S_Inline_Decision_documented : Prop := True.

(** RULE S.Inline.Speculative (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#might_inline
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining
    CODE middle_end/flambda2/terms/cost_metrics.ml#evaluate
    VERIFIED 14-validation/inlining_cost_of_primitive_on_parameters.md
    VERIFIED 14-validation/removed_operations_of_switch.md
    VERIFIED 14-validation/speculative_inlining_lifted_constants.md

    The heuristic step: in a stub => keep; declaration must/cannot
    (S.Inline.DeclDecision) decides; already speculating => keep;
    argument types not useful => keep; otherwise ACTUALLY inline and
    simplify the body inside the hermetic sandbox
    (S.Struct.SpeculativeSandbox, ch. 09) purely to measure cost
    metrics, and inline iff size minus the weighted
    removed-operations bonuses comes in under the threshold.  The
    acceptance test is on the SIMPLIFIED inlined body, not a static
    size test.  Anchor: cost model and thresholds are explicitly
    retunable; all folded into inline_dec. *)
Definition S_Inline_Speculative_documented : Prop := True.

(** RULE S.Inline.DepthLimit (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
    CODE middle_end/flambda2/terms/inlining_state.ml#is_depth_exceeded
    CODE middle_end/flambda2/simplify/simplify_rec_info_expr.ml#depth_may_exceed
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#enter_inlined_apply

    Two distinct counters keep the inliner terminating, both side
    conditions inside S.Inline.Decision: the inlining-state depth
    (total nesting of inlinings, any function; incremented by 10 per
    heuristic inlining, 1 when was_inline_always, 0 in stubs; bounded
    by max_inlining_depth) and the rec-info depth (how deep one
    recursive function is followed; bounded by max_rec_depth on the
    heuristic path only).  Anchor: termination bookkeeping of the
    oracle. *)
Definition S_Inline_DepthLimit_documented : Prop := True.

(** RULE S.Inline.Unroll.Begin (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
    CODE middle_end/flambda2/term_basics/coercion.ml#change_depth

    Step 1 of explicit unrolling: attr = Unroll n (n > 0) with the
    call-site rec-info d in Not_unrolling state and can_unroll(d)
    yields inline{unroll_to = Some n}; the substitution then binds
    my_depth = RI_unroll_to n d0 and coerces the self-closure with
    Coercion_change_depth from d0 to my_depth, so the copied body's
    self-calls see the Unrolling state.  Anchor: the verdict payload
    (unroll_to) is carried by inline_verdict; rw_inline's Substitute
    rule covers the ordinary unroll_to = None verdict, and this
    unrolling variant of the substitution (the RI_unroll_to depth
    binding plus the self-closure coercion) stays descriptive with
    the rest of the unrolling machinery. *)
Definition S_Inline_Unroll_Begin_documented : Prop := True.

(** RULE S.Inline.Unroll.Continue (CLAIM descriptive)
    -- 11-inlining.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
    CODE middle_end/flambda2/simplify/simplify_rec_info_expr.ml#known_remaining_unrolling_depth

    Step 2: on the self-call inside a body inlined by step 1, d has
    unrolling state Unrolling{remaining_depth = m} (its depth is
    RI_succ my_depth, decrementing the count); m > 0 => inline with
    unroll_to = None, m = 0 => keep.  Exactly n copies result.
    Do_not_unroll (e.g. from an absent callee simple) blocks
    unrolling entirely.  An unknown/free unrolling state falls
    through to the ordinary heuristic.  Anchor. *)
Definition S_Inline_Unroll_Continue_documented : Prop := True.
