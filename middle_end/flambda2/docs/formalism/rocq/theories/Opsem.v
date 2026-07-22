(** * Opsem.v — the small-step abstract machine (ch. 04)

    Mechanizes 04-opsem.md: the transition judgment
    <e, rho, K, H, T, R> --> <e', rho', K', H', T', R'> (one constructor
    per OS.* rule), labels for C-call events, initial/final
    configurations, the steps closure, divergence, and the behavior
    type that ch. 13 (Soundness.v) observes.

    The step relation is parameterized by the primitive denotations
    (chs. 05/06 give them; Machine.v instantiates with their union)
    and by the axiomatized C-call relation of OS.Apply.CCall.

    Owner: Plotkin (wave 3).  Imports: Base, Syntax, Values. *)

(* String BEFORE List: String's length/concat/get must not shadow the
   list ones (build-captain guidance). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Syntax Values.
Import ListNotations.

Set Implicit Arguments.

(** ** Events and labels — 04-opsem.md §6.3, §8.2

    The observable behaviour of a program is (a) the sequence of
    external effects performed by C calls and (b) the termination
    outcome (OS.Unit.Final NOTES).  A step is either silent or emits
    one C-call event. *)

(* ENCODING NOTE: each event carries the heap AT THE CALL (H_call,
   the machine heap when the external is entered).  Values alone
   under-observe the boundary: the external world READS through
   pointer arguments, so the doc's "external effects performed by C
   calls" includes the argument-reachable memory at call time, not
   just the argument words.  Soundness.v's event comparison observes
   H_call restricted to what the external can reach (callee, args,
   retained initial-heap pointers); the machine itself never inspects
   the label, so this is observation-only payload.  (Fidelity review
   W-1, joint with obs_equiv's location bijection.) *)
Inductive event : Type :=
| Ev_ccall_return (callee : value) (args : list value)
    (H_call : heap) (results : list value)
| Ev_ccall_raise (callee : value) (args : list value)
    (H_call : heap) (v_exn : value).

Inductive label : Type :=
| L_tau
| L_event (ev : event).

(** ** The axiomatized C-call relation — OS.Apply.CCall

    "A C call is an opaque external... axiomatized as an arbitrary
    relation Cextern that, given the callee, argument values, and
    heap, may read and mutate any part of H and yields result
    value(s) and a new heap"; the raising outcome is "a second
    axiomatized outcome".  Machine.v supplies a Parameter of this
    type (sanctioned opaque item `cextern`).

    ENCODING NOTE: the doc writes Cextern(s_f, vbar, H) with s_f the
    syntactic callee simple; here the oracle is keyed by the callee's
    VALUE ([[s_f]]rho, always defined by OS.Apply.CCall's premises).
    The external world sees only the runtime callee, so keying by
    value makes the oracle independent of rho and of variable
    renaming; the C-call events in the trace carry the same value. *)

Inductive ccall_outcome : Type :=
| CC_return (results : list value) (H' : heap)
| CC_raise (v_exn : value) (H' : heap).

Definition cextern_rel : Type :=
  value -> list value -> heap -> ccall_outcome -> Prop.

(** ** Closures of a labeled transition relation

    Generic over the transition relation so that Machine.v can
    instantiate behaviors for the fully-instantiated machine
    ([fl_step]) without re-deriving them. *)

Section CLOSURES.

  Variable stp : config -> label -> config -> Prop.

  (** Finite multi-step execution, accumulating the event trace
      (silent steps contribute nothing). *)
  Inductive steps : config -> list event -> config -> Prop :=
  | Steps_refl : forall c, steps c [] c
  | Steps_tau : forall c1 c2 c3 tr,
      stp c1 L_tau c2 -> steps c2 tr c3 -> steps c1 tr c3
  | Steps_event : forall c1 c2 c3 ev tr,
      stp c1 (L_event ev) c2 -> steps c2 tr c3 ->
      steps c1 (ev :: tr) c3.

  (** Divergence: an infinite --> sequence (04-opsem.md §8.2).
      ENCODING NOTE: the doc's single "divergence" outcome splits,
      CompCert-style, into silent divergence after a finite trace
      ([diverges_silently], [Beh_diverge]) and divergence with an
      infinite event trace ([reacts], [Beh_react]); the union is the
      doc's "an infinite --> sequence".  The split avoids mixed
      induction/coinduction over possibly-finite traces. *)
  CoInductive diverges_silently : config -> Prop :=
  | Div_step : forall c1 c2,
      stp c1 L_tau c2 -> diverges_silently c2 -> diverges_silently c1.

  CoInductive event_stream : Type :=
  | Es_cons (ev : event) (s : event_stream).

  CoInductive reacts : config -> event_stream -> Prop :=
  | React_step : forall c1 c2 c3 tr ev s,
      steps c1 tr c2 -> stp c2 (L_event ev) c3 -> reacts c3 s ->
      reacts c1 (fold_right Es_cons (Es_cons ev s) tr).

End CLOSURES.

(** ** Behaviors — 04-opsem.md §8.2 (OS.Unit.Final), 13-soundness.md §1

    A run of a unit is one of: normal termination (observing
    H(sym_mod) and the C-call trace), termination by uncaught
    exception, divergence, or undefined behaviour (reaching
    OS.Invalid or a stuck state). *)

(* ENCODING NOTE: the terminating behaviors carry the FINAL HEAP,
   not an extracted object.  The doc's observation at normal
   termination is the module block REACHABLE from sym_mod (ch. 15
   CM.Unit.Final: "the bytes of the module block reachable from the
   module symbol"; 04's H(sym_mod) is its root), and locations are
   opaque (ch. 19), so ch. 13's obs_equiv compares the reachable
   substructure up to a location bijection — a comparison that needs
   the heap, not just the root object.  Likewise an uncaught
   exception value may point into the heap.  The reachability
   restriction and the renaming live in obs_equiv (Soundness.v);
   carrying the whole final heap here keeps this file literal. *)
Inductive behavior : Type :=
| Beh_return (tr : list event) (H_fin : heap)
    (* normal termination; observation = part of H_fin reachable
       from sym_mod, compared via obs_equiv *)
| Beh_exn (tr : list event) (v_exn : value) (H_fin : heap)
    (* termination by uncaught exception *)
| Beh_diverge (tr : list event)
    (* silent divergence after the finite trace tr *)
| Beh_react (s : event_stream)
    (* divergence with an infinite event trace *)
| Beh_undef (tr : list event).
    (* undefined behaviour after the finite trace tr *)

(** ** Auxiliary machinery for the step relation *)

(** The "arity-info" a Closures object records per slot, read off the
    code metadata (04-opsem.md §1.5: params_arity, is_tupled). *)
Definition code_arity_info (c : code0) : arity_info :=
  match c with
  | Mk_code0 _ _ _ _ _ _ _ pa _ _ tup => mk_arity_info pa tup
  end.

Definition soc_function_decls (soc : set_of_closures)
  : list (function_slot * code_id_in_fd) :=
  match soc with Mk_set_of_closures fds _ => fds end.

Definition soc_value_slots (soc : set_of_closures)
  : list (value_slot * simple) :=
  match soc with Mk_set_of_closures _ vs => vs end.

(** Switch arms: the doc's finite map, as assoc-list lookup. *)
Fixpoint arms_lookup (n : Z) (arms : list (Z * apply_cont_expr))
  : option apply_cont_expr :=
  match arms with
  | [] => None
  | (m, ac) :: rest => if Z.eqb n m then Some ac else arms_lookup n rest
  end.

(** The doc's "K(k) = Handler <xbar, e_h, rho_def, K_def, d>"
    (04-opsem.md §1.6), covering both entry encodings: a CE_handler
    directly, and a CE_rec group entry (the K' knot, catalog #1),
    which denotes Handler <zbar ++ xbar_i, e_i, rho_def, K', d> with
    K' re-tied at lookup time via bind_rec_group (OS.LetCont.Rec:
    invariant params zbar prefix every handler's parameter list). *)
Inductive handler_entry (K : kenv) (k : continuation)
  : list variable -> expr -> env -> kenv -> nat -> Prop :=
| HE_nonrec : forall xs e_h rho_def K_def d,
    K k = Some (CE_handler xs e_h rho_def K_def d) ->
    handler_entry K k xs e_h rho_def K_def d
| HE_rec : forall zs g rho_def K_def d h,
    K k = Some (CE_rec zs g rho_def K_def d) ->
    group_lookup k g = Some h ->
    handler_entry K k (zs ++ map fst (ch_params h)) (ch_handler h)
      rho_def (bind_rec_group K_def rho_def zs g d) d.

(** ENCODING NOTE (catalog #7): the apply-cont rules match the doc's
    "Apply_cont k (sbar)" uniformly against both the syntactic
    expression (argument Simples evaluated by [[.]]rho here, folding
    the rules' "vbar = [[sbar]]rho" premise into the match) and the
    machine-internal [Ctl_jump k vs] produced by the boundary rules
    (arguments already values; never a trap action). *)
Inductive at_apply_cont (rho : env)
  : control -> continuation -> list value -> option trap_action
    -> Prop :=
| AAC_expr : forall k ss t vs,
    simple_eval_list rho ss = Some vs ->
    at_apply_cont rho (Ctl_expr (E_apply_cont (Mk_apply_cont k ss t)))
      k vs t
| AAC_jump : forall k vs,
    at_apply_cont rho (Ctl_jump k vs) k vs None.

(** OS.Let.SetOfClosures' captured environment:
    env = (w |-> [[s_v]]rho)_v, computed eagerly at allocation. *)
Inductive soc_env (rho : env)
  : list (value_slot * simple) -> fmap value_slot value -> Prop :=
| SEnv_nil : soc_env rho [] fempty
| SEnv_cons : forall w s rest m v,
    simple_eval rho s = Some v ->
    soc_env rho rest m ->
    soc_env rho ((w, s) :: rest) (fupd value_slot_eqb m w v).

(** OS.Let.SetOfClosures' function-slot map:
    funs = (f_j |-> (cid_j, arity-info from H(cid_j)))_j, with
    Deleted slots omitted. *)
Inductive soc_funs (H : heap)
  : list (function_slot * code_id_in_fd)
    -> fmap function_slot (code_id * arity_info) -> Prop :=
| SF_nil : soc_funs H [] fempty
| SF_deleted : forall f sz rest m,
    soc_funs H rest m ->
    soc_funs H ((f, FD_deleted sz) :: rest) m
| SF_code : forall f cid ofa rest m c,
    H (HK_code cid) = Some (HO_Code c) ->
    soc_funs H rest m ->
    soc_funs H ((f, FD_code_id cid ofa) :: rest)
      (fupd function_slot_eqb m f (cid, code_arity_info c)).

(** ** Static constant installation — OS.Let.Static (04-opsem.md §3.5)

    "Allocate the object described by sc at sym", with each
    Or_variable hole "filled from rho at install time". *)

(** The bytes of a static string, as HO_Bytes contents. *)
Definition string_bytes (s : string) : list Z :=
  map (fun b => Z.of_N (Byte.to_N b)) (list_byte_of_string s).

(** Resolve an Or_variable hole: "each Or_variable field `Var x` is
    read from rho(x); each `Const c` is the constant c" — [inj]
    injects the constant into [value].  No kind premise on the
    variable case, matching the doc (well-formedness lives in
    ch. 03). *)
Inductive or_var_resolve (A : Type) (rho : env) (inj : A -> value)
  : or_variable A -> value -> Prop :=
| OVR_const : forall a, or_var_resolve rho inj (OV_const a) (inj a)
| OVR_var : forall x v,
    env_get_var rho x = Some v ->
    or_var_resolve rho inj (OV_var x) v.

(** Same, producing the raw float64 an HO_FloatBlock field needs. *)
Inductive or_var_float (rho : env)
  : or_variable float64 -> float64 -> Prop :=
| OVF_const : forall f, or_var_float rho (OV_const f) f
| OVF_var : forall x f,
    env_get_var rho x = Some (V_naked_float f) ->
    or_var_float rho (OV_var x) f.

(** The array_kind of the object a static empty array denotes.
    ENCODING NOTE: the runtime object is a zero-length array for
    every empty_array_kind; the kind tag is only consulted by later
    primitive rules.  EAK_values_or_immediates_or_naked_floats (one
    shared representation) is rendered as AK_values, and
    EAK_unboxed_products (element layout irrelevant at length 0) as
    AK_unboxed_product []. *)
Definition eak_array_kind (eak : empty_array_kind) : array_kind :=
  match eak with
  | EAK_values_or_immediates_or_naked_floats => AK_values
  | EAK_unboxed_products => AK_unboxed_product []
  | EAK_naked_float32s => AK_naked_float32s
  | EAK_naked_ints => AK_naked_ints
  | EAK_naked_int8s => AK_naked_int8s
  | EAK_naked_int16s => AK_naked_int16s
  | EAK_naked_int32s => AK_naked_int32s
  | EAK_naked_int64s => AK_naked_int64s
  | EAK_naked_nativeints => AK_naked_nativeints
  | EAK_naked_vec128s => AK_naked_vec128s
  | EAK_naked_vec256s => AK_naked_vec256s
  | EAK_naked_vec512s => AK_naked_vec512s
  end.

(** The heap object "described by sc" (OS.Let.Static, Block_like
    bullet): one constructor per Block_like-able static_const form.
    SC_set_of_closures is deliberately absent — sets of closures are
    installed by install_static's set-of-closures case (they bind
    closure symbols, not a Block_like symbol). *)
Inductive static_const_object (rho : env)
  : static_const -> heap_object -> Prop :=
| SCO_block_value_only : forall t mut fields vs,
    simple_eval_list rho fields = Some vs ->
    static_const_object rho (SC_block t mut Value_only fields)
      (HO_Block t mut vs)

(** RULE P.Static.MixedBlock (STATUS normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/static_const.ml#t
    CODE middle_end/flambda2/terms/static_const.ml#block_field_kind
    CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#static_const0
    VERIFIED 14-validation/mixed-02-static.md
    Hosted here (OS.Let.Static's install mechanism) rather than in
    PrimMemoryA.v so there is exactly one install semantics; see the
    catalog.  v_i is "the value of the Simple s_i (constant or
    rho(x))" = simple_eval; kind(s_i) is the kind of that value. *)
| SCO_block_mixed : forall t mut sigma fields vs,
    length fields
      = (value_prefix_size sigma + length (flat_suffix sigma))%nat ->
    simple_eval_list rho fields = Some vs ->
    Forall2 (fun v k => value_kind v = k)
      vs (mixed_block_field_kinds sigma) ->
    (mut = Immutable \/ mut = Immutable_unique) ->
    static_const_object rho
      (SC_block t mut (Mixed_record sigma) fields)
      (HO_MixedBlock t mut sigma vs)

| SCO_boxed_float32 : forall ov v,
    or_var_resolve rho V_naked_float32 ov v ->
    static_const_object rho (SC_boxed_float32 ov)
      (HO_Boxed K_naked_float32 v)
| SCO_boxed_float : forall ov v,
    or_var_resolve rho V_naked_float ov v ->
    static_const_object rho (SC_boxed_float ov)
      (HO_Boxed K_naked_float v)
| SCO_boxed_int32 : forall ov v,
    or_var_resolve rho V_naked_int32 ov v ->
    static_const_object rho (SC_boxed_int32 ov)
      (HO_Boxed K_naked_int32 v)
| SCO_boxed_int64 : forall ov v,
    or_var_resolve rho V_naked_int64 ov v ->
    static_const_object rho (SC_boxed_int64 ov)
      (HO_Boxed K_naked_int64 v)
| SCO_boxed_nativeint : forall ov v,
    or_var_resolve rho V_naked_nativeint ov v ->
    static_const_object rho (SC_boxed_nativeint ov)
      (HO_Boxed K_naked_nativeint v)
| SCO_boxed_vec128 : forall ov v,
    or_var_resolve rho V_naked_vec128 ov v ->
    static_const_object rho (SC_boxed_vec128 ov)
      (HO_Boxed K_naked_vec128 v)
| SCO_boxed_vec256 : forall ov v,
    or_var_resolve rho V_naked_vec256 ov v ->
    static_const_object rho (SC_boxed_vec256 ov)
      (HO_Boxed K_naked_vec256 v)
| SCO_boxed_vec512 : forall ov v,
    or_var_resolve rho V_naked_vec512 ov v ->
    static_const_object rho (SC_boxed_vec512 ov)
      (HO_Boxed K_naked_vec512 v)

| SCO_float_block : forall fields fs,
    Forall2 (or_var_float rho) fields fs ->
    static_const_object rho (SC_immutable_float_block fields)
      (HO_FloatBlock Immutable fs)

| SCO_float_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_float) fields vs ->
    static_const_object rho (SC_immutable_float_array fields)
      (HO_Array AK_naked_floats Immutable vs)
| SCO_float32_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_float32) fields vs ->
    static_const_object rho (SC_immutable_float32_array fields)
      (HO_Array AK_naked_float32s Immutable vs)
| SCO_int_array : forall fields vs,
    Forall2 (or_var_resolve rho V_tagged_imm) fields vs ->
    static_const_object rho (SC_immutable_int_array fields)
      (HO_Array AK_immediates Immutable vs)
| SCO_int8_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_int8) fields vs ->
    static_const_object rho (SC_immutable_int8_array fields)
      (HO_Array AK_naked_int8s Immutable vs)
| SCO_int16_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_int16) fields vs ->
    static_const_object rho (SC_immutable_int16_array fields)
      (HO_Array AK_naked_int16s Immutable vs)
| SCO_int32_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_int32) fields vs ->
    static_const_object rho (SC_immutable_int32_array fields)
      (HO_Array AK_naked_int32s Immutable vs)
| SCO_int64_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_int64) fields vs ->
    static_const_object rho (SC_immutable_int64_array fields)
      (HO_Array AK_naked_int64s Immutable vs)
| SCO_nativeint_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_nativeint) fields vs ->
    static_const_object rho (SC_immutable_nativeint_array fields)
      (HO_Array AK_naked_nativeints Immutable vs)
| SCO_vec128_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_vec128) fields vs ->
    static_const_object rho (SC_immutable_vec128_array fields)
      (HO_Array AK_naked_vec128s Immutable vs)
| SCO_vec256_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_vec256) fields vs ->
    static_const_object rho (SC_immutable_vec256_array fields)
      (HO_Array AK_naked_vec256s Immutable vs)
| SCO_vec512_array : forall fields vs,
    Forall2 (or_var_resolve rho V_naked_vec512) fields vs ->
    static_const_object rho (SC_immutable_vec512_array fields)
      (HO_Array AK_naked_vec512s Immutable vs)
| SCO_value_array : forall fields vs,
    simple_eval_list rho fields = Some vs ->
    static_const_object rho (SC_immutable_value_array fields)
      (HO_Array AK_values Immutable vs)
| SCO_empty_array : forall eak,
    static_const_object rho (SC_empty_array eak)
      (HO_Array (eak_array_kind eak) Immutable [])

| SCO_mutable_string : forall s,
    static_const_object rho (SC_mutable_string s)
      (HO_Bytes Mutable (string_bytes s))
| SCO_immutable_string : forall s,
    static_const_object rho (SC_immutable_string s)
      (HO_Bytes Immutable (string_bytes s)).

(** "each sym_j |-> clos l f_j" for a set of closures bound at
    symbols. *)
Definition bind_closure_syms (rho : env) (l : location)
  (csyms : list (function_slot * symbol)) : env :=
  fold_left
    (fun r fs => env_upd r (Name_sym (snd fs)) (V_clos l (fst fs)))
    csyms rho.

(** match_against_bound_static's four bullets, threading (rho, H)
    left-to-right over the paired (pattern, piece) list.  A
    mismatched pair has no clause (ill-formed: stuck).

    ENCODING NOTE (the static knot): [rho_fin]/[H_fin] are the FINAL
    environment and heap of the whole group — OS.Let.Static installs
    all pieces "once" with mutual reference allowed (NOTES: "every
    recursive cycle among the defined names passes through a code
    id"), so a set of closures must read arity-info (soc_funs) of
    code installed LATER in the same group, and value slots / fields
    referencing sibling closure symbols must see clos l f, which
    only rho_fin holds.  OS_Let_Static ties the knot by passing the
    relation's own outputs in.  For everything non-recursive, rho_fin
    and H_fin agree with the incoming rho/H at the points read. *)
Inductive install_static (rho_fin : env) (H_fin : heap)
  : list (bound_static_pattern * static_const_or_code)
    -> env -> heap -> env -> heap -> Prop :=
| IS_nil : forall rho H,
    install_static rho_fin H_fin [] rho H rho H
| IS_code : forall cid c rest rho H rho' H',
    fresh_for H (HK_code cid) ->
    install_static rho_fin H_fin rest
      rho (heap_upd H (HK_code cid) (HO_Code c)) rho' H' ->
    install_static rho_fin H_fin
      ((BSP_code cid, SCC_code c) :: rest) rho H rho' H'
| IS_deleted_code : forall cid rest rho H rho' H',
    install_static rho_fin H_fin rest rho H rho' H' ->
    install_static rho_fin H_fin
      ((BSP_code cid, SCC_deleted_code) :: rest) rho H rho' H'
| IS_set_of_closures : forall csyms soc rest rho H cenv funs l H1
      rho' H',
    soc_env rho_fin (soc_value_slots soc) cenv ->
    soc_funs H_fin (soc_function_decls soc) funs ->
    alloc (HO_Closures funs cenv) H l H1 ->
    install_static rho_fin H_fin rest
      (bind_closure_syms rho l csyms) H1 rho' H' ->
    install_static rho_fin H_fin
      ((BSP_set_of_closures csyms,
        SCC_static_const (SC_set_of_closures soc)) :: rest)
      rho H rho' H'
| IS_block_like : forall sym sc o rest rho H rho' H',
    static_const_object rho_fin sc o ->
    fresh_for H (HK_addr (Addr_sym sym)) ->
    install_static rho_fin H_fin rest
      (env_upd rho (Name_sym sym) (V_ptr (Addr_sym sym)))
      (heap_upd H (HK_addr (Addr_sym sym)) o) rho' H' ->
    install_static rho_fin H_fin
      ((BSP_block_like sym, SCC_static_const sc) :: rest)
      rho H rho' H'.

(** ** Function application machinery — 04-opsem.md §6

    OS.Apply.Direct's body-entry construction, factored out because
    the indirect rules reuse it by reference ("as OS.Apply.Direct
    with this cid"). *)

(** The doc's rho_code, which "supplies the symbols/code ids visible
    to the body" (OS.Apply.Direct NOTES).
    ENCODING NOTE: rho_code is the SYMBOL restriction of the caller's
    rho.  Symbols are globally scoped and only ever accumulate along
    execution (OS.Let.Static), so the caller's symbol bindings are
    exactly the static environment visible to any code body; the
    restriction drops the caller's locals (a code body's free names
    are its params, my_closure, my_depth, its region variables, and
    symbols). *)
Definition env_syms (rho : env) : env :=
  fun n =>
    match n with
    | Name_sym _ => rho n
    | Name_var _ => None
    end.

(** "region vars of mam |-> regions from am" (OS.Apply.Direct): if
    the call is local, the body's my_alloc_mode region variables are
    bound to the caller's regions (evaluated in the caller's rho); if
    the application says Heap, "mam is Heap and there is nothing to
    bind".  A Heap/Local mismatch has no rule (ill-formed: stuck). *)
Inductive bind_region_args (rho_caller : env)
  : alloc_mode_app -> alloc_mode_app -> env -> env -> Prop :=
| BRA_heap : forall rho,
    bind_region_args rho_caller App_heap App_heap rho rho
| BRA_local : forall r gr x_r x_gr v_r v_gr rho,
    env_get_var rho_caller r = Some v_r ->
    env_get_var rho_caller gr = Some v_gr ->
    bind_region_args rho_caller (App_local r gr) (App_local x_r x_gr)
      rho (env_upd_var (env_upd_var rho x_r v_r) x_gr v_gr).

(** K_body of OS.Apply.Direct: the callee's return and exn
    continuations map to boundary entries capturing the caller's
    context; "the body's free continuations are exactly k_ret and
    k_exn, so K_body need carry nothing else" (hence built on
    fempty).  The entry placeholders xbar_r / x_b are inert — the
    boundary rules pass values through without consulting them — so
    they are left unconstrained. *)
Inductive install_boundaries (rho : env) (K : kenv) (T : trap_stack)
  (R : region_stack) (k_ret k_exn : continuation)
  (k_x : continuation) (vs_extra : list value)
  : result_continuation -> kenv -> Prop :=
| IB_returns : forall k_c xs_r x_b,
    install_boundaries rho K T R k_ret k_exn k_x vs_extra
      (RC_return k_c)
      (kenv_upd
         (kenv_upd fempty k_ret
            (CE_return xs_r (RC_return k_c) rho K T R))
         k_exn (CE_exn x_b k_x vs_extra K T R))

(** RULE OS.Apply.NeverReturns (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/apply_expr.mli#Result_continuation
    CODE middle_end/flambda2/terms/apply_expr.mli#returns
    "No Return boundary is installed: K_body carries only
    k_exn |-> Exn <...>"; K_body(k_ret) = None, so "reaching the
    callee's k_ret" has no transition — stuck, classified as
    undefined behaviour (OS.Unit.Final), exactly the rule's
    "reaching the callee's k_ret is OS.Invalid". *)
| IB_never_returns : forall x_b,
    install_boundaries rho K T R k_ret k_exn k_x vs_extra
      RC_never_returns
      (kenv_upd fempty k_exn (CE_exn x_b k_x vs_extra K T R)).

(** Entering the code of [cid] (the body of OS.Apply.Direct, from
    the H(cid) lookup onward): check the argument count, build
    rho_body over rho_code, install the boundary entries, and start
    the body on the fresh trap stack [k_exn].  [l]/[f] identify the
    callee closure (bound to my_closure); [vs_extra] are the exn
    continuation's extra args, already "evaluated now, in the
    caller". *)
Inductive enter_code (rho : env) (K : kenv) (H : heap)
  (T : trap_stack) (R : region_stack)
  : code_id -> location -> function_slot -> list value
    -> result_continuation -> continuation -> list value
    -> alloc_mode_app -> config -> Prop :=
| Enter_code : forall cid l f vs dst k_x vs_extra am c
      rho_mid rho_body K_body,
    H (HK_code cid) = Some (HO_Code c) ->
    length (c0_params c) = length vs ->
    rho_mid = env_upd_var
                (env_upd_var
                   (env_upd_vars (env_syms rho)
                      (map fst (c0_params c)) vs)
                   (c0_my_closure c) (V_clos l f))
                (c0_my_depth c) V_rec_info ->
    bind_region_args rho am (c0_my_alloc_mode c) rho_mid rho_body ->
    install_boundaries rho K T R (c0_return_continuation c)
      (c0_exn_continuation c) k_x vs_extra dst K_body ->
    enter_code rho K H T R cid l f vs dst k_x vs_extra am
      (mk_config (Ctl_expr (c0_body c)) rho_body K_body H
         [c0_exn_continuation c] R).

(** ** The transition judgment — 04-opsem.md §§3-7

    One constructor per OS.* rule.  Parameterized over the primitive
    denotations (chs. 05/06; Machine.v instantiates their union), the
    effects classification, and the C-call oracle. *)

Section STEP.

Variable denot : denotation.
Variable denot_r : denotation_r.
Variable cext : cextern_rel.

(* ENCODING NOTE: the premises "p has No_effects or
   Only_generative_effects" / "p has Arbitrary_effects"
   (OS.Let.Prim.Pure/Effect) refer to the ch. 05/06 effects
   classification, which lives with the prim denotations
   (PrimMemoryA.v, wave 3).  It is a Section Variable here so Opsem
   does not import the prim files; Machine.v instantiates both. *)
Variable pure_prim : prim_op -> Prop.
    (* No_effects or Only_generative_effects *)
Variable effectful_prim : prim_op -> Prop.
    (* Arbitrary_effects *)

(** [[p]](vbar; H) with the region-stack update of ch. 06 ("Region
    delimiters") threaded through.  OS.Let.Prim.Pure/Effect NOTES:
    the region-opening/closing primitives "additionally" update R;
    every other primitive leaves R unchanged (the DR_plain
    constructors). *)
Inductive denot_R
  : prim_op -> list value -> heap -> region_stack -> prim_result_r
    -> Prop :=
| DR_plain_ok : forall op vs H R v H',
    denot op vs H (PR_ok v H') ->
    denot_R op vs H R (PRr_ok v H' R)
| DR_plain_undef : forall op vs H R,
    denot op vs H PR_undef ->
    denot_R op vs H R PRr_undef
| DR_region : forall op vs H R r,
    denot_r op vs H R r ->
    denot_R op vs H R r.

Inductive step : config -> label -> config -> Prop :=

(** RULE OS.Let.Simple (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Let_expr
    CODE middle_end/flambda2/terms/flambda.mli#Named *)
| OS_Let_Simple : forall x nm s e rho K H T R v,
    simple_eval rho s = Some v ->
    step (mk_config
            (Ctl_expr (E_let (BPat_singleton (Mk_bound_var x nm))
                         (N_simple s) e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) (env_upd_var rho x v) K H T R)

(** RULE OS.Let.Prim.Pure (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Named
    CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
    CODE middle_end/flambda2/terms/effects.mli#t
    Conclusion carries R' per the NOTES: Begin_region/Begin_try_region
    match here and additionally push a fresh region handle (denot_R's
    DR_region); all other matching primitives have R' = R. *)
| OS_Let_Prim_Pure : forall x nm p e rho K H T R vs v H' R',
    pure_prim (prim_op_of p) ->
    simple_eval_list rho (prim_args p) = Some vs ->
    denot_R (prim_op_of p) vs H R (PRr_ok v H' R') ->
    step (mk_config
            (Ctl_expr (E_let (BPat_singleton (Mk_bound_var x nm))
                         (N_prim p) e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) (env_upd_var rho x v) K H' T R')

(** RULE OS.Let.Prim.Effect (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Named
    CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
    Same transition as OS.Let.Prim.Pure (the doc separates them for
    Simplify's sake).  End_region/End_try_region match here and pop R
    (DR_region).  If [[p]] = undef the machine is stuck (see
    undef_next below). *)
| OS_Let_Prim_Effect : forall x nm p e rho K H T R vs v H' R',
    effectful_prim (prim_op_of p) ->
    simple_eval_list rho (prim_args p) = Some vs ->
    denot_R (prim_op_of p) vs H R (PRr_ok v H' R') ->
    step (mk_config
            (Ctl_expr (E_let (BPat_singleton (Mk_bound_var x nm))
                         (N_prim p) e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) (env_upd_var rho x v) K H' T R')

(** RULE OS.Let.SetOfClosures (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Named
    CODE middle_end/flambda2/terms/set_of_closures.mli#create
    CODE middle_end/flambda2/bound_identifiers/alloc_mode.mli#For_allocations
    ENCODING NOTE (catalog #10): the rule's alloc-mode dichotomy
    (am = Heap vs Local{region = x_r}) is elided — both bullets
    extend H identically and region placement is not recorded in the
    heap; only the reclamation bookkeeping (conceptual) differs. *)
| OS_Let_SetOfClosures : forall bvs soc am e rho K H T R
      cenv funs l H' rho',
    soc_env rho (soc_value_slots soc) cenv ->
    soc_funs H (soc_function_decls soc) funs ->
    alloc (HO_Closures funs cenv) H l H' ->
    length bvs = length (soc_function_decls soc) ->
    rho' = env_upd_vars rho (map bv_var bvs)
             (map (V_clos l) (map fst (soc_function_decls soc))) ->
    step (mk_config
            (Ctl_expr (E_let (BPat_set_of_closures bvs)
                         (N_set_of_closures soc am) e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) rho' K H' T R)

(** RULE OS.Let.Static (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Named
    CODE middle_end/flambda2/terms/static_const.mli#t
    CODE middle_end/flambda2/terms/flambda.mli#Static_const_or_code
    CODE middle_end/flambda2/bound_identifiers/bound_static.mli#Pattern
    match_against_bound_static "pairs each piece with its bound
    symbol / code id" — the length premise plus positional combine;
    install_static walks the pairs (its clauses are the rule's four
    bullets, with Or_variable resolution and freshness at each
    site), tying the mutual-reference knot by receiving the final
    rho'/H' (ENCODING NOTE at install_static).  The conclusion's
    rho[sym_j |-> v_j ...] (NOTES: v_j per the match -- clos l f_j
    for set-of-closures symbols, ptr sym_j for Block_like) is
    accumulated by the walk. *)
| OS_Let_Static : forall bs g e rho K H T R rho' H',
    length bs = length g ->
    install_static rho' H' (combine bs g) rho H rho' H' ->
    step (mk_config
            (Ctl_expr (E_let (BPat_static bs) (N_static_consts g)
                         e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) rho' K H' T R)

(** RULE OS.Let.RecInfo (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Named
    CODE middle_end/flambda2/term_basics/rec_info_expr.mli#t *)
| OS_Let_RecInfo : forall x nm ri e rho K H T R,
    step (mk_config
            (Ctl_expr (E_let (BPat_singleton (Mk_bound_var x nm))
                         (N_rec_info ri) e)) rho K H T R)
      L_tau
      (mk_config (Ctl_expr e) (env_upd_var rho x V_rec_info) K H T R)

(** RULE OS.LetCont.NonRec (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
    CODE middle_end/flambda2/terms/flambda.mli#Non_recursive_let_cont_handler
    CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
    K_def = K: the handler cannot see k. *)
| OS_LetCont_NonRec : forall k h e_body rho K H T R entry,
    entry = CE_handler (map fst (ch_params h)) (ch_handler h)
              rho K (length T) ->
    step (mk_config (Ctl_expr (E_let_cont_nonrec k h e_body))
            rho K H T R)
      L_tau
      (mk_config (Ctl_expr e_body) rho
         (fupd continuation_eqb K k entry) H T R)

(** RULE OS.LetCont.Rec (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
    CODE middle_end/flambda2/terms/flambda.mli#Recursive_let_cont_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation
    CODE middle_end/flambda2/simplify/apply_cont_rewrite.ml#get_used_params
    ENCODING NOTE (the K' knot, catalog #1): the doc's
    K' = K[k_i |-> Handler <zbar ++ xbar_i, e_i, rho, K', |T|>] is
    realized by bind_rec_group (Values.v), which stores the syntactic
    group and re-ties the fixed point at lookup (handler_entry's
    HE_rec). *)
| OS_LetCont_Rec : forall inv_params handlers e_body rho K H T R K',
    K' = bind_rec_group K rho (map fst inv_params) handlers
           (length T) ->
    step (mk_config
            (Ctl_expr (E_let_cont_rec inv_params handlers e_body))
            rho K H T R)
      L_tau
      (mk_config (Ctl_expr e_body) rho K' H T R)

(** RULE OS.ApplyCont (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/apply_cont_expr.mli#create
    CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
    The handler runs in its definition-time environments extended by
    the arguments; T and R are unchanged (no trap action). *)
| OS_ApplyCont : forall ctl rho K H T R k vs xs e_h rho_def K_def d,
    at_apply_cont rho ctl k vs None ->
    handler_entry K k xs e_h rho_def K_def d ->
    length xs = length vs ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_expr e_h) (env_upd_vars rho_def xs vs)
         K_def H T R)

(** RULE OS.ApplyCont.Return (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/apply_expr.mli#Result_continuation
    CODE middle_end/flambda2/terms/flambda.mli#Function_params_and_body
    Function return: control transfers to the caller's destination
    with the caller's trap and region stacks restored; the conclusion
    is the synthetic Apply_cont k_c (values vbar), i.e. Ctl_jump.
    The trap action t is unconstrained (the doc states no premise on
    it; the callee's T is discarded wholesale).  The dst =
    Never_returns case is undefined behaviour (OS.Apply.NeverReturns,
    via undef_next). *)
| OS_ApplyCont_Return : forall ctl rho K H T R k_ret vs t
      xs dst rho_c K_c T_c R_c k_c,
    at_apply_cont rho ctl k_ret vs t ->
    K k_ret = Some (CE_return xs dst rho_c K_c T_c R_c) ->
    dst = RC_return k_c ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_jump k_c vs) rho_c K_c H T_c R_c)

(** RULE OS.ApplyCont.ExnBoundary (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/exn_continuation.mli#t
    CODE middle_end/flambda2/terms/exn_continuation.mli#extra_args
    An exception escaping the callee: forward the bucket to the
    caller's handler k_x with the caller's extra args appended and
    the caller's context restored; k_x (top of the stored T_c) is
    popped before the jump, preserving the §1.7 depth invariant.
    The first argument is the bucket ("raised bucket, plus possibly
    extra args already in sbar"); incoming syntactic extras are
    superseded by the entry's stored extras (identical in WF
    programs per exn_continuation extra_args).  t unconstrained, as
    in OS.ApplyCont.Return.
    ENCODING NOTE: the doc's conclusion leaves the environment
    component as "_" (a Ctl_jump state never reads rho); we pick
    fempty. *)
| OS_ApplyCont_ExnBoundary : forall ctl rho K H T R k_exn
      v_b vs_rest t x_b k_x v_extra K_c T_c R_c T',
    at_apply_cont rho ctl k_exn (v_b :: vs_rest) t ->
    K k_exn = Some (CE_exn x_b k_x v_extra K_c T_c R_c) ->
    T_c = k_x :: T' ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_jump k_x (v_b :: v_extra)) fempty K_c H T' R_c)

(** RULE OS.ApplyCont.TrapPush (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/trap_action.mli#t
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps
    Entering a try body: push the handler k_h, then jump to k. *)
| OS_ApplyCont_TrapPush : forall ctl rho K H T R k vs k_h
      xs e_h rho_def K_def d,
    at_apply_cont rho ctl k vs (Some (Trap_push k_h)) ->
    handler_entry K k xs e_h rho_def K_def d ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_expr e_h) (env_upd_vars rho_def xs vs)
         K_def H (k_h :: T) R)

(** RULE OS.ApplyCont.TrapPop (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/trap_action.mli#t
    CODE middle_end/flambda2/terms/apply_cont_expr.ml#is_raise
    Non-exceptional exit of a try: pop k_h, continue to the join
    continuation k (<> k_h: not a raise). *)
| OS_ApplyCont_TrapPop : forall ctl rho K H T R k vs k_h rk
      xs e_h rho_def K_def d T',
    at_apply_cont rho ctl k vs (Some (Trap_pop k_h rk)) ->
    k <> k_h ->
    handler_entry K k xs e_h rho_def K_def d ->
    T = k_h :: T' ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_expr e_h) (env_upd_vars rho_def xs vs)
         K_def H T' R)

(** RULE OS.ApplyCont.Raise (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/apply_cont_expr.ml#is_raise
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_raise0
    CODE middle_end/flambda2/terms/trap_action.mli#Raise_kind
    Raise: target equals the popped handler (is_raise); pop the top
    frame and jump to it with bucket :: extra args.  raise_kind
    affects only the runtime backtrace and is ignored.  When k_h is a
    callee's exn continuation (an Exn boundary entry),
    OS.ApplyCont.ExnBoundary fires instead (handler_entry does not
    match boundary entries). *)
| OS_ApplyCont_Raise : forall ctl rho K H T R k_h vs rk T'
      xs e_h rho_def K_def d,
    at_apply_cont rho ctl k_h vs (Some (Trap_pop k_h rk)) ->
    T = k_h :: T' ->
    handler_entry K k_h xs e_h rho_def K_def d ->
    step (mk_config ctl rho K H T R) L_tau
      (mk_config (Ctl_expr e_h) (env_upd_vars rho_def xs vs)
         K_def H T' R)

(** RULE OS.Switch (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/switch_expr.mli#create
    CODE middle_end/flambda2/terms/switch_expr.mli#arms
    The selected arm is itself an Apply_cont (possibly with a trap
    action), which then steps by the §4 rules.  The out-of-arms case
    is OS.Switch.Undef (stuck; see undef_next). *)
| OS_Switch : forall s arms rho K H T R n ac,
    simple_eval rho s = Some (V_naked_imm n) ->
    arms_lookup n arms = Some ac ->
    step (mk_config (Ctl_expr (E_switch (Mk_switch s arms)))
            rho K H T R)
      L_tau
      (mk_config (Ctl_expr (E_apply_cont ac)) rho K H T R)

(** RULE OS.Apply.Direct (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/apply_expr.mli#create
    CODE middle_end/flambda2/terms/call_kind.mli#Function_call
    CODE middle_end/flambda2/terms/flambda.mli#Function_params_and_body
    CODE middle_end/flambda2/terms/code_metadata.mli#params_arity
    Callee and argument evaluation here; the H(cid) code lookup,
    |xbar| = |vbar| check, rho_body/K_body/T_body construction are
    [enter_code] above (factored because the indirect rules reuse
    them as "as OS.Apply.Direct with this cid").  An arity mismatch
    has no rule ("else OS.Invalid": stuck). *)
| OS_Apply_Direct : forall s_f ss dst exnc aa ra cid am rho K H T R
      l f vs vs_extra c',
    simple_eval rho s_f = Some (V_clos l f) ->
    simple_eval_list rho ss = Some vs ->
    simple_eval_list rho (map fst (ec_extra_args exnc))
      = Some vs_extra ->
    enter_code rho K H T R cid l f vs dst (ec_exn_handler exnc)
      vs_extra am c' ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss dst exnc
                                  aa ra (CK_function (FC_direct cid))
                                  am))) rho K H T R)
      L_tau c'

(** RULE OS.Apply.IndirectUnknownArity.Full (STATUS normative)
    -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Function_call
    CODE middle_end/flambda2/terms/code_metadata.mli#params_arity
    The code id is read from the closure's function slot; when the
    argument count matches the callee's arity ([ai_arity]: tupled
    callees compare against 1, the tuple, per the NOTES), the call
    proceeds exactly as OS.Apply.Direct with this cid. *)
| OS_Apply_IndirectUnknownArity_Full : forall s_f ss dst exnc aa ra
      am rho K H T R l f funs cenv cid ai vs vs_extra c',
    simple_eval rho s_f = Some (V_clos l f) ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs cenv) ->
    funs f = Some (cid, ai) ->
    length ss = ai_arity ai ->
    simple_eval_list rho ss = Some vs ->
    simple_eval_list rho (map fst (ec_extra_args exnc))
      = Some vs_extra ->
    enter_code rho K H T R cid l f vs dst (ec_exn_handler exnc)
      vs_extra am c' ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss dst exnc
                                  aa ra (CK_function
                                           FC_indirect_unknown_arity)
                                  am))) rho K H T R)
      L_tau c'

(** RULE OS.Apply.IndirectUnknownArity.Partial (STATUS conjectured)
    -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Function_call
    CODE middle_end/flambda2/terms/code_metadata.mli#first_complex_local_param
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application
    Too few arguments: allocate a partial-application closure and
    return it to k_c (the conclusion is the synthetic
    Apply_cont k_c (clos l_pa f_pa), i.e. Ctl_jump).
    ENCODING NOTE: the closure FORM is forced by the rule's
    conclusion (it returns clos l_pa f_pa, so the fresh location
    must hold SOME Closures object); the closure's contents
    [funs_pa]/[cenv_pa], its slot [f_pa], and its application
    behavior are deliberately unconstrained: the doc marks the
    runtime caml_curry wrapper's "precise wrapper shape and
    allocation region" unverified, and describes the object only
    as "a 'partial application' closure recording
    (clos l f, [[sbar]]rho)".  The region-placement dichotomy is
    elided as in catalog #10. *)
| OS_Apply_IndirectUnknownArity_Partial : forall s_f ss k_c exnc
      aa ra am rho K H T R l f funs cenv cid ai vs
      funs_pa cenv_pa f_pa l_pa H',
    simple_eval rho s_f = Some (V_clos l f) ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs cenv) ->
    funs f = Some (cid, ai) ->
    (0 < length ss)%nat ->
    (length ss < ai_arity ai)%nat ->
    simple_eval_list rho ss = Some vs ->
    alloc (HO_Closures funs_pa cenv_pa) H l_pa H' ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss
                                  (RC_return k_c) exnc aa ra
                                  (CK_function
                                     FC_indirect_unknown_arity)
                                  am))) rho K H T R)
      L_tau
      (mk_config (Ctl_jump k_c [V_clos l_pa f_pa]) rho K H' T R)

(** RULE OS.Apply.IndirectUnknownArity.Over (STATUS conjectured)
    -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Function_call
    CODE middle_end/flambda2/terms/apply_expr.mli#return_arity
    CODE middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application
    Too many arguments: call with the first m, binding the result g
    in a fresh continuation k_mid that applies the rest; both
    sub-applications share the caller's exn continuation.
    ENCODING NOTE: the doc leaves the freshness of the handler
    parameter g implicit; we premise g fresh for rho (so the stored
    sbar_2 cannot be captured when the handler binds g).  The
    synthesized Applys reuse the original args_arity/return_arity
    metadata (the machine never consults those fields).  The
    local-callee Begin/End_region wrapping is elided, as in the
    rule's own NOTES. *)
| OS_Apply_IndirectUnknownArity_Over : forall s_f ss ss1 ss2 k_c
      exnc aa ra am rho K H T R l f funs cenv cid ai k_mid g
      e_rest K',
    simple_eval rho s_f = Some (V_clos l f) ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs cenv) ->
    funs f = Some (cid, ai) ->
    (ai_arity ai < length ss)%nat ->
    ss = ss1 ++ ss2 ->
    length ss1 = ai_arity ai ->
    fresh_for K k_mid ->
    fresh_for rho (Name_var g) ->
    e_rest = E_apply (Mk_apply (Some (simple_of_name (Name_var g)))
                        ss2 (RC_return k_c) exnc aa ra
                        (CK_function FC_indirect_unknown_arity) am) ->
    K' = kenv_upd K k_mid (CE_handler [g] e_rest rho K (length T)) ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss
                                  (RC_return k_c) exnc aa ra
                                  (CK_function
                                     FC_indirect_unknown_arity)
                                  am))) rho K H T R)
      L_tau
      (mk_config
         (Ctl_expr (E_apply (Mk_apply (Some s_f) ss1
                               (RC_return k_mid) exnc aa ra
                               (CK_function
                                  FC_indirect_unknown_arity) am)))
         rho K' H T R)

(** RULE OS.Apply.IndirectKnownArity (STATUS normative)
    -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Function_call
    "The argument arity is statically known to match the callee's
    arity, so only the full-application case applies" — semantics as
    a full indirect call (no runtime arity comparison; a mismatch
    fails enter_code's |params| = |vbar| check: stuck).  The
    optional code-id set only constrains which cid may be found. *)
| OS_Apply_IndirectKnownArity : forall s_f ss dst exnc aa ra
      code_ids am rho K H T R l f funs cenv cid ai vs vs_extra c',
    simple_eval rho s_f = Some (V_clos l f) ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs cenv) ->
    funs f = Some (cid, ai) ->
    (forall S, code_ids = Ou_known S -> In cid S) ->
    simple_eval_list rho ss = Some vs ->
    simple_eval_list rho (map fst (ec_extra_args exnc))
      = Some vs_extra ->
    enter_code rho K H T R cid l f vs dst (ec_exn_handler exnc)
      vs_extra am c' ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss dst exnc
                                  aa ra (CK_function
                                           (FC_indirect_known_arity
                                              code_ids))
                                  am))) rho K H T R)
      L_tau c'

(** RULE OS.Apply.CCall (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#t
    CODE middle_end/flambda2/terms/apply_expr.mli#create
    Normal return of the external: (rbar, H') in
    Cextern(s_f, vbar, H), stepping to the synthetic
    Apply_cont k_c (rbar).  The step emits the C-call event — this
    is where observable I/O enters the trace (§7 / OS.Unit.Final).
    The oracle is keyed by the callee VALUE (ENCODING NOTE at
    cextern_rel above). *)
| OS_Apply_CCall : forall s_f ss k_c exnc aa ra ncc icb eff coeff
      am rho K H T R v_f vs rs H',
    simple_eval rho s_f = Some v_f ->
    simple_eval_list rho ss = Some vs ->
    cext v_f vs H (CC_return rs H') ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss
                                  (RC_return k_c) exnc aa ra
                                  (CK_c_call ncc icb eff coeff)
                                  am))) rho K H T R)
      (L_event (Ev_ccall_return v_f vs H rs))
      (mk_config (Ctl_jump k_c rs) rho K H' T R)

(** The raising outcome of OS.Apply.CCall ("a second axiomatized
    outcome", NOTES): the call instead steps to
    Apply_cont k_x (v_exn :: vbar_extra).  The result continuation
    is left unconstrained here: a Never_returns C call may still
    raise (OS.Apply.NeverReturns NOTES: "the exn continuation is
    still installed"), while its normal return has no rule. *)
| OS_Apply_CCall_Raise : forall s_f ss dst exnc aa ra ncc icb eff
      coeff am rho K H T R v_f vs vs_extra v_exn H',
    simple_eval rho s_f = Some v_f ->
    simple_eval_list rho ss = Some vs ->
    simple_eval_list rho (map fst (ec_extra_args exnc))
      = Some vs_extra ->
    cext v_f vs H (CC_raise v_exn H') ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply (Some s_f) ss dst exnc
                                  aa ra (CK_c_call ncc icb eff coeff)
                                  am))) rho K H T R)
      (L_event (Ev_ccall_raise v_f vs H v_exn))
      (mk_config (Ctl_jump (ec_exn_handler exnc)
                    (v_exn :: vs_extra)) rho K H' T R)

(** RULE OS.Apply.Method (STATUS conjectured) -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Method_kind
    Method dispatch resolves to a closure, applied like an indirect
    call.
    ENCODING NOTE: the resolved closure (l, f) is UNCONSTRAINED by
    the object value — the doc marks the object-system lowering
    unmodelled ("the resolution is not grounded against the code"),
    so any closure in H may be selected; only the full-application
    shape is given (via enter_code), matching the rule's "applied to
    sbar (as an indirect call)". *)
| OS_Apply_Method : forall s_f mk s_obj ss k_c exnc aa ra am
      rho K H T R v_obj l f funs cenv cid ai vs vs_extra c',
    simple_eval rho s_obj = Some v_obj ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs cenv) ->
    funs f = Some (cid, ai) ->
    simple_eval_list rho ss = Some vs ->
    simple_eval_list rho (map fst (ec_extra_args exnc))
      = Some vs_extra ->
    enter_code rho K H T R cid l f vs (RC_return k_c)
      (ec_exn_handler exnc) vs_extra am c' ->
    step (mk_config
            (Ctl_expr (E_apply (Mk_apply s_f ss (RC_return k_c)
                                  exnc aa ra (CK_method mk s_obj)
                                  am))) rho K H T R)
      L_tau c'.

(** RULE OS.Apply.Effect (STATUS conjectured) -- 04-opsem.md
    CODE middle_end/flambda2/terms/call_kind.mli#Effect
    Out of scope: the doc gives NO transition ("modelling them
    requires a fibre component absent from <e, rho, K, H, T, R>";
    deferred to the scope ledger, §01).  In-scope residue: a
    configuration whose apply has call_kind = CK_effect has no step
    constructor above, so it is stuck and OS.Unit.Final classifies
    it as undefined behaviour — a MODEL artifact (real effect ops
    are defined), acceptable only because Simplify's rewrites never
    introduce or depend on effect operations.  A future
    mechanization would add a fibre stack to [config] and
    transitions for Perform/Reperform/Resume/With_stack{,_
    preemptible}. *)
Definition OS_Apply_Effect_documented : Prop := True.

(** ** Classifying non-stepping configurations — §7, §8.2

    OS.Unit.Final's undefined-behaviour outcome is "reaching
    OS.Invalid or a stuck state".  [undef_next] collects the
    configurations the doc explicitly calls undefined; [stuck]
    is the complement classification (not final, no step).
    They are kept SEPARATE — not undef_next c -> stuck c — because
    denotations may be nondeterministic: a primitive can relate the
    same inputs to both PR_ok and PR_undef (e.g. ReadOffset), so an
    undef-reaching configuration may also step. *)

Inductive undef_next : config -> Prop :=

(** RULE OS.Invalid (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda.mli#expr_descr
    CODE middle_end/flambda2/terms/flambda.mli#Invalid
    "has no transition (undefined behaviour)": no step constructor
    matches Invalid, and this clause classifies it as
    undef-reaching.  Soundness (ch. 13) must show reachable
    configurations never step to Invalid. *)
| UN_invalid : forall msg rho K H T R,
    undef_next (mk_config (Ctl_expr (E_invalid msg)) rho K H T R)

(** RULE OS.Switch.Undef (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/switch_expr.mli#create
    CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
    "There is no default case": a discriminant outside the arms is
    stuck (no OS.Switch step) and explicitly undefined
    behaviour. *)
| UN_switch : forall s arms rho K H T R n,
    simple_eval rho s = Some (V_naked_imm n) ->
    arms_lookup n arms = None ->
    undef_next (mk_config (Ctl_expr (E_switch (Mk_switch s arms)))
                  rho K H T R)

(** [[p]](vbar; H) = undef at a Let-bound primitive (the undef case
    of OS.Let.Prim.Pure/Effect: "evaluation is undefined behaviour"
    per the ch. 05/06 denotations). *)
| UN_prim : forall x nm p e rho K H T R vs,
    simple_eval_list rho (prim_args p) = Some vs ->
    denot_R (prim_op_of p) vs H R PRr_undef ->
    undef_next (mk_config
                  (Ctl_expr (E_let (BPat_singleton
                                      (Mk_bound_var x nm))
                               (N_prim p) e)) rho K H T R).

(** The two halting shapes of OS.Unit.Final (both control forms via
    at_apply_cont).  The trap action is quantified, not required
    None: the doc's halting shapes are silent about trap actions,
    and a toplevel raise is literally Apply_cont k_exn0 (bucket)
    with a Pop of k_exn0 (close_raise0; OS.ApplyCont.ExnBoundary's
    NOTES: "At toplevel this pops k_exn0 and reaches Halt_exn at
    depth 0 by both paths").  Requiring None misclassified that
    defined outcome as stuck, hence Beh_undef (KF-019); same latent
    shape on the return side. *)
Inductive final_config : config -> Prop :=
| Final_return : forall ctl rho K H T R k vs t,
    at_apply_cont rho ctl k vs t ->
    K k = Some CE_halt_return ->
    final_config (mk_config ctl rho K H T R)
| Final_exn : forall ctl rho K H T R k v_exn vs_rest t,
    at_apply_cont rho ctl k (v_exn :: vs_rest) t ->
    K k = Some CE_halt_exn ->
    final_config (mk_config ctl rho K H T R).

(** A stuck state: not final and no transition applies
    (OS.Unit.Final: "reaching OS.Invalid or a stuck state" is
    undefined behaviour). *)
Definition stuck (c : config) : Prop :=
  ~ final_config c /\ (forall lbl c', ~ step c lbl c').

(** RULE OS.Unit.Final (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda_unit.mli#return_continuation
    CODE middle_end/flambda2/terms/flambda_unit.mli#module_symbol
    "A run of u is one of: normal termination (observing H(sym_mod)
    and the trace of C-call effects), termination by uncaught
    exception, divergence (an infinite --> sequence), or undefined
    behaviour (reaching OS.Invalid or a stuck state)."
    Divergence is split Beh_diverge/Beh_react (catalog #11); the
    undefined outcome covers undef_next OR stuck, matching the
    quoted sentence literally.  The terminating behaviors carry the
    final heap (see the ENCODING NOTE at [behavior]); the H(sym_mod)
    observation is taken by obs_equiv, so sym_mod is not a parameter
    here. *)
Inductive has_behavior (c0 : config) : behavior -> Prop :=
| HB_return : forall tr ctl rho K H T R k vs t,
    steps step c0 tr (mk_config ctl rho K H T R) ->
    at_apply_cont rho ctl k vs t ->
    K k = Some CE_halt_return ->
    has_behavior c0 (Beh_return tr H)
| HB_exn : forall tr ctl rho K H T R k v_exn vs_rest t,
    steps step c0 tr (mk_config ctl rho K H T R) ->
    at_apply_cont rho ctl k (v_exn :: vs_rest) t ->
    K k = Some CE_halt_exn ->
    has_behavior c0 (Beh_exn tr v_exn H)
| HB_diverge : forall tr c,
    steps step c0 tr c ->
    diverges_silently step c ->
    has_behavior c0 (Beh_diverge tr)
| HB_react : forall s,
    reacts step c0 s ->
    has_behavior c0 (Beh_react s)
| HB_undef : forall tr c,
    steps step c0 tr c ->
    (undef_next c \/ stuck c) ->
    has_behavior c0 (Beh_undef tr).

End STEP.

(** RULE OS.Unit.Init (STATUS normative) -- 04-opsem.md
    CODE middle_end/flambda2/terms/flambda_unit.mli#create
    ENCODING NOTE: the doc's ambient "predefined symbols |-> their
    pointers" / "predefined symbols and external code ids |-> their
    objects" are the PARAMETERS rho_pre and H0 — the doc does not
    enumerate them, and OS.Unit.Final NOTES quantifies observations
    "from every starting heap", so ch. 13 states its theorems for
    all such rho_pre/H0.  "iota_0 fresh" is vacuous at start-up (the
    region stack begins empty), so iota_0 is simply quantified. *)
Inductive initial (u : flambda_unit) (rho_pre : env) (H0 : heap)
  : config -> Prop :=
| Init : forall i0,
    (* rho_0 binds only the two region variables and predefined
       SYMBOLS: rho_pre has no variable bindings. *)
    (forall x, env_get_var rho_pre x = None) ->
    initial u rho_pre H0
      (mk_config (Ctl_expr (fu_body u))
         (env_upd_var
            (env_upd_var rho_pre (fu_toplevel_my_region u)
               (V_region i0))
            (fu_toplevel_my_ghost_region u) (V_region i0))
         (kenv_upd
            (kenv_upd fempty (fu_return_continuation u)
               CE_halt_return)
            (fu_exn_continuation u) CE_halt_exn)
         H0 [fu_exn_continuation u] [i0]).
