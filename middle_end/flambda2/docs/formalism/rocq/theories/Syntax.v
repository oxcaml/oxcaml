(* Syntax.v -- the Flambda 2 term language, from 02-syntax.md (grammar)
   with the primitive inventory of 05-primitives-scalar.md /
   06-primitives-memory.md pinned against terms/flambda_primitive.mli.

   Contents: rec_info expressions, coercions, constants and simples;
   the primitive operator taxonomy (six arities) and its descriptor
   enums; call kinds; the expression grammar (mutual block);
   compilation units; replace_apply_cont; the 13 WF.Syntax.* rules;
   the sanctioned binder Parameters.

   Conventions: rocq/CORRESPONDENCE.md.  Wave 1; owner: Church. *)

From Stdlib Require Import ZArith Bool String List Relations.
From Flambda2 Require Import Base.
Import ListNotations.

(* ENCODING NOTE: debug-info payloads (dbg, Debuginfo.t,
   Inlined_debuginfo.t, condition_dbg, relative_history) are elided
   throughout this file: they never affect the semantics and carrying
   them would only thread noise through every rule. *)

(* ================================================================== *)
(* 1. rec_info expressions (02-syntax.md, rec_info;                   *)
(*    identifiers/rec_info_expr0.mli)                                 *)
(* ================================================================== *)

(* d ::= (int) | infinity  -- int Or_infinity.t *)
Inductive or_infinity :=
  Fin (n : Z)
| Inf.

(* u ::= Not_unrolling | Unrolling { remaining_depth } | Do_not_unroll *)
Inductive unrolling_state :=
  Not_unrolling
| Unrolling (remaining_depth : Z)
| Do_not_unroll.

Inductive rec_info_expr :=
  RI_const (depth : or_infinity) (unrolling : unrolling_state)
| RI_var (x : variable)
| RI_succ (ri : rec_info_expr)
| RI_unroll_to (depth : Z) (ri : rec_info_expr).

(* ================================================================== *)
(* 2. Coercions, constants, simples (02-syntax.md;                    *)
(*    identifiers/coercion0.mli, int_ids.mli#Const.Descr)             *)
(* ================================================================== *)

Inductive coercion :=
  Coercion_id
| Coercion_change_depth (from : rec_info_expr) (to_ : rec_info_expr).

Inductive const :=
  Const_naked_immediate (i : Z)
| Const_tagged_immediate (i : Z)
| Const_naked_float (f : float64)
| Const_naked_float32 (f : float32)
| Const_naked_int8 (n : Z)
| Const_naked_int16 (n : Z)
| Const_naked_int32 (n : Z)
| Const_naked_int64 (n : Z)
| Const_naked_nativeint (n : Z)
| Const_naked_vec128 (v : vec128)
| Const_naked_vec256 (v : vec256)
| Const_naked_vec512 (v : vec512)
| Const_null
| Const_poison (k : kind) (msg : string).

(* s ::= x | sym | c: a name always carries a coercion (Id when
   absent); a constant never does (int_ids.mli#Simple pattern_match). *)
Inductive simple :=
  Simple_name (n : name) (co : coercion)
| Simple_const (c : const).

Definition simple_of_name (n : name) : simple :=
  Simple_name n Coercion_id.

(* The underlying name, looking through the coercion; None iff the
   simple is a constant. *)
Definition simple_name (s : simple) : option name :=
  match s with
  | Simple_name n _ => Some n
  | Simple_const _ => None
  end.

(* ================================================================== *)
(* 3. Modes and effect axes needed by term syntax                     *)
(* ================================================================== *)

(* Lambda.locality_mode (code metadata result_mode). *)
Inductive locality_mode :=
  LM_heap
| LM_local.

(* ENCODING NOTE: Alloc_mode.For_assignments.t lives here rather than
   in Base.v: Base's mode set was frozen green without it, and its
   only consumers are Init_or_assign / Write_offset below. *)
Inductive alloc_mode_assign :=
  Assign_heap
| Assign_local.

(* Asttypes.mutable_flag (two-valued; used by Read_offset). *)
Inductive mutable_flag :=
  MF_immutable
| MF_mutable.

(* ENCODING NOTE: Effects.t / Coeffects.t are defined here (not in the
   ch. 06 effects-axes file) because call_kind's C_call carries them;
   PrimMemoryA.v reuses these very definitions for the effects axes. *)
Inductive effects :=
(** RULE P.Effects.NoEffects (STATUS normative) -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/effects.ml#t *)
  No_effects
(** RULE P.Effects.OnlyGenerative (STATUS normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/effects.ml#t *)
| Only_generative_effects (mut : mutability)
(** RULE P.Effects.Arbitrary (STATUS normative) -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/effects.ml#t *)
| Arbitrary_effects.

(** RULE P.Effects.Coeffects (STATUS normative) -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/coeffects.ml#t *)
Inductive coeffects :=
  No_coeffects
| Has_coeffects.

(* ================================================================== *)
(* 4. Primitive descriptor enums (flambda_primitive.mli modules;      *)
(*    06-primitives-memory.md section 1)                              *)
(* ================================================================== *)

Inductive lazy_block_tag :=
  Lazy_tag
| Forward_tag.

(* Block_kind.t.  Tag.Scannable is encoded as plain tag. *)
Inductive block_kind :=
  BK_values (t : tag) (field_kinds : list kind_ws)
| BK_naked_floats
| BK_mixed (t : tag) (shape : mixed_block_shape).

(* Array_kind.t (recursive: unboxed products nest). *)
Inductive array_kind :=
  AK_immediates
| AK_gc_ignorable_values
| AK_values
| AK_naked_floats
| AK_naked_float32s
| AK_naked_ints
| AK_naked_int8s
| AK_naked_int16s
| AK_naked_int32s
| AK_naked_int64s
| AK_naked_nativeints
| AK_naked_vec128s
| AK_naked_vec256s
| AK_naked_vec512s
| AK_unboxed_product (kinds : list array_kind).

Inductive array_kind_for_length :=
  AKL_array_kind (ak : array_kind)
| AKL_float_array_opt_dynamic.

Inductive init_or_assign :=
  IOA_initialization
| IOA_assignment (am : alloc_mode_assign).

Inductive array_load_kind :=
  ALK_immediates
| ALK_gc_ignorable_values
| ALK_values
| ALK_naked_floats
| ALK_naked_float32s
| ALK_naked_ints
| ALK_naked_int8s
| ALK_naked_int16s
| ALK_naked_int32s
| ALK_naked_int64s
| ALK_naked_nativeints
| ALK_naked_vec128s
| ALK_naked_vec256s
| ALK_naked_vec512s.

Inductive array_set_kind :=
  ASK_immediates
| ASK_gc_ignorable_values
| ASK_values (ia : init_or_assign)
| ASK_naked_floats
| ASK_naked_float32s
| ASK_naked_ints
| ASK_naked_int8s
| ASK_naked_int16s
| ASK_naked_int32s
| ASK_naked_int64s
| ASK_naked_nativeints
| ASK_naked_vec128s
| ASK_naked_vec256s
| ASK_naked_vec512s.

Inductive duplicate_block_kind :=
  DBK_values (t : tag) (length : Z)
| DBK_naked_floats (length : Z)
| DBK_mixed.

Inductive duplicate_array_kind :=
  DAK_immediates
| DAK_values
| DAK_naked_floats (length : option Z)
| DAK_naked_float32s (length : option Z)
| DAK_naked_ints (length : option Z)
| DAK_naked_int8s (length : option Z)
| DAK_naked_int16s (length : option Z)
| DAK_naked_int32s (length : option Z)
| DAK_naked_int64s (length : option Z)
| DAK_naked_nativeints (length : option Z)
| DAK_naked_vec128s (length : option Z)
| DAK_naked_vec256s (length : option Z)
| DAK_naked_vec512s (length : option Z).

Inductive block_access_field_kind :=
  BAFK_any_value
| BAFK_immediate.

Inductive mixed_block_access_field_kind :=
  MBAFK_value_prefix (fk : block_access_field_kind)
| MBAFK_flat_suffix (fse : flat_suffix_element).

Inductive block_access_kind :=
  BAK_values (t : or_unknown tag) (sz : or_unknown Z)
    (fk : block_access_field_kind)
| BAK_naked_floats (sz : or_unknown Z)
| BAK_mixed (t : or_unknown tag) (sz : or_unknown Z)
    (fk : mixed_block_access_field_kind) (shape : mixed_block_shape).

Inductive string_or_bytes :=
  SB_string
| SB_bytes.

(* ENCODING NOTE: flambda_primitive.mli's ['s comparison] is renamed
   prim_comparison with PC_* constructors: Stdlib already defines a
   [comparison] type (Eq/Lt/Gt) in Datatypes. *)
Inductive prim_comparison (S : Type) :=
  PC_eq
| PC_neq
| PC_lt (s : S)
| PC_gt (s : S)
| PC_le (s : S)
| PC_ge (s : S).
Arguments PC_eq {S}.
Arguments PC_neq {S}.
Arguments PC_lt {S} s.
Arguments PC_gt {S} s.
Arguments PC_le {S} s.
Arguments PC_ge {S} s.

Inductive equality_comparison :=
  EC_eq
| EC_neq.

Inductive comparison_behaviour (S : Type) :=
  CB_yielding_bool (c : prim_comparison S)
| CB_yielding_int_like_compare_functions (s : S).
Arguments CB_yielding_bool {S} c.
Arguments CB_yielding_int_like_compare_functions {S} s.

Inductive signed_or_unsigned :=
  Signed
| Unsigned.

Inductive bigarray_kind :=
  BGK_float16
| BGK_float32
| BGK_float32_t
| BGK_float64
| BGK_sint8
| BGK_uint8
| BGK_sint16
| BGK_uint16
| BGK_int32
| BGK_int64
| BGK_int_width_int
| BGK_targetint_width_int
| BGK_complex32
| BGK_complex64.

Inductive bigarray_layout :=
  BL_c
| BL_fortran.

Inductive string_accessor_width :=
  SAW_eight
| SAW_eight_signed
| SAW_sixteen
| SAW_sixteen_signed
| SAW_thirty_two
| SAW_single
| SAW_sixty_four
| SAW_one_twenty_eight (aligned : bool)
| SAW_two_fifty_six (aligned : bool)
| SAW_five_twelve (aligned : bool).

Inductive float_bitwidth :=
  FBW_float32
| FBW_float64.

Inductive string_like_value :=
  SLV_string
| SLV_bytes
| SLV_bigstring.

Inductive bytes_like_value :=
  BLV_bytes
| BLV_bigstring.

(* Flambda_kind.Standard_int.t *)
Inductive standard_int :=
  SI_tagged_immediate
| SI_naked_immediate
| SI_naked_int8
| SI_naked_int16
| SI_naked_int32
| SI_naked_int64
| SI_naked_nativeint.

(* Flambda_kind.Standard_int_or_float.t *)
Inductive standard_int_or_float :=
  SIF_tagged_immediate
| SIF_naked_immediate
| SIF_naked_float32
| SIF_naked_float
| SIF_naked_int8
| SIF_naked_int16
| SIF_naked_int32
| SIF_naked_int64
| SIF_naked_nativeint.

(* Flambda_kind.Boxable_number.t *)
Inductive boxable_number :=
  BN_naked_float32
| BN_naked_float
| BN_naked_int32
| BN_naked_int64
| BN_naked_nativeint
| BN_naked_vec128
| BN_naked_vec256
| BN_naked_vec512.

Inductive unary_int_arith_op :=
  UIA_swap_byte_endianness.

Inductive unary_float_arith_op :=
  UFA_abs
| UFA_neg.

Inductive reinterpret_64_bit_word :=
  R64_tagged_int63_as_unboxed_int64
| R64_unboxed_int64_as_tagged_int63
| R64_unboxed_int64_as_unboxed_float64
| R64_unboxed_float64_as_unboxed_int64.

Inductive binary_int_arith_op :=
  BIA_add
| BIA_sub
| BIA_mul
| BIA_div
| BIA_mod
| BIA_and
| BIA_or
| BIA_xor.

Inductive int_shift_op :=
  IS_lsl
| IS_lsr
| IS_asr.

Inductive binary_float_arith_op :=
  BFA_add
| BFA_sub
| BFA_mul
| BFA_div.

Inductive int_atomic_op :=
  IAO_fetch_add
| IAO_add
| IAO_sub
| IAO_and
| IAO_or
| IAO_xor.

Inductive write_offset_kind :=
  WOK_into_block
| WOK_into_block_or_off_heap.

(* term_basics/empty_array_kind.mli *)
Inductive empty_array_kind :=
  EAK_values_or_immediates_or_naked_floats
| EAK_unboxed_products
| EAK_naked_float32s
| EAK_naked_ints
| EAK_naked_int8s
| EAK_naked_int16s
| EAK_naked_int32s
| EAK_naked_int64s
| EAK_naked_nativeints
| EAK_naked_vec128s
| EAK_naked_vec256s
| EAK_naked_vec512s.

(* ================================================================== *)
(* 5. Primitive operators, by arity (flambda_primitive.mli)           *)
(* ================================================================== *)

Inductive nullary_primitive :=
  NP_invalid (k : kind)
| NP_optimised_out (k : kind)
| NP_probe_is_enabled (name : string) (enabled_at_init : option bool)
| NP_enter_inlined_apply  (* Inlined_debuginfo payload elided *)
| NP_dls_get
| NP_tls_get
| NP_domain_index
| NP_poll
| NP_cpu_relax.

Inductive unary_primitive :=
  UP_block_load (kind : block_access_kind) (mut : mutability)
    (field : Z)
| UP_duplicate_block (kind : duplicate_block_kind)
| UP_duplicate_array (kind : duplicate_array_kind)
    (source_mutability : mutability)
    (destination_mutability : mutability)
| UP_is_int (variant_only : bool)
| UP_is_null
| UP_get_tag
| UP_array_length (akl : array_kind_for_length)
| UP_bigarray_length (dimension : nat)
| UP_string_length (sb : string_or_bytes)
| UP_int_as_pointer (am : alloc_mode_alloc)
| UP_opaque_identity (middle_end_only : bool) (k : kind)
| UP_int_arith (k : standard_int) (op : unary_int_arith_op)
| UP_float_arith (w : float_bitwidth) (op : unary_float_arith_op)
| UP_num_conv (src : standard_int_or_float)
    (dst : standard_int_or_float)
| UP_boolean_not
| UP_reinterpret_64_bit_word (r : reinterpret_64_bit_word)
| UP_reinterpret_boxed_vector
| UP_unbox_number (bn : boxable_number)
| UP_box_number (bn : boxable_number) (am : alloc_mode_alloc)
| UP_untag_immediate
| UP_tag_immediate
| UP_project_function_slot (move_from : function_slot)
    (move_to : function_slot)
| UP_project_value_slot (project_from : function_slot)
    (vs : value_slot)
| UP_is_boxed_float
| UP_is_flat_float_array
| UP_end_region (ghost : bool)
| UP_end_try_region (ghost : bool)
| UP_obj_dup
| UP_get_header
| UP_peek (k : standard_int_or_float)
| UP_make_lazy (lt : lazy_block_tag).

Inductive binary_primitive :=
  BP_block_set (kind : block_access_kind) (init : init_or_assign)
    (field : Z)
| BP_array_load (ak : array_kind) (alk : array_load_kind)
    (mut : mutability)
| BP_string_or_bigstring_load (slv : string_like_value)
    (w : string_accessor_width)
| BP_bigarray_load (dims : nat) (bk : bigarray_kind)
    (bl : bigarray_layout)
| BP_phys_equal (ec : equality_comparison)
| BP_int_arith (k : standard_int) (op : binary_int_arith_op)
| BP_int_shift (k : standard_int) (op : int_shift_op)
| BP_int_comp (k : standard_int)
    (cb : comparison_behaviour signed_or_unsigned)
| BP_float_arith (w : float_bitwidth) (op : binary_float_arith_op)
| BP_float_comp (w : float_bitwidth) (cb : comparison_behaviour unit)
| BP_bigarray_get_alignment (align : Z)
| BP_atomic_load_field (fk : block_access_field_kind)
| BP_poke (k : standard_int_or_float)
| BP_read_offset (kw : kind_ws) (mf : mutable_flag).

Inductive ternary_primitive :=
  TP_array_set (ak : array_kind) (ask : array_set_kind)
| TP_bytes_or_bigstring_set (blv : bytes_like_value)
    (w : string_accessor_width)
| TP_bigarray_set (dims : nat) (bk : bigarray_kind)
    (bl : bigarray_layout)
| TP_atomic_field_int_arith (op : int_atomic_op)
| TP_atomic_set_field (fk : block_access_field_kind)
| TP_atomic_exchange_field (fk : block_access_field_kind)
| TP_write_offset (wok : write_offset_kind) (kw : kind_ws)
    (am : alloc_mode_assign).

Inductive quaternary_primitive :=
  QP_atomic_compare_and_set_field (fk : block_access_field_kind)
| QP_atomic_compare_exchange_field
    (atomic_kind : block_access_field_kind)
    (args_kind : block_access_field_kind).

Inductive variadic_primitive :=
  VP_begin_region (ghost : bool)
| VP_begin_try_region (ghost : bool)
| VP_make_block (bk : block_kind) (mut : mutability)
    (am : alloc_mode_alloc)
| VP_make_array (ak : array_kind) (mut : mutability)
    (am : alloc_mode_alloc).

(* The application of a primitive to its arguments
   (flambda_primitive.mli#t). *)
Inductive prim :=
  P_nullary (op : nullary_primitive)
| P_unary (op : unary_primitive) (s : simple)
| P_binary (op : binary_primitive) (s1 s2 : simple)
| P_ternary (op : ternary_primitive) (s1 s2 s3 : simple)
| P_quaternary (op : quaternary_primitive) (s1 s2 s3 s4 : simple)
| P_variadic (op : variadic_primitive) (args : list simple).

(* The bare operator, without its arguments: denotation judgments
   consume the operator plus already-evaluated argument VALUES
   ([[p]](vbar; H)), so they are typed over prim_op, with prim_op_of /
   prim_args mediating at the OS.Let.Prim rules (approved
   frozen-interface amendment). *)
Inductive prim_op :=
  Op_nullary (op : nullary_primitive)
| Op_unary (op : unary_primitive)
| Op_binary (op : binary_primitive)
| Op_ternary (op : ternary_primitive)
| Op_quaternary (op : quaternary_primitive)
| Op_variadic (op : variadic_primitive).

Definition prim_op_of (p : prim) : prim_op :=
  match p with
  | P_nullary op => Op_nullary op
  | P_unary op _ => Op_unary op
  | P_binary op _ _ => Op_binary op
  | P_ternary op _ _ _ => Op_ternary op
  | P_quaternary op _ _ _ _ => Op_quaternary op
  | P_variadic op _ => Op_variadic op
  end.

Definition prim_args (p : prim) : list simple :=
  match p with
  | P_nullary _ => []
  | P_unary _ s => [s]
  | P_binary _ s1 s2 => [s1; s2]
  | P_ternary _ s1 s2 s3 => [s1; s2; s3]
  | P_quaternary _ s1 s2 s3 s4 => [s1; s2; s3; s4]
  | P_variadic _ args => args
  end.

(* ================================================================== *)
(* 6. Call kinds and application apparatus (02-syntax.md, Apply /     *)
(*    Apply_cont / Switch; call_kind.mli, apply_expr.mli,             *)
(*    exn_continuation.mli, apply_cont_expr.mli, trap_action.mli,     *)
(*    switch_expr.mli)                                                *)
(* ================================================================== *)

Inductive function_call :=
  FC_direct (cid : code_id)
| FC_indirect_unknown_arity
| FC_indirect_known_arity (callees : or_unknown (list code_id)).

Inductive method_kind :=
  MK_self
| MK_public
| MK_cached.

(* Effect operations carry their operands as Simples INSIDE the call
   kind (WF.Syntax.EffectCalleeNone below). *)
Inductive effect_op :=
  Eff_perform (eff : simple)
| Eff_reperform (eff : simple) (cont : simple) (last_fiber : simple)
| Eff_with_stack (valuec : simple) (exnc : simple) (effc : simple)
    (f : simple) (arg : simple)
| Eff_with_stack_preemptible (valuec : simple) (exnc : simple)
    (effc : simple) (handle_tick : simple) (f : simple) (arg : simple)
| Eff_resume (cont : simple) (f : simple) (arg : simple).

Inductive call_kind :=
  CK_function (fc : function_call)
| CK_method (mk : method_kind) (obj : simple)
| CK_c_call (needs_caml_c_call : bool) (is_c_builtin : bool)
    (eff : effects) (coeff : coeffects)
| CK_effect (eff : effect_op).

(* rc ::= Return k | Never_returns *)
Inductive result_continuation :=
  RC_return (k : continuation)
| RC_never_returns.

Record exn_continuation := Mk_exn_continuation {
  ec_exn_handler : continuation;
  ec_extra_args : list (simple * kind_ws)
}.

Inductive raise_kind :=
  RK_regular
| RK_reraise
| RK_no_trace.

Inductive trap_action :=
  Trap_push (exn_handler : continuation)
| Trap_pop (exn_handler : continuation) (rk : option raise_kind).

Record apply_cont_expr := Mk_apply_cont {
  ac_continuation : continuation;
  ac_args : list simple;
  ac_trap_action : option trap_action
}.

(* ENCODING NOTE: the arms map { i |-> ac } is an association list
   keyed by target_ocaml_int (= Z); finiteness is needed for
   WF.Syntax.SwitchMinArms.  There is no default-case field, matching
   the doc ("There is no default case"). *)
Record switch_expr := Mk_switch {
  sw_scrutinee : simple;
  sw_arms : list (target_ocaml_int * apply_cont_expr)
}.

(* ENCODING NOTE: apply_expr's optimizer bookkeeping (inlined,
   inlining_state, probe, position, dbg, relative_history) is elided;
   the semantically relevant fields are kept in doc order. *)
Record apply_expr := Mk_apply {
  ap_callee : option simple;
  ap_args : list simple;
  ap_result_continuation : result_continuation;
  ap_exn_continuation : exn_continuation;
  (* ENCODING NOTE: the doc's [`Complex]/[`Unarized] phantom distinction
     on arities is not modeled; both apply_expr's and code0's arity
     fields share the one arity type.  WF.Arity.ApplyFlavours
     (WellFormed.v) states the unarizedness constraint directly. *)
  ap_args_arity : arity;
  ap_return_arity : arity;
  ap_call_kind : call_kind;
  ap_alloc_mode : alloc_mode_app
}.

(* ================================================================== *)
(* 7. Binders (02-syntax.md, Binders; bound_pattern.mli,              *)
(*    bound_var.ml, bound_static.mli)                                 *)
(* ================================================================== *)

Record bound_var := Mk_bound_var {
  bv_var : variable;
  bv_name_mode : name_mode
}.

(* ENCODING NOTE: Function_slot.Lmap's ordered maps are association
   lists throughout (declaration order is preserved and semantic for
   layout). *)
Inductive bound_static_pattern :=
  BSP_code (cid : code_id)
| BSP_set_of_closures (closure_syms : list (function_slot * symbol))
| BSP_block_like (sym : symbol).

Inductive bound_pattern :=
  BPat_singleton (bv : bound_var)
| BPat_set_of_closures (bvs : list bound_var)
| BPat_static (bst : list bound_static_pattern).

Definition bound_pattern_vars (p : bound_pattern) : list variable :=
  match p with
  | BPat_singleton bv => [bv_var bv]
  | BPat_set_of_closures bvs => map bv_var bvs
  | BPat_static _ => []
  end.

(* ================================================================== *)
(* 8. Sets of closures (02-syntax.md; set_of_closures.mli,            *)
(*    function_declarations.mli)                                      *)
(* ================================================================== *)

(* function_declarations.mli#code_id_in_function_declaration
   (dbg elided; function_slot_size retained -- it matters for
   layout in ch. 18). *)
Inductive code_id_in_fd :=
  FD_deleted (function_slot_size : Z)
| FD_code_id (cid : code_id) (only_full_applications : bool).

Record set_of_closures := Mk_set_of_closures {
  soc_function_decls : list (function_slot * code_id_in_fd);
  soc_value_slots : list (value_slot * simple)
}.

(* ================================================================== *)
(* 9. Static constants (02-syntax.md; static_const.mli)               *)
(* ================================================================== *)

(* ov ::= (literal) | Var x  -- Or_variable.t *)
Inductive or_variable (A : Type) :=
  OV_const (a : A)
| OV_var (x : variable).
Arguments OV_const {A} a.
Arguments OV_var {A} x.

Inductive static_const :=
  SC_set_of_closures (soc : set_of_closures)
| SC_block (t : tag) (mut : mutability)
    (shape : scannable_block_shape) (fields : list simple)
| SC_boxed_float32 (v : or_variable float32)
| SC_boxed_float (v : or_variable float64)
| SC_boxed_int32 (v : or_variable Z)
| SC_boxed_int64 (v : or_variable Z)
| SC_boxed_nativeint (v : or_variable Z)
| SC_boxed_vec128 (v : or_variable vec128)
| SC_boxed_vec256 (v : or_variable vec256)
| SC_boxed_vec512 (v : or_variable vec512)
| SC_immutable_float_block (fields : list (or_variable float64))
| SC_immutable_float_array (fields : list (or_variable float64))
| SC_immutable_float32_array (fields : list (or_variable float32))
| SC_immutable_int_array (fields : list (or_variable Z))
| SC_immutable_int8_array (fields : list (or_variable Z))
| SC_immutable_int16_array (fields : list (or_variable Z))
| SC_immutable_int32_array (fields : list (or_variable Z))
| SC_immutable_int64_array (fields : list (or_variable Z))
| SC_immutable_nativeint_array (fields : list (or_variable Z))
| SC_immutable_vec128_array (fields : list (or_variable vec128))
| SC_immutable_vec256_array (fields : list (or_variable vec256))
| SC_immutable_vec512_array (fields : list (or_variable vec512))
| SC_immutable_value_array (fields : list simple)
| SC_empty_array (eak : empty_array_kind)
| SC_mutable_string (initial_value : string)
| SC_immutable_string (s : string).

(* ================================================================== *)
(* 10. Expressions (02-syntax.md, Expressions; flambda.mli)           *)
(* ================================================================== *)

(* The mutual block contains exactly the types that recursively
   contain expr: expr itself, named (via static consts), continuation
   handlers, static_const_or_code, and code0 (function bodies). *)

Inductive expr :=
  E_let (p : bound_pattern) (defining : named) (body : expr)
  (* ENCODING NOTE: the Non_recursive optimizer hints
     (num_free_occurrences, is_applied_with_traps, can_be_lifted) and
     Let's free_names_of_body are elided. *)
| E_let_cont_nonrec (k : continuation) (h : cont_handler) (body : expr)
| E_let_cont_rec (invariant_params : list (variable * kind_ws))
    (handlers : list (continuation * cont_handler)) (body : expr)
| E_apply (ap : apply_expr)
| E_apply_cont (ac : apply_cont_expr)
| E_switch (sw : switch_expr)
| E_invalid (message : string)

with named :=
  N_simple (s : simple)
| N_prim (p : prim)
| N_set_of_closures (soc : set_of_closures) (am : alloc_mode_alloc)
| N_static_consts (scg : list static_const_or_code)
| N_rec_info (ri : rec_info_expr)

(* H ::= lambda(params). handler  with is_exn_handler, is_cold *)
with cont_handler :=
  Mk_cont_handler (params : list (variable * kind_ws)) (handler : expr)
    (is_exn_handler : bool) (is_cold : bool)

with static_const_or_code :=
  SCC_code (c : code0)
| SCC_deleted_code
| SCC_static_const (sc : static_const)

(* Code0.t = params-and-body abstraction + the semantically relevant
   code metadata (02-syntax.md, Code; code0.mli, code_metadata.mli).
   Metadata fields not load-bearing for the semantics (param_modes,
   first_complex_local_param, recursive, stub, is_my_closure_used,
   newer_version_of, result_types, inlining metadata) are elided.
   In particular newer_version_of's code-aging relation lives on the
   typing side as tenv.te_code_age (TypeGrammar.v); readers looking
   for code aging should follow that pointer. *)
with code0 :=
  Mk_code0
    (return_continuation : continuation)
    (exn_continuation : continuation)
    (params : list (variable * kind_ws))
    (body : expr)
    (my_closure : variable)
    (my_depth : variable)
    (my_alloc_mode : alloc_mode_app)
    (params_arity : arity)
    (result_arity : arity)
    (result_mode : locality_mode)
    (is_tupled : bool).

(* Accessors (the mutual block cannot use Record syntax). *)

Definition ch_params (h : cont_handler) : list (variable * kind_ws) :=
  match h with Mk_cont_handler ps _ _ _ => ps end.
Definition ch_handler (h : cont_handler) : expr :=
  match h with Mk_cont_handler _ e _ _ => e end.
Definition ch_is_exn_handler (h : cont_handler) : bool :=
  match h with Mk_cont_handler _ _ b _ => b end.
Definition ch_is_cold (h : cont_handler) : bool :=
  match h with Mk_cont_handler _ _ _ b => b end.

Definition c0_return_continuation (c : code0) : continuation :=
  match c with Mk_code0 k _ _ _ _ _ _ _ _ _ _ => k end.
Definition c0_exn_continuation (c : code0) : continuation :=
  match c with Mk_code0 _ k _ _ _ _ _ _ _ _ _ => k end.
Definition c0_params (c : code0) : list (variable * kind_ws) :=
  match c with Mk_code0 _ _ ps _ _ _ _ _ _ _ _ => ps end.
Definition c0_body (c : code0) : expr :=
  match c with Mk_code0 _ _ _ e _ _ _ _ _ _ _ => e end.
Definition c0_my_closure (c : code0) : variable :=
  match c with Mk_code0 _ _ _ _ x _ _ _ _ _ _ => x end.
Definition c0_my_depth (c : code0) : variable :=
  match c with Mk_code0 _ _ _ _ _ x _ _ _ _ _ => x end.
Definition c0_my_alloc_mode (c : code0) : alloc_mode_app :=
  match c with Mk_code0 _ _ _ _ _ _ am _ _ _ _ => am end.
Definition c0_params_arity (c : code0) : arity :=
  match c with Mk_code0 _ _ _ _ _ _ _ a _ _ _ => a end.
Definition c0_result_arity (c : code0) : arity :=
  match c with Mk_code0 _ _ _ _ _ _ _ _ a _ _ => a end.
Definition c0_result_mode (c : code0) : locality_mode :=
  match c with Mk_code0 _ _ _ _ _ _ _ _ _ m _ => m end.
Definition c0_is_tupled (c : code0) : bool :=
  match c with Mk_code0 _ _ _ _ _ _ _ _ _ _ b => b end.

(* The code table of the unit under compilation: what find_code_exn
   consults. Threaded as the leading argument of every rw_* relation
   (code is defined BY the unit, not an external oracle; tenv rightly
   excludes it, as Simplify's denv carries code separately from the
   types env). *)
Definition code_env := fmap code_id code0.

(* ================================================================== *)
(* 11. The compilation unit (02-syntax.md; flambda_unit.mli)          *)
(* ================================================================== *)

(* ENCODING NOTE: used_value_slots (cross-unit bookkeeping) elided. *)
Record flambda_unit := Mk_flambda_unit {
  fu_return_continuation : continuation;
  fu_exn_continuation : continuation;
  fu_toplevel_my_region : variable;
  fu_toplevel_my_ghost_region : variable;
  fu_module_symbol : symbol;
  fu_body : expr
}.

(* ================================================================== *)
(* 12. replace_apply_cont                                             *)
(* ================================================================== *)

(* e[ apply_cont k abar |-> f abar ]: the substitution operation of
   S.Rewrite.LetCont.Inline / S.Rewrite.LetCont.Shortcut
   (10-simplify-rewrites.md).  Replaces every expression-position
   Apply_cont targeting k that carries NO trap action (matching the
   Inlinable use-kind premise); trap-action-bearing uses and switch-arm
   actions (which are apply_cont_exprs, not exprs, and are
   Non_inlinable uses) are left alone.
   ENCODING NOTE: scope-aware -- rebinding of k (by a Let_cont or by a
   code0's return/exn continuations) stops the replacement, since the
   docs' substitution is on the free occurrences of k. *)

Section ReplaceApplyCont.

Variable k : continuation.
Variable f : list simple -> expr.

Fixpoint replace_apply_cont (e : expr) : expr :=
  match e with
  | E_let p n body =>
      E_let p (rac_named n) (replace_apply_cont body)
  | E_let_cont_nonrec k' h body =>
      (* k' scopes over the body only (non-recursive). *)
      E_let_cont_nonrec k' (rac_handler h)
        (if continuation_eqb k' k then body
         else replace_apply_cont body)
  | E_let_cont_rec inv handlers body =>
      if existsb (fun kh => continuation_eqb (fst kh) k) handlers
      then e  (* k rebound over both body and handlers *)
      else
        E_let_cont_rec inv
          ((fix go (hs : list (continuation * cont_handler)) :=
              match hs with
              | [] => []
              | (k', h) :: tl => (k', rac_handler h) :: go tl
              end) handlers)
          (replace_apply_cont body)
  | E_apply _ => e
  | E_apply_cont ac =>
      match ac with
      | Mk_apply_cont k'' args trap =>
          if continuation_eqb k'' k
          then match trap with
               | None => f args
               | Some _ => e
               end
          else e
      end
  | E_switch _ => e
  | E_invalid _ => e
  end

with rac_named (n : named) : named :=
  match n with
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ => n
  | N_static_consts scg =>
      N_static_consts
        ((fix go (l : list static_const_or_code) :=
            match l with
            | [] => []
            | s :: tl => rac_scc s :: go tl
            end) scg)
  end

with rac_handler (h : cont_handler) : cont_handler :=
  match h with
  | Mk_cont_handler ps body exn cold =>
      Mk_cont_handler ps (replace_apply_cont body) exn cold
  end

with rac_scc (s : static_const_or_code) : static_const_or_code :=
  match s with
  | SCC_code c => SCC_code (rac_code c)
  | SCC_deleted_code | SCC_static_const _ => s
  end

with rac_code (c : code0) : code0 :=
  match c with
  | Mk_code0 kret kexn ps body clo dep am pa ra rm tup =>
      if continuation_eqb kret k || continuation_eqb kexn k
      then c
      else Mk_code0 kret kexn ps (replace_apply_cont body)
             clo dep am pa ra rm tup
  end.

End ReplaceApplyCont.

(* ================================================================== *)
(* 13. Structural well-formedness (02-syntax.md, WF.Syntax rules)     *)
(* ================================================================== *)

(** RULE WF.Syntax.Anf (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/terms/flambda.mli#expr_descr *)
(* ENCODING NOTE: the ANF invariant ("the defining expression of a Let
   never affects control flow") is enforced BY CONSTRUCTION: [named]
   is a separate syntactic category with no Apply / Apply_cont /
   Switch / Invalid constructor, so no defining expression can branch,
   jump, or return.  The semantic counterpart is that every OS.Let.*
   rule of 04-opsem.md continues with the Let's own body.  The theorem
   below records the by-construction status (there is no residual
   proposition to require of terms). *)
Theorem WF_Syntax_Anf : True.
Proof. exact I. Qed.

(** RULE WF.Syntax.LetKindUniform (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/bound_identifiers/bound_pattern.mli#t *)
(* Premise: Let (P = n) e binds x1 ... xn (n > 1 only for
   Set_of_closures).  Conclusion: all xi have the same kind, relative
   to a kind assignment for the bound variables (supplied by the
   kinding judgment of WellFormed.v). *)
Definition WF_Syntax_LetKindUniform (kind_of : variable -> kind)
    (p : bound_pattern) : Prop :=
  forall x y,
    In x (bound_pattern_vars p) ->
    In y (bound_pattern_vars p) ->
    kind_of x = kind_of y.

(** RULE WF.Syntax.SingletonNotSetOfClosures (STATUS normative)
    -- 02-syntax.md
    CODE middle_end/flambda2/bound_identifiers/bound_pattern.mli#t *)
Definition WF_Syntax_SingletonNotSetOfClosures
    (p : bound_pattern) (n : named) : Prop :=
  match p, n with
  | BPat_singleton _, N_set_of_closures _ _ => False
  | _, _ => True
  end.

(* Target_ocaml_int.t range: (machine_width - 1)-bit signed. *)
Definition in_target_ocaml_int_range (i : Z) : Prop :=
  wrap (machine_width - 1)%Z i = i.

(** RULE WF.Syntax.SwitchScrutinee (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/terms/switch_expr.mli#t
    CODE middle_end/flambda2/terms/switch_expr.mli#create *)
(* The structural residue: each discriminant is a Target_ocaml_int,
   and there is no default arm (by construction: switch_expr has no
   default field).  The scrutinee-kind premise ("s has kind
   Naked_immediate") is the kinding judgment's part; it is stated as
   WF.Switch.Scrutinee in WellFormed.v, as the doc's NOTES direct. *)
Definition WF_Syntax_SwitchScrutinee (sw : switch_expr) : Prop :=
  Forall (fun arm => in_target_ocaml_int_range (fst arm)) (sw_arms sw).

(** RULE WF.Syntax.SwitchMinArms (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/terms/switch_expr.mli#t
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_switch
    CODE middle_end/flambda2/simplify/expr_builder.ml#create_switch
    CODE middle_end/flambda2/terms/flambda.mli#Invalid.t *)
Definition WF_Syntax_SwitchMinArms (sw : switch_expr) : Prop :=
  (2 <= length (sw_arms sw))%nat.

(** RULE WF.Syntax.ExnHandlerNonRecursive (STATUS normative)
    -- 02-syntax.md
    CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
    CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr *)
(* Stated on recursive groups: no handler in a Recursive Let_cont is
   an exception handler.  Per the doc this holds "by the time the term
   reaches Flambda_to_cmm" (transient stub recursion is permitted
   earlier). *)
Definition WF_Syntax_ExnHandlerNonRecursive
    (handlers : list (continuation * cont_handler)) : Prop :=
  Forall (fun kh => ch_is_exn_handler (snd kh) = false) handlers.

(** RULE WF.Syntax.ExnHandlerFirstParamBucket (STATUS conjectured)
    -- 02-syntax.md
    CODE middle_end/flambda2/terms/exn_continuation.mli#arity
    CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler *)
(* Premise: k is an exception handler with parameters p1 ... pm.
   Conclusion: p1 is the exception bucket; p2 ... pm correspond to the
   extra_args of any Exn_continuation.t targeting k (count and kinds
   align).  To be instantiated, in a term-WF judgment, for each
   exn_continuation ec whose handler is the continuation bound to h. *)
Definition WF_Syntax_ExnHandlerFirstParamBucket
    (h : cont_handler) (ec : exn_continuation) : Prop :=
  ch_is_exn_handler h = true ->
  length (ch_params h) = S (length (ec_extra_args ec)) /\
  map snd (tl (ch_params h)) = map snd (ec_extra_args ec).

(** RULE WF.Syntax.EffectCalleeNone (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/terms/call_kind.mli#Effect
    CODE middle_end/flambda2/terms/apply_expr.mli#create *)
Definition WF_Syntax_EffectCalleeNone (ap : apply_expr) : Prop :=
  match ap_call_kind ap with
  | CK_effect _ => ap_callee ap = None /\ ap_args ap = []
  | _ => True
  end.

(** RULE WF.Syntax.ContSecondClass (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr *)
(* ENCODING NOTE: by construction.  The type [continuation] occurs in
   the grammar only as: Apply_cont targets and trap-action handlers,
   Apply result/exception continuations, Let_cont binders, and code0's
   bound return/exception continuations.  [simple], [const], and the
   static-const grammar have no continuation constructor, so
   continuations cannot be stored in variables, symbols, or blocks;
   and cont_handler closes over nothing (no environment field).  As
   with WF.Syntax.Anf, no residual proposition remains to require. *)
Theorem WF_Syntax_ContSecondClass : True.
Proof. exact I. Qed.

(** RULE WF.Syntax.NonRecOccursPositive (STATUS conjectured)
    -- 02-syntax.md
    CODE middle_end/flambda2/terms/flambda.mli#let_cont_expr
    CODE middle_end/flambda2/terms/flambda.ml#Let_cont_expr.create_non_recursive0 *)
(* ENCODING NOTE: the rule's subject is the elided optimizer hint
   field num_free_occurrences (Known m => m > 0).  Since the field is
   not modeled (see E_let_cont_nonrec), the rule is recorded as a
   documented anchor.  The in-scope residue -- a non-recursive
   Let_cont's bound continuation has at least one free occurrence in
   its body -- is a property of compiler-produced terms only (the
   low-level constructor does not enforce it; the doc's NOTES), so no
   term-level proposition is imposed here. *)
Definition WF_Syntax_NonRecOccursPositive_documented : Prop := True.

(* Shallow symbol-mention apparatus for WF.Syntax.StaticRecThroughCode.
   Only non-code static constants can participate in a code-free
   cycle, and their symbol mentions are syntactically shallow (field
   simples and value slots); code bodies never enter the picture
   because an edge THROUGH code is exactly what the rule permits. *)

Definition simple_mentions_sym (s : simple) (sym : symbol) : Prop :=
  match s with
  | Simple_name (Name_sym s') _ => s' = sym
  | _ => False
  end.

Definition static_const_mentions_sym (sc : static_const)
    (sym : symbol) : Prop :=
  match sc with
  | SC_set_of_closures soc =>
      Exists (fun ws => simple_mentions_sym (snd ws) sym)
        (soc_value_slots soc)
  | SC_block _ _ _ fields =>
      Exists (fun s => simple_mentions_sym s sym) fields
  | SC_immutable_value_array fields =>
      Exists (fun s => simple_mentions_sym s sym) fields
  | _ => False
  end.

Definition bsp_binds_sym (bsp : bound_static_pattern)
    (sym : symbol) : Prop :=
  match bsp with
  | BSP_code _ => False
  | BSP_set_of_closures closure_syms => In sym (map snd closure_syms)
  | BSP_block_like s => s = sym
  end.

(* Edge s1 -> s2: the (non-code) definition bound to symbol s1
   mentions symbol s2.  defs pairs each bound-static pattern with its
   static_const_or_code, in group order. *)
Definition static_sym_edge
    (defs : list (bound_static_pattern * static_const_or_code))
    (s1 s2 : symbol) : Prop :=
  exists bsp sc,
    In (bsp, SCC_static_const sc) defs /\
    bsp_binds_sym bsp s1 /\
    static_const_mentions_sym sc s2.

(** RULE WF.Syntax.StaticRecThroughCode (STATUS normative)
    -- 02-syntax.md
    CODE middle_end/flambda2/bound_identifiers/bound_static.mli#create *)
(* ENCODING NOTE: "every recursive cycle among the bound names passes
   through at least one code ID" is encoded as: the symbol-to-symbol
   mention relation restricted to NON-code definitions has no cycle
   (no clos_trans loop).  A cycle passing through code never yields a
   chain of such edges, so the two statements coincide. *)
Definition WF_Syntax_StaticRecThroughCode
    (defs : list (bound_static_pattern * static_const_or_code))
    : Prop :=
  forall s, ~ clos_trans _ (static_sym_edge defs) s s.

(** RULE WF.Syntax.ImmutableArrayNonEmpty (STATUS normative)
    -- 02-syntax.md
    CODE middle_end/flambda2/terms/static_const.mli#t *)
Definition WF_Syntax_ImmutableArrayNonEmpty (sc : static_const)
    : Prop :=
  match sc with
  | SC_immutable_float_array fields => fields <> []
  | SC_immutable_float32_array fields => fields <> []
  | SC_immutable_int_array fields => fields <> []
  | SC_immutable_int8_array fields => fields <> []
  | SC_immutable_int16_array fields => fields <> []
  | SC_immutable_int32_array fields => fields <> []
  | SC_immutable_int64_array fields => fields <> []
  | SC_immutable_nativeint_array fields => fields <> []
  | SC_immutable_vec128_array fields => fields <> []
  | SC_immutable_vec256_array fields => fields <> []
  | SC_immutable_vec512_array fields => fields <> []
  | SC_immutable_value_array fields => fields <> []
  | _ => True
  end.

(** RULE WF.Syntax.NameModeInTerms (STATUS normative) -- 02-syntax.md
    CODE middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms *)
Definition WF_Syntax_NameModeInTerms (bv : bound_var) : Prop :=
  match bv_name_mode bv with
  | NM_normal | NM_phantom => True
  | NM_in_types => False
  end.

(* ================================================================== *)
(* 14. Sanctioned binder Parameters (CORRESPONDENCE.md, "Binders")    *)
(* ================================================================== *)

(* Nominal binding machinery is opaque at the statement level: these
   appear only in rule premises (capture-avoidance, freshening). *)
Parameter free_vars : expr -> variable -> Prop.
Parameter bound_vars : expr -> variable -> Prop.
Parameter alpha_eq : expr -> expr -> Prop.
(* Continuation-namespace sibling of free_vars (CORRESPONDENCE.md,
   "Binders"); used for continuation-freshness premises
   (S.Rewrite.Loopify.Body, S.Rewrite.Apply.OverApplication) and by
   S.Struct.Run.ClosedResult. *)
Parameter free_conts : expr -> continuation -> Prop.
(* Occurrence of a static constant anywhere in an expr, including
   nested through code bodies (a real walker would duplicate the
   whole grammar); consumed by KF-040's ch. 20 carve-out premise. *)
Parameter static_consts_in : expr -> static_const -> Prop.
