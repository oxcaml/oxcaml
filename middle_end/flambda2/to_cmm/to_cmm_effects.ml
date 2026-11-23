(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type effects_and_coeffects_classification =
  | Pure
  | Effect
  | Coeffect_only
  | Generative_immutable

let classify_by_effects_and_coeffects effs =
  (* See the comments on type [classification] in the .mli. *)
  match (effs : Effects_and_coeffects.t) with
  | Only_generative_effects Immutable, No_coeffects, _, _ ->
    Generative_immutable
  | Arbitrary_effects, (Has_coeffects | No_coeffects), _, _
  | ( Only_generative_effects (Mutable | Immutable | Immutable_unique),
      (Has_coeffects | No_coeffects),
      _,
      _ ) ->
    Effect
  | No_effects, Has_coeffects, _, _ -> Coeffect_only
  | No_effects, No_coeffects, _, _ -> Pure

let reads_memory (p : Flambda_primitive.t) ~is_my_closure =
  match p with
  | Nullary prim -> (
    match prim with
    | Dls_get | Tls_get | Domain_index | Thread_self_id -> true
    | Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
    | Poll | Cpu_relax ->
      false)
  | Unary (prim, arg) -> (
    match prim with
    | Project_value_slot _ -> not (is_my_closure arg)
    | Block_load _ | Duplicate_block _ | Duplicate_array _ | Get_tag
    | Array_length _ | Bigarray_length _ | String_length _ | Unbox_number _
    | Is_boxed_float | Is_flat_float_array | Obj_dup _ | Get_header | Peek _ ->
      true
    | Is_int _ | Is_null | Int_as_pointer _ | Opaque_identity _ | Int_arith _
    | Float_arith _ | Num_conv _ | Boolean_not _ | Reinterpret_64_bit_word _
    | Reinterpret_boxed_vector | Box_number _ | Untag_immediate | Tag_immediate
    | Project_function_slot _ | End_region _ | End_try_region _ | Make_lazy _ ->
      false)
  | Binary (prim, _, _) -> (
    match prim with
    | Array_load _ | String_or_bigstring_load _ | Bigarray_load _
    | Bigarray_get_alignment _ | Atomic_load _ | Read_offset _ ->
      true
    | Block_set _ | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _
    | Float_arith _ | Float_comp _ | Poke _ ->
      false)
  | Ternary
      ( ( Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
        | Atomic_int_arith _ | Atomic_set _ | Atomic_exchange _ | Write_offset _
          ),
        _,
        _,
        _ )
  | Quaternary
      ((Atomic_compare_and_set _ | Atomic_compare_exchange _), _, _, _, _)
  | Variadic
      ((Begin_region _ | Begin_try_region _ | Make_block _ | Make_array _), _)
    ->
    false

type let_binding_classification =
  | Drop_defining_expr
  | Regular
  | May_inline_once
  | Must_inline_once
  | Must_inline_and_duplicate

let classify_let_binding var
    ~(effects_and_coeffects_of_defining_expr : Effects_and_coeffects.t)
    ~(num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t) =
  match Variable.Map.find var num_normal_occurrences_of_bound_vars with
  | exception Not_found -> Regular
  | Zero -> (
    match
      classify_by_effects_and_coeffects effects_and_coeffects_of_defining_expr
    with
    | Coeffect_only | Generative_immutable | Pure -> Drop_defining_expr
    | Effect ->
      Regular
      (* Could be May_inline technically, but it doesn't matter since it can
         only be flushed by the env. *))
  | One -> (
    (* This case represents expressions that are guaranteed to be evaluated at
       most once at runtime (and thus do not include expressions inside loops).

       Any defining expression used exactly once is considered for inlining at
       this stage. The environment is going to handle the details of preserving
       the effects and coeffects ordering (if inlining without reordering is
       impossible then the expressions will be bound at some safe place
       instead).

       Whether inlining of _effectful_ expressions _actually occurs_ depends on
       the context. Currently this is very restricted, see comments in
       [To_cmm_primitive]. *)
    match effects_and_coeffects_of_defining_expr with
    | _, _, Delay, _ -> Must_inline_once
    | _, _, Strict, _ -> May_inline_once)
  | More_than_one -> (
    (* Note: expressions in loops are counted as having two occurrences to
       ensure that they fall into this case *)
    match effects_and_coeffects_of_defining_expr with
    | _, _, Delay, _ -> Must_inline_and_duplicate
    | _, _, Strict, _ -> Regular)

type continuation_handler_classification =
  | Regular
  | May_inline

let cont_is_known_to_have_exactly_one_occurrence k (num : _ Or_unknown.t) =
  match num with
  | Unknown -> false
  | Known num -> (
    match (num : Num_occurrences.t) with
    | One -> true
    | More_than_one -> false
    | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k)

let classify_continuation_handler k handler ~num_free_occurrences
    ~is_applied_with_traps : continuation_handler_classification =
  if
    (not (Continuation_handler.is_exn_handler handler))
    && (not is_applied_with_traps)
    && cont_is_known_to_have_exactly_one_occurrence k num_free_occurrences
  then May_inline
  else Regular

(* CR gyorsh: currently only used to reorder C calls in the backend. *)
let transl_c_call_effects (e : Effects.t) : Cmm.effects =
  match e with
  | No_effects -> No_effects
  | Only_generative_effects _ | Arbitrary_effects -> Arbitrary_effects

let transl_c_call_coeffects (ce : Coeffects.t) : Cmm.coeffects =
  match ce with No_coeffects -> No_coeffects | Has_coeffects -> Has_coeffects
