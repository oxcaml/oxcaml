(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-fragile-match"]

open! Flambda
module BP = Bound_parameter
module IR = Closure_conversion_aux.IR
module Acc = Closure_conversion_aux.Acc
module Env = Closure_conversion_aux.Env
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Apply_cont_with_acc = Closure_conversion_aux.Apply_cont_with_acc
module Let_cont_with_acc = Closure_conversion_aux.Let_cont_with_acc
module Let_with_acc = Closure_conversion_aux.Let_with_acc
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module K = Flambda_kind
module P = Flambda_primitive
module VB = Bound_var

type 'a close_program_metadata =
  | Normal : [`Normal] close_program_metadata
  | Classic :
      (Exported_code.t
      * Name_occurrences.t
      * Flambda_cmx_format.t option
      * Exported_offsets.t)
      -> [`Classic] close_program_metadata

type 'a close_program_result =
  { unit : Flambda_unit.t;
    metadata : 'a close_program_metadata;
    code_slot_offsets : Slot_offsets.t Code_id.Map.t
  }

type close_functions_result =
  | Lifted of (Symbol.t * Env.value_approximation) Function_slot.Lmap.t
  | Dynamic of Set_of_closures.t * Env.value_approximation Function_slot.Map.t

let manufacture_symbol acc proposed_name =
  let acc, linkage_name =
    if Flambda_features.Expert.shorten_symbol_names ()
    then Acc.manufacture_symbol_short_name acc
    else acc, Linkage_name.of_string proposed_name
  in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ()) linkage_name
  in
  acc, symbol

let declare_symbol_for_function_slot env acc ident function_slot :
    Env.t * Acc.t * Symbol.t =
  let acc, symbol =
    manufacture_symbol acc (Function_slot.to_string function_slot)
  in
  let env =
    Env.add_simple_to_substitute env ident (Simple.symbol symbol)
      K.With_subkind.any_value
  in
  env, acc, symbol

let register_const0 acc constant name =
  match Static_const.Map.find constant (Acc.shareable_constants acc) with
  | exception Not_found ->
    (* Create a variable to ensure uniqueness of the symbol. *)
    let var = Variable.create name K.value in
    let acc, symbol =
      manufacture_symbol acc
        (* CR mshinwell: this Variable.rename looks to be redundant *)
        (Variable.unique_name (Variable.rename var))
    in
    let acc = Acc.add_declared_symbol ~symbol ~constant acc in
    let acc =
      if Static_const.can_share constant
      then Acc.add_shareable_constant ~symbol ~constant acc
      else acc
    in
    acc, symbol
  | symbol -> acc, symbol

let register_const acc dbg constant name =
  let acc, symbol = register_const0 acc constant name in
  acc, Simple.With_debuginfo.create (Simple.symbol symbol) dbg, name

let rec declare_const acc dbg (const : Lambda.structured_constant) =
  let module SC = Static_const in
  let module RWC = Reg_width_const in
  let[@inline] reg_width cst =
    Simple.With_debuginfo.create (Simple.const cst) dbg
  in
  match const with
  | Const_base (Const_int c) ->
    acc, reg_width (RWC.tagged_immediate (Targetint_31_63.of_int c)), "int"
  | Const_base (Const_char c) ->
    acc, reg_width (RWC.tagged_immediate (Targetint_31_63.of_char c)), "char"
  | Const_base (Const_unboxed_float c) ->
    let c = Numeric_types.Float_by_bit_pattern.of_string c in
    acc, reg_width (RWC.naked_float c), "unboxed_float"
  | Const_base (Const_unboxed_float32 c) ->
    let c = Numeric_types.Float32_by_bit_pattern.of_string c in
    acc, reg_width (RWC.naked_float32 c), "unboxed_float32"
  | Const_base (Const_string (s, _, _)) ->
    register_const acc dbg (SC.immutable_string s) "immstring"
  | Const_base (Const_float c) ->
    let c = Numeric_types.Float_by_bit_pattern.create (float_of_string c) in
    register_const acc dbg (SC.boxed_float (Const c)) "float"
  | Const_base (Const_float32 c) ->
    let c = Numeric_types.Float32_by_bit_pattern.create (float_of_string c) in
    register_const acc dbg (SC.boxed_float32 (Const c)) "float32"
  | Const_base (Const_int32 c) ->
    register_const acc dbg (SC.boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const acc dbg (SC.boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint_32_64.of_int64 (Int64.of_nativeint c) in
    register_const acc dbg (SC.boxed_nativeint (Const c)) "nativeint"
  | Const_base (Const_unboxed_int32 c) ->
    acc, reg_width (RWC.naked_int32 c), "unboxed_int32"
  | Const_base (Const_unboxed_int64 c) ->
    acc, reg_width (RWC.naked_int64 c), "unboxed_int64"
  | Const_base (Const_unboxed_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint_32_64.of_int64 (Int64.of_nativeint c) in
    acc, reg_width (RWC.naked_nativeint c), "unboxed_nativeint"
  | Const_immstring c ->
    register_const acc dbg (SC.immutable_string c) "immstring"
  | Const_float_block c ->
    register_const acc dbg
      (SC.immutable_float_block
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_block"
  | Const_float_array c ->
    register_const acc dbg
      (SC.immutable_float_array
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_array"
  | Const_block (tag, consts) ->
    let acc, fields =
      List.fold_left_map
        (fun acc c ->
          let acc, field, _ = declare_const acc dbg c in
          Simple.pattern_match'
            (Simple.With_debuginfo.simple field)
            ~var:(fun _var ~coercion:_ ->
              Misc.fatal_errorf
                "Did not expect field %a of [Const_block] to be a variable:@ %a"
                Simple.With_debuginfo.print field
                Printlambda.structured_constant const)
            ~symbol:(fun _sym ~coercion:_ -> ())
            ~const:(fun cst ->
              match RWC.descr cst with
              | Tagged_immediate _ | Null -> ()
              | Naked_immediate _ | Naked_float32 _ | Naked_float _
              | Naked_int8 _ | Naked_int16 _ | Naked_int32 _ | Naked_int64 _
              | Naked_nativeint _ | Naked_vec128 _ | Naked_vec256 _
              | Naked_vec512 _ ->
                Misc.fatal_errorf
                  "Unboxed constants are not allowed inside of Const_block: %a"
                  Printlambda.structured_constant const);
          acc, field)
        acc consts
    in
    let const : SC.t =
      SC.block (Tag.Scannable.create_exn tag) Immutable Value_only fields
    in
    register_const acc dbg const "const_block"
  | Const_mixed_block (tag, shape, args) ->
    let shape =
      Mixed_block_shape.of_mixed_block_elements
        ~print_locality:(fun ppf () -> Format.fprintf ppf "()")
        shape
    in
    let unbox_float_constant (c : Lambda.structured_constant) :
        Lambda.structured_constant =
      match c with
      | Const_base (Const_float f) -> Const_base (Const_unboxed_float f)
      | Const_base
          ( Const_int _ | Const_char _ | Const_string _ | Const_float32 _
          | Const_unboxed_float _ | Const_unboxed_float32 _ | Const_int32 _
          | Const_int64 _ | Const_nativeint _ | Const_unboxed_int32 _
          | Const_unboxed_int64 _ | Const_unboxed_nativeint _ )
      | Const_block _ | Const_mixed_block _ | Const_float_array _
      | Const_immstring _ | Const_float_block _ | Const_null ->
        Misc.fatal_errorf
          "In constant mixed block, a field of kind\n\
          \       Float_boxed contained the  constant %a"
          Printlambda.structured_constant c
    in
    (* CR mshinwell: factor out, this is also in the Pmakemixedblock case. Or
       even better, add support for lifting mixed blocks, then remove this
       special handling for Const_block and Const_mixed_block and use that
       (mshinwell has a partial patch for this). *)
    let args =
      let new_indexes_to_old_indexes =
        Mixed_block_shape.new_indexes_to_old_indexes shape
      in
      let args = Array.of_list args in
      Array.init (Array.length args) (fun new_index ->
          args.(new_indexes_to_old_indexes.(new_index)))
      |> Array.to_list
    in
    let args =
      let flattened_reordered_shape =
        Mixed_block_shape.flattened_reordered_shape shape
      in
      List.mapi
        (fun new_index arg ->
          match flattened_reordered_shape.(new_index) with
          | Value _ | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
          | Vec128 | Vec256 | Vec512 | Word ->
            arg
          | Float_boxed _ -> unbox_float_constant arg)
        args
    in
    let kind_shape = K.Mixed_block_shape.from_mixed_block_shape shape in
    let acc, fields =
      List.fold_left_map
        (fun acc c ->
          let acc, field, _name = declare_const acc dbg c in
          acc, field)
        acc args
    in
    let const : SC.t =
      SC.block
        (Tag.Scannable.create_exn tag)
        Immutable (Mixed_record kind_shape) fields
    in
    register_const acc dbg const "const_mixed_block"
  | Const_null -> acc, reg_width RWC.const_null, "null"

let close_const acc const =
  (* For this code path, the debuginfo is discarded (see just below). *)
  let acc, simple_with_dbg, name = declare_const acc Debuginfo.none const in
  let named =
    Named.create_simple (Simple.With_debuginfo.simple simple_with_dbg)
  in
  acc, named, name

let find_simple_from_id_with_kind env id =
  match Env.find_simple_to_substitute_exn env id with
  | simple, kind -> simple, kind
  | exception Not_found -> (
    match Env.find_var_exn env id with
    | exception Not_found ->
      Misc.fatal_errorf
        "find_simple_from_id: Cannot find [Ident] %a in environment" Ident.print
        id
    | var, kind -> Simple.var var, kind)

let find_simple_from_id env id = fst (find_simple_from_id_with_kind env id)

(* CR mshinwell: Avoid the double lookup *)
let find_simple acc env (simple : IR.simple) =
  match simple with
  | Const const ->
    (* For this code path, the debuginfo isn't relevant. *)
    let acc, simple, _ = declare_const acc Debuginfo.none const in
    acc, Simple.With_debuginfo.simple simple
  | Var id -> acc, find_simple_from_id env id

let find_simples acc env ids =
  List.fold_left_map (fun acc id -> find_simple acc env id) acc ids

let find_value_approximation env simple =
  Simple.pattern_match' simple
    ~var:(fun var ~coercion:_ -> Env.find_var_approximation env var)
    ~symbol:(fun sym ~coercion:_ -> Value_approximation.Value_symbol sym)
    ~const:(fun const -> Value_approximation.Value_const const)

let find_value_approximation_through_symbol acc env simple =
  match find_value_approximation env simple with
  | Value_approximation.Value_symbol sym ->
    Acc.find_symbol_approximation acc sym
  | approx -> approx

module Inlining = struct
  include Closure_conversion_aux.Inlining

  (* CR keryan: we need to emit warnings *)
  let inlinable env apply callee_approx =
    let tracker = Env.inlining_history_tracker env in
    let are_rebuilding_terms = Are_rebuilding_terms.are_rebuilding in
    let compilation_unit =
      Env.inlining_history_tracker env
      |> Inlining_history.Tracker.absolute
      |> Inlining_history.Absolute.compilation_unit
    in
    match (callee_approx : Env.value_approximation option) with
    | None | Some Value_unknown ->
      Inlining_report.record_decision_at_call_site_for_unknown_function ~tracker
        ~apply ~pass:After_closure_conversion ();
      Not_inlinable
    | Some (Value_symbol _)
    | Some (Value_const _)
    | Some (Block_approximation _) ->
      assert false
    | Some (Closure_approximation { code; _ }) ->
      let metadata = Code_or_metadata.code_metadata code in
      let fun_params_length =
        Code_metadata.params_arity metadata |> Flambda_arity.num_params
      in
      if (not (Code_or_metadata.code_present code))
         || fun_params_length > List.length (Apply_expr.args apply)
      then (
        Inlining_report.record_decision_at_call_site_for_known_function ~tracker
          ~apply ~pass:After_closure_conversion ~unrolling_depth:None
          ~callee:(Inlining_history.Absolute.empty compilation_unit)
          ~are_rebuilding_terms Definition_says_not_to_inline;
        Not_inlinable)
      else
        let code = Code_or_metadata.get_code code in
        let inlined_call = Apply_expr.inlined apply in
        let decision, res =
          match inlined_call with
          | Never_inlined ->
            ( Call_site_inlining_decision_type.Never_inlined_attribute,
              Not_inlinable )
          | Always_inlined _ | Hint_inlined ->
            Call_site_inlining_decision_type.Attribute_always, Inlinable code
          | Default_inlined | Unroll _ ->
            (* Closure ignores completely [@unrolled] attributes, so it seems
               safe to do the same. *)
            ( Call_site_inlining_decision_type.Definition_says_inline
                { was_inline_always = false },
              Inlinable code )
        in
        Inlining_report.record_decision_at_call_site_for_known_function ~tracker
          ~apply ~pass:After_closure_conversion ~unrolling_depth:None
          ~callee:(Code.absolute_history code)
          ~are_rebuilding_terms decision;
        res

  let make_inlined_body acc ~callee ~called_code_id ~region_inlined_into ~params
      ~args ~my_closure ~my_region ~my_ghost_region ~my_depth ~body
      ~free_names_of_body ~exn_continuation ~return_continuation
      ~apply_exn_continuation ~apply_return_continuation ~apply_depth ~apply_dbg
      =
    let my_depth_duid = Flambda_debug_uid.none in
    let my_closure_duid = Flambda_debug_uid.none in
    let rec_info =
      match apply_depth with
      | None -> Rec_info_expr.initial
      | Some depth -> Rec_info_expr.var depth
    in
    let bind_params ~params ~args ~body:(acc, body) =
      let acc = Acc.with_free_names free_names_of_body acc in
      List.fold_left2
        (fun (acc, body) (param, param_duid) arg ->
          Let_with_acc.create acc
            (Bound_pattern.singleton
               (VB.create param param_duid Name_mode.normal))
            (Named.create_simple arg) ~body)
        (acc, body) params args
    in
    let bind_depth ~my_depth ~rec_info ~body:(acc, body) =
      Let_with_acc.create acc
        (Bound_pattern.singleton
           (VB.create my_depth my_depth_duid Name_mode.normal))
        (Named.create_rec_info rec_info)
        ~body
    in
    let apply_renaming (acc, body) renaming =
      let acc =
        Acc.with_free_names
          (Name_occurrences.apply_renaming (Acc.free_names acc) renaming)
          acc
      in
      acc, Expr.apply_renaming body renaming
    in
    let acc, body =
      Inlining_helpers.make_inlined_body ~callee ~called_code_id
        ~region_inlined_into ~params ~args
        ~my_closure:(my_closure, my_closure_duid)
        ~my_region ~my_ghost_region ~my_depth ~rec_info ~body:(acc, body)
        ~exn_continuation ~return_continuation ~apply_exn_continuation
        ~apply_return_continuation ~bind_params ~bind_depth ~apply_renaming
    in
    let inlined_debuginfo =
      Inlined_debuginfo.create ~called_code_id ~apply_dbg
    in
    let inlined_dbg_var = Variable.create "inlined_dbg" K.value in
    let inlined_dbg_var_duid = Flambda_debug_uid.none in
    Let_with_acc.create acc
      (Bound_pattern.singleton
         (VB.create inlined_dbg_var inlined_dbg_var_duid Name_mode.normal))
      (Named.create_prim
         (Nullary (Enter_inlined_apply { dbg = inlined_debuginfo }))
         Debuginfo.none)
      ~body

  let wrap_inlined_body_for_exn_extra_args acc ~extra_args
      ~apply_exn_continuation ~apply_return_continuation ~result_arity
      ~make_inlined_body =
    let apply_cont_create acc ~trap_action cont ~args ~dbg =
      let acc, apply_cont =
        Apply_cont_with_acc.create acc ~trap_action cont ~args ~dbg
      in
      Expr_with_acc.create_apply_cont acc apply_cont
    in
    let let_cont_create acc cont ~handler_params ~handler ~body ~is_exn_handler
        ~is_cold =
      Let_cont_with_acc.build_non_recursive acc cont ~handler_params ~handler
        ~body ~is_exn_handler ~is_cold
    in
    Inlining_helpers.wrap_inlined_body_for_exn_extra_args acc ~extra_args
      ~apply_exn_continuation ~apply_return_continuation ~result_arity
      ~make_inlined_body ~apply_cont_create ~let_cont_create

  let inline acc ~apply ~apply_depth ~func_desc:code =
    let apply_dbg = Apply.dbg apply in
    let callee = Apply.callee apply in
    let region_inlined_into =
      match Apply.call_kind apply with
      | Function { alloc_mode; _ } -> alloc_mode
      | Method _ | C_call _ | Effect _ ->
        Misc.fatal_error
          "Trying to call [Closure_conversion.Inlining.inline] on a non-OCaml \
           function call."
    in
    let args = Apply.args apply in
    let apply_return_continuation = Apply.continuation apply in
    let apply_exn_continuation = Apply.exn_continuation apply in
    let params_and_body = Code.params_and_body code in
    let cost_metrics = Code.cost_metrics code in
    Function_params_and_body.pattern_match params_and_body
      ~f:(fun
           ~return_continuation
           ~exn_continuation
           params
           ~body
           ~my_closure
           ~is_my_closure_used:_
           ~my_region
           ~my_ghost_region
           ~my_depth
           ~free_names_of_body
         ->
        let free_names_of_body =
          match free_names_of_body with
          | Unknown ->
            Misc.fatal_error
              "Params_and_body needs free_names_of_body in [Closure_conversion]"
          | Known free_names -> free_names
        in
        let make_inlined_body =
          make_inlined_body ~callee ~called_code_id:(Code.code_id code)
            ~region_inlined_into
            ~params:(Bound_parameters.vars_and_uids params)
            ~args ~my_closure ~my_region ~my_ghost_region ~my_depth ~body
            ~free_names_of_body ~exn_continuation ~return_continuation
            ~apply_depth ~apply_dbg
        in
        let acc = Acc.with_free_names Name_occurrences.empty acc in
        let acc = Acc.increment_metrics cost_metrics acc in
        match Exn_continuation.extra_args apply_exn_continuation with
        | [] ->
          make_inlined_body acc
            ~apply_exn_continuation:
              (Exn_continuation.exn_handler apply_exn_continuation)
            ~apply_return_continuation
        | extra_args ->
          wrap_inlined_body_for_exn_extra_args acc ~extra_args
            ~apply_exn_continuation ~apply_return_continuation
            ~result_arity:(Code.result_arity code) ~make_inlined_body)
end

type unarized_extern_repr =
  { kind : K.t;
    arg_transformer : P.unary_primitive option;
    return_transformer : P.unary_primitive option
  }

(* The following two functions form part of what fixes the calling convention
   for unboxed products in externals (the remainder being the register
   assignment in the backend). *)

let rec unarize_const_sort_for_extern_repr (sort : Jkind.Sort.Const.t) =
  match sort with
  | Base base -> (
    match base with
    | Void -> []
    | Value ->
      [{ kind = K.value; arg_transformer = None; return_transformer = None }]
    | Float64 ->
      [ { kind = K.naked_float;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Float32 ->
      [ { kind = K.naked_float32;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Word ->
      [ { kind = K.naked_nativeint;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Bits8 ->
      [ { kind = K.naked_int8;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Bits16 ->
      [ { kind = K.naked_int16;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Bits32 ->
      [ { kind = K.naked_int32;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Bits64 ->
      [ { kind = K.naked_int64;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Vec128 ->
      [ { kind = K.naked_vec128;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Vec256 ->
      [ { kind = K.naked_vec256;
          arg_transformer = None;
          return_transformer = None
        } ]
    | Vec512 ->
      [ { kind = K.naked_vec512;
          arg_transformer = None;
          return_transformer = None
        } ])
  | Product sorts -> List.concat_map unarize_const_sort_for_extern_repr sorts

let unarize_extern_repr alloc_mode (extern_repr : Lambda.extern_repr) =
  match extern_repr with
  | Same_as_ocaml_repr (Base _ as sort) ->
    let kind =
      Typeopt.layout_of_non_void_sort sort
      |> K.With_subkind.from_lambda_values_and_unboxed_numbers_only
      |> K.With_subkind.kind
    in
    [{ kind; arg_transformer = None; return_transformer = None }]
  | Same_as_ocaml_repr (Product sorts) ->
    List.concat_map unarize_const_sort_for_extern_repr sorts
  | Unboxed_float Boxed_float64 ->
    [ { kind = K.naked_float;
        arg_transformer = Some (P.Unbox_number Naked_float);
        return_transformer = Some (P.Box_number (Naked_float, alloc_mode))
      } ]
  | Unboxed_float Boxed_float32 ->
    [ { kind = K.naked_float32;
        arg_transformer = Some (P.Unbox_number Naked_float32);
        return_transformer = Some (P.Box_number (Naked_float32, alloc_mode))
      } ]
  | Unboxed_integer Unboxed_int8 ->
    [ { kind = K.naked_int8;
        arg_transformer =
          Some (P.Num_conv { src = Tagged_immediate; dst = Naked_int8 });
        return_transformer =
          Some (P.Num_conv { src = Naked_int8; dst = Tagged_immediate })
      } ]
  | Unboxed_integer Unboxed_int16 ->
    [ { kind = K.naked_int16;
        arg_transformer =
          Some (P.Num_conv { src = Tagged_immediate; dst = Naked_int16 });
        return_transformer =
          Some (P.Num_conv { src = Naked_int16; dst = Tagged_immediate })
      } ]
  | Unboxed_integer Unboxed_nativeint ->
    [ { kind = K.naked_nativeint;
        arg_transformer = Some (P.Unbox_number Naked_nativeint);
        return_transformer = Some (P.Box_number (Naked_nativeint, alloc_mode))
      } ]
  | Unboxed_integer Unboxed_int32 ->
    [ { kind = K.naked_int32;
        arg_transformer = Some (P.Unbox_number Naked_int32);
        return_transformer = Some (P.Box_number (Naked_int32, alloc_mode))
      } ]
  | Unboxed_integer Unboxed_int64 ->
    [ { kind = K.naked_int64;
        arg_transformer = Some (P.Unbox_number Naked_int64);
        return_transformer = Some (P.Box_number (Naked_int64, alloc_mode))
      } ]
  | Unboxed_vector Boxed_vec128 ->
    [ { kind = K.naked_vec128;
        arg_transformer = Some (P.Unbox_number Naked_vec128);
        return_transformer = Some (P.Box_number (Naked_vec128, alloc_mode))
      } ]
  | Unboxed_vector Boxed_vec256 ->
    [ { kind = K.naked_vec256;
        arg_transformer = Some (P.Unbox_number Naked_vec256);
        return_transformer = Some (P.Box_number (Naked_vec256, alloc_mode))
      } ]
  | Unboxed_vector Boxed_vec512 ->
    [ { kind = K.naked_vec512;
        arg_transformer = Some (P.Unbox_number Naked_vec512);
        return_transformer = Some (P.Box_number (Naked_vec512, alloc_mode))
      } ]
  | Untagged_int ->
    [ { kind = K.naked_immediate;
        arg_transformer = Some P.Untag_immediate;
        return_transformer = Some P.Tag_immediate
      } ]

let close_c_call acc env ~loc ~let_bound_ids_with_kinds
    (({ prim_name;
        prim_arity;
        prim_alloc;
        prim_c_builtin;
        prim_effects;
        prim_coeffects;
        prim_native_name;
        prim_native_repr_args;
        prim_native_repr_res;
        prim_is_layout_poly
      } :
       Lambda.external_call_description) as prim_desc)
    ~(args : Simple.t list list) exn_continuation dbg
    ~(current_region : Variable.t option) ~current_ghost_region
    (k : Acc.t -> Named.t list -> Expr_with_acc.t) : Expr_with_acc.t =
  if prim_is_layout_poly
  then
    Misc.fatal_errorf
      "close_c_call: C call primitive %s can't be layout polymorphic." prim_name;
  let env, let_bound_vars =
    List.fold_left_map
      (fun env (id, id_duid, kind) ->
        let env, let_bound_var =
          Env.add_var_like env id Not_user_visible kind
        in
        env, (let_bound_var, id_duid))
      env let_bound_ids_with_kinds
  in
  let cost_metrics_of_body, free_names_of_body, acc, body =
    Acc.measure_cost_metrics acc ~f:(fun acc ->
        k acc (List.map (fun (v, _) -> Named.create_var v) let_bound_vars))
  in
  let alloc_mode_app =
    match Lambda.locality_mode_of_primitive_description prim_desc with
    | None ->
      (* This happens when stack allocation is disabled. *)
      Alloc_mode.For_applications.heap
    | Some alloc_mode ->
      Alloc_mode.For_applications.from_lambda alloc_mode ~current_region
        ~current_ghost_region
  in
  let alloc_mode =
    match Lambda.locality_mode_of_primitive_description prim_desc with
    | None ->
      (* This happens when stack allocation is disabled. *)
      Alloc_mode.For_allocations.heap
    | Some alloc_mode ->
      Alloc_mode.For_allocations.from_lambda alloc_mode ~current_region
  in
  let unarized_params =
    List.concat_map
      (unarize_extern_repr alloc_mode)
      (List.map snd prim_native_repr_args)
  in
  let unarized_results =
    unarize_extern_repr alloc_mode (snd prim_native_repr_res)
  in
  if List.compare_lengths unarized_params args <> 0
  then
    Misc.fatal_errorf
      "Mismatch between unarized [prim_native_repr_args] (length %d) and \
       argument list (length %d):@ %a"
      (List.length unarized_params)
      (List.length args) Debuginfo.print_compact dbg;
  if List.compare_lengths unarized_results let_bound_vars <> 0
  then
    Misc.fatal_errorf
      "Mismatch between unarized [prim_native_repr_res] (length %d) and result \
       var list (length %d):@ %a"
      (List.length unarized_results)
      (List.length let_bound_vars)
      Debuginfo.print_compact dbg;
  let need_return_transformer =
    List.exists
      (fun { return_transformer; _ } -> Option.is_some return_transformer)
      unarized_results
  in
  let return_continuation, needs_wrapper =
    match Expr.descr body with
    | Apply_cont apply_cont
      when Simple.List.equal
             (Apply_cont_expr.args apply_cont)
             (Simple.vars (List.map fst let_bound_vars))
           && Option.is_none (Apply_cont_expr.trap_action apply_cont)
           && not need_return_transformer ->
      Apply_cont_expr.continuation apply_cont, false
    | _ -> Continuation.create (), true
  in
  (* Unlike for OCaml function calls, we don't preserve unboxed product arity
     information here, as there is no partial application etc. *)
  let[@inline] build_arity unarized =
    List.map (fun { kind; _ } -> K.With_subkind.anything kind) unarized
    |> Flambda_arity.create_singletons
  in
  let param_arity = build_arity unarized_params in
  let return_arity = build_arity unarized_results in
  let effects = Effects.from_lambda prim_effects in
  let coeffects = Coeffects.from_lambda prim_coeffects in
  let call_kind =
    Call_kind.c_call ~needs_caml_c_call:prim_alloc ~is_c_builtin:prim_c_builtin
      ~effects ~coeffects alloc_mode_app
  in
  let call_symbol =
    let prim_name =
      if String.equal prim_native_name "" then prim_name else prim_native_name
    in
    Symbol.create
      (Symbol.external_symbols_compilation_unit ())
      (Linkage_name.of_string prim_name)
  in
  let call args acc =
    (* Some C primitives have implementations within Flambda itself. *)
    let[@inline] unboxed_int64_to_and_from_unboxed_float ~src_kind ~dst_kind ~op
        =
      if prim_arity <> 1
      then Misc.fatal_errorf "Expected arity one for %s" prim_native_name
      else
        match prim_native_repr_args, prim_native_repr_res with
        | [(_, src)], (_, dst)
          when Stdlib.( = ) src src_kind && Stdlib.( = ) dst dst_kind -> (
          match args with
          | [arg] ->
            let prim = P.Unary (Reinterpret_64_bit_word op, arg) in
            let result =
              Variable.create "reinterpreted" (P.result_kind' prim)
            in
            let result_duid = Flambda_debug_uid.none in
            let result' =
              Bound_var.create result result_duid Name_mode.normal
            in
            let bindable = Bound_pattern.singleton result' in
            let acc, return_result =
              Apply_cont_with_acc.create acc return_continuation
                ~args:[Simple.var result]
                ~dbg
            in
            let acc, return_result_expr =
              Expr_with_acc.create_apply_cont acc return_result
            in
            Let_with_acc.create acc bindable
              (Named.create_prim prim dbg)
              ~body:return_result_expr
          | [] | _ :: _ ->
            Misc.fatal_errorf "Expected one arg for %s" prim_native_name)
        | _, _ ->
          Misc.fatal_errorf "Wrong argument and/or result kind(s) for %s"
            prim_native_name
    in
    match prim_native_name with
    | "caml_int64_float_of_bits_unboxed" ->
      unboxed_int64_to_and_from_unboxed_float
        ~src_kind:(Unboxed_integer Unboxed_int64)
        ~dst_kind:(Unboxed_float Boxed_float64)
        ~op:Unboxed_int64_as_unboxed_float64
    | "caml_int64_bits_of_float_unboxed" ->
      unboxed_int64_to_and_from_unboxed_float
        ~src_kind:(Unboxed_float Boxed_float64)
        ~dst_kind:(Unboxed_integer Unboxed_int64)
        ~op:Unboxed_float64_as_unboxed_int64
    | _ ->
      let callee = Simple.symbol call_symbol in
      let apply =
        Apply.create ~callee:(Some callee)
          ~continuation:(Return return_continuation) exn_continuation ~args
          ~args_arity:param_arity ~return_arity ~call_kind dbg
          ~inlined:Default_inlined
          ~inlining_state:(Inlining_state.default ~round:0)
          ~probe:None ~position:Normal
          ~relative_history:(Env.relative_history_from_scoped ~loc env)
      in
      Expr_with_acc.create_apply acc apply
  in
  let call : Acc.t -> Expr_with_acc.t =
    List.fold_left2
      (fun (call : Simple.t list -> Acc.t -> Expr_with_acc.t) arg param ->
        match param.arg_transformer with
        | None -> fun args acc -> call (arg :: args) acc
        | Some named ->
          fun args acc ->
            let prim = P.Unary (named, arg) in
            let unboxed_arg = Variable.create "unboxed" (P.result_kind' prim) in
            let unboxed_arg_duid = Flambda_debug_uid.none in
            let unboxed_arg' =
              VB.create unboxed_arg unboxed_arg_duid Name_mode.normal
            in
            let acc, body = call (Simple.var unboxed_arg :: args) acc in
            let named = Named.create_prim prim dbg in
            Let_with_acc.create acc
              (Bound_pattern.singleton unboxed_arg')
              named ~body)
      call (List.flatten args) unarized_params []
  in
  let wrap_c_call acc ~handler_params ~code_after_call c_call =
    let params =
      List.map2
        (fun (ret_value, ret_value_duid) { kind; _ } ->
          BP.create ret_value (K.With_subkind.anything kind) ret_value_duid)
        handler_params unarized_results
      |> Bound_parameters.create
    in
    Let_cont_with_acc.build_non_recursive acc return_continuation
      ~handler_params:params ~handler:code_after_call ~body:c_call
      ~is_exn_handler:false ~is_cold:false
  in
  let keep_body acc =
    ( Acc.with_cost_metrics
        (Cost_metrics.( + ) (Acc.cost_metrics acc) cost_metrics_of_body)
        (Acc.with_free_names free_names_of_body acc),
      body )
  in
  let box_unboxed_returns () =
    let let_bound_vars' =
      List.map
        (fun (let_bound_var, let_bound_var_duid) ->
          VB.create let_bound_var let_bound_var_duid Name_mode.normal)
        let_bound_vars
    in
    let handler_params =
      List.map
        (fun (let_bound_var, let_bound_var_duid) ->
          Variable.rename let_bound_var, let_bound_var_duid)
        let_bound_vars
    in
    let body acc =
      let acc, body = keep_body acc in
      List.fold_left2
        (fun (acc, body) unarized_param
             ((handler_param, _handler_param_duid), let_bound_var') ->
          let named =
            let handler_param = Simple.var handler_param in
            match unarized_param.return_transformer with
            | None -> Named.create_simple handler_param
            | Some return_transformer ->
              Named.create_prim (Unary (return_transformer, handler_param)) dbg
          in
          Let_with_acc.create acc
            (Bound_pattern.singleton let_bound_var')
            named ~body)
        (acc, body) unarized_results
        (List.combine handler_params let_bound_vars')
    in
    body, handler_params
  in
  if not need_return_transformer
  then
    if needs_wrapper
    then
      wrap_c_call acc ~handler_params:let_bound_vars ~code_after_call:keep_body
        call
    else
      (* Here the body is discarded. It might be useful to explicitly remove
         anything that has been added to the acc while converting the body.
         However, as we are hitting this code only when body is a goto
         continuation where the only parameter is [let_bound_var] this operation
         would be a noop and we can skip it. *)
      call acc
  else
    let code_after_call, handler_params = box_unboxed_returns () in
    wrap_c_call acc ~handler_params ~code_after_call call

let close_exn_continuation acc env (exn_continuation : IR.exn_continuation) =
  let acc, extra_args =
    List.fold_left_map
      (fun acc (simple, _uid, kind) ->
        let acc, simple = find_simple acc env simple in
        acc, (simple, kind))
      acc exn_continuation.extra_args
  in
  ( acc,
    Exn_continuation.create ~exn_handler:exn_continuation.exn_handler
      ~extra_args )

let close_raise0 acc env ~raise_kind ~arg ~dbg exn_continuation =
  let acc, exn_cont = close_exn_continuation acc env exn_continuation in
  let exn_handler = Exn_continuation.exn_handler exn_cont in
  let args =
    (* CR mshinwell: Share with [Lambda_to_flambda_primitives_helpers] *)
    let extra_args =
      List.map
        (fun (simple, _kind) -> simple)
        (Exn_continuation.extra_args exn_cont)
    in
    arg :: extra_args
  in
  let raise_kind = Some (Trap_action.Raise_kind.from_lambda raise_kind) in
  let trap_action = Trap_action.Pop { exn_handler; raise_kind } in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ~trap_action exn_handler ~args ~dbg
  in
  (* Since raising of an exception doesn't terminate, we don't call [k]. *)
  Expr_with_acc.create_apply_cont acc apply_cont

let close_raise acc env ~raise_kind ~arg ~dbg exn_continuation =
  let acc, arg = find_simple acc env arg in
  close_raise0 acc env ~raise_kind ~arg ~dbg exn_continuation

let close_effect_primitive acc env ~dbg exn_continuation
    (prim : Lambda.primitive) ~args ~let_bound_ids_with_kinds
    (k : Acc.t -> Named.t list -> Expr_with_acc.t) : Expr_with_acc.t =
  if not Config.runtime5
  then Misc.fatal_error "Effect primitives are only supported on runtime5";
  (* CR mshinwell: share with close_c_call, above *)
  let _env, let_bound_vars =
    List.fold_left_map
      (fun env (id, _id_duid, kind) ->
        let env, let_bound_var =
          Env.add_var_like env id Not_user_visible kind
        in
        env, (let_bound_var, Flambda_debug_uid.none))
        (* We are dropping the debug uid here, because the variable is not user
           visible. *)
      env let_bound_ids_with_kinds
  in
  let let_bound_var, let_bound_var_duid =
    match let_bound_vars with
    | [(let_bound_var, let_bound_var_duid)] -> let_bound_var, let_bound_var_duid
    | [] | _ :: _ :: _ ->
      Misc.fatal_errorf
        "close_effect_primitive: expected singleton return for primitive %a, \
         but got: [%a]"
        Printlambda.primitive prim
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Variable.print)
        (List.map fst let_bound_vars)
  in
  let continuation = Continuation.create () in
  let return_kind = Flambda_kind.With_subkind.any_value in
  let params =
    [BP.create let_bound_var return_kind let_bound_var_duid]
    |> Bound_parameters.create
  in
  let close call_kind =
    let apply acc =
      Apply_expr.create ~callee:None ~continuation:(Return continuation)
        exn_continuation ~args:[] ~args_arity:Flambda_arity.nullary
        ~return_arity:
          (Flambda_arity.create_singletons
             [Flambda_kind.With_subkind.any_value])
        ~call_kind dbg ~inlined:Never_inlined
        ~inlining_state:(Inlining_state.default ~round:0)
        ~probe:None ~position:Normal
        ~relative_history:Inlining_history.Relative.empty
      |> Expr_with_acc.create_apply acc
    in
    Let_cont_with_acc.build_non_recursive acc continuation
      ~handler_params:params
      ~handler:(fun acc ->
        let cost_metrics_of_body, free_names_of_body, acc, code_after_call =
          Acc.measure_cost_metrics acc ~f:(fun acc ->
              k acc (List.map (fun (v, _) -> Named.create_var v) let_bound_vars))
        in
        let acc =
          Acc.with_cost_metrics
            (Cost_metrics.( + ) (Acc.cost_metrics acc) cost_metrics_of_body)
            (Acc.with_free_names free_names_of_body acc)
        in
        acc, code_after_call)
      ~body:apply ~is_exn_handler:false ~is_cold:false
  in
  let module C = Call_kind in
  let module E = C.Effect in
  match[@ocaml.warning "-fragile-match"] prim, args with
  | Pperform, [[eff]] ->
    let call_kind = C.effect (E.perform ~eff) in
    close call_kind
  | Prunstack, [[stack]; [f]; [arg]] ->
    let call_kind = C.effect (E.run_stack ~stack ~f ~arg) in
    close call_kind
  | Presume, [[stack]; [f]; [arg]; [last_fiber]] ->
    let call_kind = C.effect (E.resume ~stack ~f ~arg ~last_fiber) in
    close call_kind
  | Preperform, [[eff]; [cont]; [last_fiber]] ->
    let call_kind = C.effect (E.reperform ~eff ~cont ~last_fiber) in
    close call_kind
  | _ ->
    Misc.fatal_errorf
      "close_effect_primitive: Wrong primitive and/or number of arguments: %a \
       (%d args)"
      Printlambda.primitive prim (List.length args)

let close_primitive acc env ~let_bound_ids_with_kinds named
    (prim : Lambda.primitive) ~args loc
    (exn_continuation : IR.exn_continuation option) ~current_region
    ~current_ghost_region (k : Acc.t -> Named.t list -> Expr_with_acc.t) :
    Expr_with_acc.t =
  let orig_exn_continuation = exn_continuation in
  let acc, exn_continuation =
    match exn_continuation with
    | None -> acc, None
    | Some exn_continuation ->
      let acc, cont = close_exn_continuation acc env exn_continuation in
      acc, Some cont
  in
  let acc, args =
    List.fold_left_map (fun acc arg -> find_simples acc env arg) acc args
  in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Pccall is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_c_call acc env ~loc ~let_bound_ids_with_kinds prim ~args
      exn_continuation dbg ~current_region ~current_ghost_region k
  | Pgetglobal cu, [] ->
    if Compilation_unit.equal cu (Env.current_unit env)
    then
      Misc.fatal_errorf "Pgetglobal %a in the same unit" Compilation_unit.print
        cu;
    let symbol =
      Flambda2_import.Symbol.for_compilation_unit cu |> Symbol.create_wrapped
    in
    let named = Named.create_simple (Simple.symbol symbol) in
    k acc [named]
  | Pgetpredef id, [] ->
    let symbol =
      Flambda2_import.Symbol.for_predef_ident id |> Symbol.create_wrapped
    in
    let named = Named.create_simple (Simple.symbol symbol) in
    k acc [named]
  | Praise raise_kind, [[arg]] ->
    let exn_continuation =
      match orig_exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_raise0 acc env ~raise_kind ~arg ~dbg exn_continuation
  | ( ( Pmakeblock _ | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakearray _
      | Pmakemixedblock _ ),
      [] ) ->
    (* Special case for liftable empty block or array *)
    let acc, sym =
      match prim with
      | Pmakeblock (tag, _, _, _mode) ->
        if tag <> 0
        then
          (* There should not be any way to reach this from Ocaml code. *)
          Misc.fatal_error
            "Non-zero tag on empty block allocation in [Closure_conversion]"
        else
          register_const0 acc
            (Static_const.block Tag.Scannable.zero Immutable Value_only [])
            "empty_block"
      | Pmakefloatblock _ ->
        Misc.fatal_error "Unexpected empty float block in [Closure_conversion]"
      | Pmakeufloatblock _ ->
        Misc.fatal_error "Unexpected empty float# block in [Closure_conversion]"
      | Pmakemixedblock _ ->
        Misc.fatal_error "Unexpected empty mixed block in [Closure_conversion]"
      | Pmakearray (array_kind, _, _mode) ->
        let array_kind = Empty_array_kind.of_lambda array_kind in
        register_const0 acc (Static_const.empty_array array_kind) "empty_array"
      | Parrayblit _array_set_kind ->
        Misc.fatal_error "Closure_conversion.close_primitive: unimplemented"
      | Pmakearray_dynamic _ | Pbytes_to_string | Pbytes_of_string
      | Parray_of_iarray | Parray_to_iarray | Pignore | Pgetglobal _
      | Psetglobal _ | Pgetpredef _ | Pfield _ | Pfield_computed _ | Psetfield _
      | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
      | Pccall _ | Praise _ | Pufloatfield _ | Psetufloatfield _ | Psequand
      | Psequor | Pnot | Pnegint | Pmixedfield _ | Psetmixedfield _ | Paddint
      | Psubint | Pmulint | Pdivint _ | Pmodint _ | Pandint | Porint | Pxorint
      | Plslint | Plsrint | Pasrint | Pintcomp _ | Pcompare_ints
      | Pcompare_floats _ | Pcompare_bints _ | Poffsetint _ | Poffsetref _
      | Pintoffloat _
      | Pfloatofint (_, _)
      | Pfloatoffloat32 _ | Pfloat32offloat _
      | Pnegfloat (_, _)
      | Pabsfloat (_, _)
      | Paddfloat (_, _)
      | Psubfloat (_, _)
      | Pmulfloat (_, _)
      | Pdivfloat (_, _)
      | Pfloatcomp (_, _)
      | Punboxed_float_comp (_, _)
      | Pstringlength | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu
      | Pbytessetu | Pbytesrefs | Pbytessets | Pduparray _ | Parraylength _
      | Parrayrefu _ | Parraysetu _ | Parrayrefs _ | Parraysets _ | Pisint _
      | Pisnull | Pisout | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _
      | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _
      | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _
      | Pasrbint _ | Pbintcomp _ | Punboxed_int_comp _ | Pbigarrayref _
      | Pbigarrayset _ | Pbigarraydim _ | Pstring_load_16 _ | Pstring_load_32 _
      | Pstring_load_f32 _ | Pstring_load_64 _ | Pstring_load_vec _
      | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_f32 _
      | Pbytes_load_64 _ | Pbytes_load_vec _ | Pbytes_set_16 _ | Pbytes_set_32 _
      | Pbytes_set_f32 _ | Pbytes_set_64 _ | Pbytes_set_vec _
      | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_f32 _
      | Pbigstring_load_64 _ | Pbigstring_load_vec _ | Pbigstring_set_16 _
      | Pbigstring_set_32 _ | Pbigstring_set_f32 _ | Pbigstring_set_64 _
      | Pbigstring_set_vec _ | Pfloatarray_load_vec _ | Pfloat_array_load_vec _
      | Pint_array_load_vec _ | Punboxed_float_array_load_vec _
      | Punboxed_float32_array_load_vec _ | Punboxed_int32_array_load_vec _
      | Punboxed_int64_array_load_vec _ | Punboxed_nativeint_array_load_vec _
      | Pfloatarray_set_vec _ | Pfloat_array_set_vec _ | Pint_array_set_vec _
      | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
      | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
      | Punboxed_nativeint_array_set_vec _ | Pctconst _ | Pbswap16 | Pbbswap _
      | Pint_as_pointer _ | Popaque _ | Pprobe_is_enabled _ | Pobj_dup
      | Pobj_magic _ | Punbox_float _
      | Pbox_float (_, _)
      | Punbox_vector _
      | Pbox_vector (_, _)
      | Pmakelazyblock _ | Puntag_int _ | Ptag_int _ | Punbox_int _ | Pbox_int _
      | Punbox_unit | Pmake_unboxed_product _ | Punboxed_product_field _
      | Parray_element_size_in_bytes _ | Pget_header _ | Prunstack | Pperform
      | Presume | Preperform | Patomic_exchange_field _
      | Patomic_compare_exchange_field _ | Patomic_compare_set_field _
      | Patomic_fetch_add_field | Patomic_add_field | Patomic_sub_field
      | Patomic_land_field | Patomic_lor_field | Patomic_lxor_field | Pdls_get
      | Ppoll | Patomic_load_field _ | Patomic_set_field _ | Pcpu_relax
      | Preinterpret_tagged_int63_as_unboxed_int64
      | Preinterpret_unboxed_int64_as_tagged_int63 | Ppeek _ | Ppoke _ ->
        (* Inconsistent with outer match *)
        assert false
    in
    k acc [Named.create_simple (Simple.symbol sym)]
  | (Pperform | Prunstack | Presume | Preperform), args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf
          "Effect primitive is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_effect_primitive acc env ~dbg exn_continuation prim ~args
      ~let_bound_ids_with_kinds k
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind acc exn_continuation
      ~big_endian:(Env.big_endian env) ~register_const0 prim ~args dbg
      ~current_region ~current_ghost_region k

let close_trap_action_opt trap_action =
  Option.map
    (fun (trap_action : IR.trap_action) : Trap_action.t ->
      match trap_action with
      | Push { exn_handler } -> Push { exn_handler }
      | Pop { exn_handler } -> Pop { exn_handler; raise_kind = None })
    trap_action

let close_named acc env ~let_bound_ids_with_kinds (named : IR.named)
    (k : Acc.t -> Named.t list -> Expr_with_acc.t) : Expr_with_acc.t =
  match named with
  | Simple (Var id) ->
    assert (not (Ident.is_global_or_predef id));
    let acc, simple = find_simple acc env (Var id) in
    let named = Named.create_simple simple in
    k acc [named]
  | Simple (Const cst) ->
    let acc, named, _name = close_const acc cst in
    k acc [named]
  | Get_tag var ->
    let named = find_simple_from_id env var in
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Unary (Tag_immediate, Prim (Unary (Get_tag, Simple named)))
    in
    Lambda_to_flambda_primitives_helpers.bind_recs acc None ~register_const0
      prim Debuginfo.none k
  | Begin_region { is_try_region; ghost; parent_region } ->
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      let arg : Lambda_to_flambda_primitives_helpers.simple_or_prim list =
        match parent_region with
        | None -> []
        | Some parent_region -> [Simple (find_simple_from_id env parent_region)]
      in
      Variadic
        ( (if is_try_region
          then Begin_try_region { ghost }
          else Begin_region { ghost }),
          arg )
    in
    Lambda_to_flambda_primitives_helpers.bind_recs acc None ~register_const0
      prim Debuginfo.none k
  | End_region { is_try_region; region; ghost } ->
    let named = find_simple_from_id env region in
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Unary
        ( (if is_try_region
          then End_try_region { ghost }
          else End_region { ghost }),
          Simple named )
    in
    Lambda_to_flambda_primitives_helpers.bind_recs acc None ~register_const0
      prim Debuginfo.none k
  | Prim { prim; args; loc; exn_continuation; region; ghost_region } ->
    let get_region_ident region =
      Option.map (fun region -> fst (Env.find_var env region)) region
    in
    close_primitive acc env ~let_bound_ids_with_kinds named prim ~args loc
      exn_continuation ~current_region:(get_region_ident region)
      ~current_ghost_region:(get_region_ident ghost_region)
      k

type simplified_block_load =
  | Unknown
  | Not_a_block
  | Block_but_cannot_simplify of Code_or_metadata.t Value_approximation.t
  | Field_contents of Simple.t

let simplify_block_load acc body_env ~block ~field : simplified_block_load =
  match find_value_approximation_through_symbol acc body_env block with
  | Value_unknown -> Unknown
  | Closure_approximation _ | Value_symbol _ | Value_const _ -> Not_a_block
  | Block_approximation (_tag, _shape, approx, _alloc_mode) -> (
    let approx =
      let i = Targetint_31_63.to_int field in
      if i >= Array.length approx then None else Some approx.(i)
    in
    match approx with
    | Some (Value_symbol sym) -> Field_contents (Simple.symbol sym)
    | Some (Value_const c) -> Field_contents (Simple.const c)
    | Some approx -> Block_but_cannot_simplify approx
    | None -> Not_a_block)

type block_static_kind =
  | Dynamic_block
  | Computed_static of Simple.With_debuginfo.t list
  | Constant of Simple.With_debuginfo.t list

let classify_fields_of_block env fields alloc_mode =
  let is_local =
    match (alloc_mode : Alloc_mode.For_allocations.t) with
    | Local _ -> true
    | Heap -> false
  in
  let static_fields =
    List.fold_left
      (fun static_fields f ->
        match static_fields with
        | None -> None
        | Some fields ->
          Simple.pattern_match'
            (Simple.With_debuginfo.simple f)
            ~const:(fun _cst -> Some (f :: fields))
            ~symbol:(fun _sym ~coercion:_ -> Some (f :: fields))
            ~var:(fun _var ~coercion:_ ->
              if Env.at_toplevel env
                 && Flambda_features.classic_mode ()
                 && not is_local
              then Some (f :: fields)
              else None))
      (Some []) fields
    |> Option.map List.rev
  in
  match static_fields with
  | None -> Dynamic_block
  | Some fields ->
    if List.exists
         (fun simple_with_dbg ->
           Simple.is_var (Simple.With_debuginfo.simple simple_with_dbg))
         fields
    then Computed_static fields
    else Constant fields

let close_let acc env let_bound_ids_with_kinds user_visible defining_expr
    ~(body : Acc.t -> Env.t -> Expr_with_acc.t) : Expr_with_acc.t =
  let rec cont ids_with_kinds env acc (defining_exprs : Named.t list) =
    match ids_with_kinds, defining_exprs with
    | [], [] -> body acc env
    | (id, uid, kind) :: ids_with_kinds, defining_expr :: defining_exprs -> (
      let body_env, var = Env.add_var_like env id user_visible kind in
      let body acc env = cont ids_with_kinds env acc defining_exprs in
      match defining_expr with
      | Simple simple ->
        let body_env = Env.add_simple_to_substitute env id simple kind in
        body acc body_env
      | Prim ((Variadic (Begin_region _, _) | Unary (End_region _, _)), _)
        when not (Flambda_features.stack_allocation_enabled ()) ->
        (* We use [body_env] to ensure the region variables are still in the
           environment, to avoid lookup errors, even though the [Let] won't be
           generated. *)
        body acc body_env
      | _ -> (
        (match defining_expr with
        | Prim (prim, _) ->
          let kind = Flambda_kind.With_subkind.kind kind in
          let result_kind =
            match Flambda_primitive.result_kind prim with
            | Unit -> Flambda_kind.value
            | Singleton result_kind -> result_kind
          in
          (* This kind check is always ok since it happens prior to any beta
             reduction. *)
          if not (Flambda_kind.equal kind result_kind)
          then
            Misc.fatal_errorf
              "Incompatible kinds when binding %a: this variable has kind %a, \
               but is bound to the result of %a which has kind %a@."
              Variable.print var Flambda_kind.print kind Flambda_primitive.print
              prim Flambda_kind.print result_kind
        | Simple _ | Static_consts _ | Set_of_closures _ | Rec_info _ -> ());
        let bound_pattern =
          Bound_pattern.singleton (VB.create var uid Name_mode.normal)
        in
        let bind acc env =
          (* CR pchambart: Not tail ! The body function is the recursion *)
          let acc, body = body acc env in
          Let_with_acc.create acc bound_pattern defining_expr ~body
        in
        match defining_expr with
        | Prim
            ( Variadic (Make_block (block_kind, Immutable, alloc_mode), fields),
              dbg ) -> (
          (* CR mshinwell: split into a separate function *)
          let tag, block_shape = P.Block_kind.to_shape block_kind in
          let approxs =
            List.map (find_value_approximation body_env) fields |> Array.of_list
          in
          let fields_kind =
            classify_fields_of_block env
              (List.map
                 (fun field -> Simple.With_debuginfo.create field dbg)
                 fields)
              alloc_mode
          in
          match fields_kind with
          | Constant static_fields | Computed_static static_fields -> (
            let approx, static_const =
              match block_shape with
              | Scannable scannable_block_shape -> (
                match Tag.Scannable.of_tag tag with
                | Some tag ->
                  let static_const =
                    Static_const.block tag Immutable scannable_block_shape
                      static_fields
                  in
                  let approx =
                    Value_approximation.Block_approximation
                      ( tag,
                        scannable_block_shape,
                        approxs,
                        Alloc_mode.For_allocations.as_type alloc_mode )
                  in
                  approx, static_const
                | None ->
                  Misc.fatal_errorf
                    "Binding of %a to %a has yielded a tag %a which is not \
                     scannable, yet the block shape appears to be: %a"
                    Ident.print id Named.print defining_expr Tag.print tag
                    Flambda_kind.Block_shape.print block_shape)
              | Float_record ->
                let static_fields =
                  List.map
                    (fun simple_with_dbg ->
                      let dbg = Simple.With_debuginfo.dbg simple_with_dbg in
                      Simple.pattern_match'
                        (Simple.With_debuginfo.simple simple_with_dbg)
                        ~var:(fun var ~coercion:_ -> Or_variable.Var (var, dbg))
                        ~symbol:(fun _sym ~coercion:_ ->
                          Misc.fatal_errorf
                            "Binding of %a to %a contains a symbol inside a \
                             float record, whereas only naked floats are \
                             permitted"
                            Ident.print id Named.print defining_expr)
                        ~const:(fun cst ->
                          match Reg_width_const.descr cst with
                          | Naked_float f -> Or_variable.Const f
                          | Tagged_immediate _ | Naked_immediate _
                          | Naked_float32 _ | Naked_int8 _ | Naked_int16 _
                          | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
                          | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _
                          | Null ->
                            Misc.fatal_errorf
                              "Binding of %a to %a contains the constant %a \
                               inside a float record, whereas only naked \
                               floats are permitted"
                              Ident.print id Named.print defining_expr
                              Reg_width_const.print cst))
                    static_fields
                in
                let static_const =
                  Static_const.immutable_float_block static_fields
                in
                (* Note: no approximations are currently provided for these. *)
                let approx = Value_approximation.Value_unknown in
                approx, static_const
            in
            match fields_kind with
            | Constant _ ->
              let acc, sym = register_const0 acc static_const (Ident.name id) in
              let body_env =
                Env.add_simple_to_substitute body_env id (Simple.symbol sym)
                  kind
              in
              let acc = Acc.add_symbol_approximation acc sym approx in
              body acc body_env
            | Computed_static _ ->
              (* This is a inconstant statically-allocated value, so cannot go
                 through [register_const0]. The definition must be placed right
                 away. *)
              let acc, symbol =
                manufacture_symbol acc (Variable.unique_name var)
              in
              let static_consts =
                [Static_const_or_code.create_static_const static_const]
              in
              let defining_expr =
                Static_const_group.create static_consts
                |> Named.create_static_consts
              in
              let body_env =
                Env.add_simple_to_substitute body_env id (Simple.symbol symbol)
                  kind
              in
              let acc = Acc.add_symbol_approximation acc symbol approx in
              let acc, body = body acc body_env in
              Let_with_acc.create acc
                (Bound_pattern.static
                   (Bound_static.create
                      [Bound_static.Pattern.block_like symbol]))
                defining_expr ~body
            | Dynamic_block -> (* Handled in outer match *) assert false)
          | Dynamic_block ->
            let body_env =
              match block_shape with
              | Scannable scannable_block_shape -> (
                match Tag.Scannable.of_tag tag with
                | Some tag ->
                  Env.add_block_approximation body_env var tag
                    scannable_block_shape approxs
                    (Alloc_mode.For_allocations.as_type alloc_mode)
                | None ->
                  Misc.fatal_errorf
                    "Binding of %a to %a (dynamic block) has yielded a tag %a \
                     which is not scannable, yet the block shape appears to \
                     be: %a"
                    Ident.print id Named.print defining_expr Tag.print tag
                    Flambda_kind.Block_shape.print block_shape)
              | Float_record ->
                (* No approximations for float records at the moment. *)
                body_env
            in
            bind acc body_env)
        | Prim
            ( Variadic
                ( Make_block (Values (tag, _), Immutable_unique, _alloc_mode),
                  [exn_name; exn_id] ),
              dbg )
          when Tag.Scannable.equal tag Tag.Scannable.object_tag
               && Env.at_toplevel env
               && Flambda_features.classic_mode () ->
          (* Special case to lift toplevel exception declarations *)
          let acc, symbol = manufacture_symbol acc (Variable.unique_name var) in
          let transform_arg arg = Simple.With_debuginfo.create arg dbg in
          (* This is an inconstant statically-allocated value, so cannot go
             through [register_const0]. The definition must be placed right
             away. *)
          let static_const =
            Static_const.block Tag.Scannable.object_tag Immutable_unique
              Value_only
              [transform_arg exn_name; transform_arg exn_id]
          in
          let static_consts =
            [Static_const_or_code.create_static_const static_const]
          in
          let defining_expr =
            Static_const_group.create static_consts
            |> Named.create_static_consts
          in
          let body_env =
            Env.add_simple_to_substitute body_env id (Simple.symbol symbol) kind
          in
          let acc =
            Acc.add_symbol_approximation acc symbol
              Value_approximation.Value_unknown
          in
          let acc, body = body acc body_env in
          Let_with_acc.create acc
            (Bound_pattern.static
               (Bound_static.create [Bound_static.Pattern.block_like symbol]))
            defining_expr ~body
        | Prim (Unary (Block_load { field; _ }, block), _) -> (
          match simplify_block_load acc body_env ~block ~field with
          | Unknown -> bind acc body_env
          | Not_a_block ->
            if Flambda_features.kind_checks ()
            then
              (* CR keryan: This is hidden behind kind checks because it can
                 appear on correct code using Lazy or GADT. It might warrant a
                 proper warning at some point. *)
              Misc.fatal_errorf
                "Unexpected approximation found when block approximation was \
                 expected in [Closure_conversion]: %a"
                Named.print defining_expr
            else
              ( acc,
                Expr.create_invalid
                  (Defining_expr_of_let (bound_pattern, defining_expr)) )
          | Field_contents sim ->
            let body_env = Env.add_simple_to_substitute env id sim kind in
            body acc body_env
          | Block_but_cannot_simplify approx ->
            let body_env = Env.add_var_approximation body_env var approx in
            bind acc body_env)
        | _ -> bind acc body_env))
    | _, _ ->
      Misc.fatal_errorf
        "CC.close_let: defining_exprs should have the same length as number of \
         variables"
  in
  close_named acc env ~let_bound_ids_with_kinds defining_expr
    (cont let_bound_ids_with_kinds env)

let close_let_cont acc env ~name ~is_exn_handler ~params
    ~(recursive : Asttypes.rec_flag)
    ~(handler : Acc.t -> Env.t -> Expr_with_acc.t)
    ~(body : Acc.t -> Env.t -> Expr_with_acc.t) : Expr_with_acc.t =
  (if is_exn_handler
  then
    match recursive with
    | Nonrecursive -> ()
    | Recursive ->
      Misc.fatal_errorf
        "[Let_cont]s marked as exception handlers must be [Nonrecursive]: %a"
        Continuation.print name);
  let handler_env, env_params = Env.add_vars_like env params in
  let handler_params =
    List.map2
      (fun param (_, uid, _, kind) -> BP.create param kind uid)
      env_params params
    |> Bound_parameters.create
  in
  let handler acc =
    let handler_env =
      match Acc.continuation_known_arguments ~cont:name acc with
      | None -> handler_env
      | Some args ->
        List.fold_left2
          (fun env arg_approx (param, (param_id, _param_uid, _, kind)) ->
            let env = Env.add_var_approximation env param arg_approx in
            match (arg_approx : Env.value_approximation) with
            | Value_symbol s | Closure_approximation { symbol = Some s; _ } ->
              Env.add_simple_to_substitute env param_id (Simple.symbol s) kind
            | _ -> env)
          handler_env args
          (List.combine env_params params)
    in
    handler acc handler_env
  in
  let body acc = body acc env in
  match recursive with
  | Nonrecursive ->
    Let_cont_with_acc.build_non_recursive acc name ~handler_params ~handler
      ~body ~is_exn_handler ~is_cold:false (* CR ncourant: from lambda *)
  | Recursive ->
    (* CR ncourant: from lambda *)
    let handlers =
      Continuation.Map.singleton name
        (handler, handler_params, is_exn_handler, false)
    in
    (* CR ncourant: If we could somehow detect the syntactically invariant
       parameters here, we could be able to improve the results of [Simplify] in
       some cases. *)
    Let_cont_with_acc.build_recursive acc
      ~invariant_params:Bound_parameters.empty ~handlers ~body

let close_exact_or_unknown_apply acc env
    ({ kind;
       func;
       args;
       continuation;
       exn_continuation;
       loc;
       inlined;
       probe;
       mode;
       region_close;
       region;
       ghost_region;
       args_arity;
       return_arity
     } :
      IR.apply) callee_approx ~replace_region : Expr_with_acc.t =
  let callee = find_simple_from_id env func in
  let mode =
    let current_region, current_ghost_region =
      match replace_region with
      | None ->
        let convert_region region =
          Option.map (fun region -> fst (Env.find_var env region)) region
        in
        convert_region region, convert_region ghost_region
      | Some (region, ghost_region) -> Some region, Some ghost_region
    in
    Alloc_mode.For_applications.from_lambda mode ~current_region
      ~current_ghost_region
  in
  let dbg = Debuginfo.from_location loc in
  let acc, call_kind, can_erase_callee =
    match kind with
    | Function -> (
      match (callee_approx : Env.value_approximation option) with
      | Some (Closure_approximation { code_id; code = code_or_meta; _ }) ->
        let meta = Code_or_metadata.code_metadata code_or_meta in
        if Code_metadata.is_tupled meta
        then
          (* CR keryan : We could do better here since we know the arity, but we
             would have to untuple the arguments and we lack information for
             now *)
          acc, Call_kind.indirect_function_call_unknown_arity mode, false
        else
          let result_arity_from_code = Code_metadata.result_arity meta in
          if (* See comment about when this check can be done, in
                simplify_apply_expr.ml *)
             Flambda_features.kind_checks ()
             && not
                  (Flambda_arity.equal_ignoring_subkinds return_arity
                     result_arity_from_code)
          then
            Misc.fatal_errorf
              "Wrong return arity for direct OCaml function call to %a@ \
               (expected %a, found %a):@ %a@ code metadata:@ %a"
              Ident.print func Flambda_arity.print result_arity_from_code
              Flambda_arity.print return_arity Debuginfo.print_compact dbg
              Code_metadata.print meta;
          let can_erase_callee =
            Flambda_features.classic_mode ()
            && not (Code_metadata.is_my_closure_used meta)
          in
          acc, Call_kind.direct_function_call code_id mode, can_erase_callee
      | None -> acc, Call_kind.indirect_function_call_unknown_arity mode, false
      | Some
          ( Value_unknown | Value_symbol _ | Value_const _
          | Block_approximation _ ) ->
        assert false (* See [close_apply] *))
    | Method { kind; obj } ->
      let acc, obj = find_simple acc env obj in
      ( acc,
        Call_kind.method_call (Call_kind.Method_kind.from_lambda kind) ~obj mode,
        false )
  in
  let acc, apply_exn_continuation =
    close_exn_continuation acc env exn_continuation
  in
  let acc, args = find_simples acc env args in
  let inlined_call = Inlined_attribute.from_lambda inlined in
  let probe = Probe.from_lambda probe in
  let position =
    match region_close with
    | Rc_normal | Rc_close_at_apply -> Apply.Position.Normal
    | Rc_nontail -> Apply.Position.Nontail
  in
  let apply =
    Apply.create
      ~callee:(if can_erase_callee then None else Some callee)
      ~continuation:(Return continuation) apply_exn_continuation ~args
      ~args_arity ~return_arity ~call_kind dbg ~inlined:inlined_call
      ~inlining_state:(Inlining_state.default ~round:0)
      ~probe ~position
      ~relative_history:(Env.relative_history_from_scoped ~loc env)
  in
  if Flambda_features.classic_mode ()
  then
    match Inlining.inlinable env apply callee_approx with
    | Not_inlinable ->
      let apply =
        Apply.with_inlined_attribute apply
          (Inlined_attribute.with_use_info (Apply.inlined apply)
             Unused_because_function_unknown)
      in
      Expr_with_acc.create_apply acc apply
    | Inlinable func_desc ->
      let acc = Acc.mark_continuation_as_untrackable continuation acc in
      let acc =
        Acc.mark_continuation_as_untrackable
          (Exn_continuation.exn_handler apply_exn_continuation)
          acc
      in
      Inlining.inline acc ~apply ~apply_depth:(Env.current_depth env) ~func_desc
  else Expr_with_acc.create_apply acc apply

let close_apply_cont acc env ~dbg cont trap_action args : Expr_with_acc.t =
  let acc, args = find_simples acc env args in
  let trap_action = close_trap_action_opt trap_action in
  let args_approx = List.map (find_value_approximation env) args in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ?trap_action ~args_approx cont ~args ~dbg
  in
  Expr_with_acc.create_apply_cont acc apply_cont

let close_switch acc env ~condition_dbg scrutinee (sw : IR.switch) :
    Expr_with_acc.t =
  let scrutinee = find_simple_from_id env scrutinee in
  let untagged_scrutinee = Variable.create "untagged" K.naked_immediate in
  let untagged_scrutinee_duid = Flambda_debug_uid.none in
  (* CR sspies: We should probably have something like a phantom let here, even
     though the target variable isn't user-visible. Then in the debugger, if
     only the untagged scrutinee were available (and not the original variable),
     we could still show a value for the original variable. See #3967. *)
  let untagged_scrutinee' =
    VB.create untagged_scrutinee untagged_scrutinee_duid Name_mode.normal
  in
  let known_const_scrutinee =
    match find_value_approximation_through_symbol acc env scrutinee with
    | Value_approximation.Value_const c -> Reg_width_const.is_tagged_immediate c
    | _ -> None
  in
  let untag =
    Named.create_prim (Unary (Untag_immediate, scrutinee)) condition_dbg
  in
  let acc, arms =
    List.fold_left_map
      (fun acc (case, cont, dbg, trap_action, args) ->
        let trap_action = close_trap_action_opt trap_action in
        let acc, args = find_simples acc env args in
        let args_approx = List.map (find_value_approximation env) args in
        let action acc =
          Apply_cont_with_acc.create acc ?trap_action ~args_approx cont ~args
            ~dbg
        in
        acc, (Targetint_31_63.of_int case, action))
      acc sw.consts
  in
  match arms, sw.failaction with
  | ( [(case, action)],
      Some (default_action, dbg, default_trap_action, default_args) )
    when sw.numconsts >= 3 ->
    (* Avoid enormous switches, where every arm goes to the same place except
       one, that arise from single-arm [Lambda] switches with a default case.
       (Seen in code generated by ppx_compare for variants, which exhibited
       quadratic size blowup.) *)
    let compare =
      Named.create_prim
        (Binary
           ( Int_comp (Naked_immediate, Yielding_bool Eq),
             Simple.var untagged_scrutinee,
             Simple.const (Reg_width_const.naked_immediate case) ))
        condition_dbg
    in
    let comparison_result = Variable.create "eq" K.naked_immediate in
    let comparison_result_duid = Flambda_debug_uid.none in
    let comparison_result' =
      VB.create comparison_result comparison_result_duid Name_mode.normal
    in
    let acc, default_action =
      let acc, args = find_simples acc env default_args in
      let trap_action = close_trap_action_opt default_trap_action in
      Apply_cont_with_acc.create acc ?trap_action default_action ~args ~dbg
    in
    let acc, switch =
      let scrutinee = Simple.var comparison_result in
      let acc, action = action acc in
      Expr_with_acc.create_switch acc
        (Switch.if_then_else ~condition_dbg ~scrutinee ~if_true:action
           ~if_false:default_action)
    in
    let acc, body =
      Let_with_acc.create acc
        (Bound_pattern.singleton comparison_result')
        compare ~body:switch
    in
    Let_with_acc.create acc
      (Bound_pattern.singleton untagged_scrutinee')
      untag ~body
  | _, _ ->
    let acc, arms =
      match sw.failaction with
      | None -> acc, Targetint_31_63.Map.of_list arms
      | Some (default, dbg, trap_action, args) ->
        Numeric_types.Int.Set.fold
          (fun case (acc, cases) ->
            let case = Targetint_31_63.of_int case in
            if Targetint_31_63.Map.mem case cases
            then acc, cases
            else
              let acc, args = find_simples acc env args in
              let trap_action = close_trap_action_opt trap_action in
              let default acc =
                Apply_cont_with_acc.create acc ?trap_action default ~args ~dbg
              in
              acc, Targetint_31_63.Map.add case default cases)
          (Numeric_types.Int.zero_to_n (sw.numconsts - 1))
          (acc, Targetint_31_63.Map.of_list arms)
    in
    if Targetint_31_63.Map.is_empty arms
    then Expr_with_acc.create_invalid acc Zero_switch_arms
    else
      let scrutinee = Simple.var untagged_scrutinee in
      let acc, body =
        match Targetint_31_63.Map.get_singleton arms with
        | Some (_discriminant, action) ->
          let acc, action = action acc in
          Expr_with_acc.create_apply_cont acc action
        | None -> (
          match known_const_scrutinee with
          | None ->
            let acc, arms =
              Targetint_31_63.Map.fold
                (fun case action (acc, arms) ->
                  let acc, arm = action acc in
                  acc, Targetint_31_63.Map.add case arm arms)
                arms
                (acc, Targetint_31_63.Map.empty)
            in
            Expr_with_acc.create_switch acc
              (Switch.create ~condition_dbg ~scrutinee ~arms)
          | Some case -> (
            match Targetint_31_63.Map.find case arms acc with
            | acc, action -> Expr_with_acc.create_apply_cont acc action
            | exception Not_found ->
              Expr_with_acc.create_invalid acc Zero_switch_arms))
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton untagged_scrutinee')
        untag ~body

let variables_for_unboxing boxed_variable_name (k : Function_decl.unboxing_kind)
    =
  (* CR sspies: In the future, improve the debugging UIDs we produce here
     (currently they are all none) for better debug information. *)
  match k with
  | Fields_of_block_with_tag_zero kinds ->
    List.mapi
      (fun i kind ->
        ( Variable.create
            (boxed_variable_name ^ "_field_" ^ Int.to_string i)
            (Flambda_kind.With_subkind.kind kind),
          Flambda_debug_uid.none,
          kind ))
      kinds
  | Unboxed_number bn ->
    [ ( Variable.create
          (boxed_variable_name ^ "_unboxed")
          (Flambda_kind.Boxable_number.unboxed_kind bn),
        Flambda_debug_uid.none,
        Flambda_kind.With_subkind.naked_of_boxable_number bn ) ]
  | Unboxed_float_record num_fields ->
    List.init num_fields (fun i ->
        ( Variable.create
            (boxed_variable_name ^ "_floatfield_" ^ Int.to_string i)
            K.naked_float,
          Flambda_debug_uid.none,
          Flambda_kind.With_subkind.naked_float ))

let unboxing_primitive (k : Function_decl.unboxing_kind) boxed_variable i =
  match k with
  | Fields_of_block_with_tag_zero kinds ->
    let block_access_kind : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known (Targetint_31_63.of_int (List.length kinds));
          field_kind = Any_value
        }
    in
    Flambda_primitive.Unary
      ( Block_load { kind = block_access_kind; mut = Immutable; field = i },
        Simple.var boxed_variable )
  | Unboxed_number bn ->
    Flambda_primitive.Unary (Unbox_number bn, Simple.var boxed_variable)
  | Unboxed_float_record num_fields ->
    let block_access_kind : P.Block_access_kind.t =
      Naked_floats { size = Known (Targetint_31_63.of_int num_fields) }
    in
    Flambda_primitive.Unary
      ( Block_load { kind = block_access_kind; mut = Immutable; field = i },
        Simple.var boxed_variable )

let boxing_primitive (k : Function_decl.unboxing_kind) alloc_mode
    unboxed_variables : Flambda_primitive.t =
  match k with
  | Fields_of_block_with_tag_zero kinds ->
    Flambda_primitive.Variadic
      ( Make_block (Values (Tag.Scannable.zero, kinds), Immutable, alloc_mode),
        Simple.vars unboxed_variables )
  | Unboxed_number bn ->
    let unboxed_variable =
      match unboxed_variables with
      | [var] -> var
      | [] | _ :: _ :: _ ->
        Misc.fatal_error
          "boxing_primitive: Unboxed_number should correspond to a single \
           variable"
    in
    Flambda_primitive.Unary
      (Box_number (bn, alloc_mode), Simple.var unboxed_variable)
  | Unboxed_float_record _ ->
    Flambda_primitive.Variadic
      ( Make_block (Naked_floats, Immutable, alloc_mode),
        Simple.vars unboxed_variables )

let compute_body_of_unboxed_function acc my_region my_closure
    ~unarized_params:params params_arity ~unarized_param_modes:param_modes
    function_slot compute_body return return_continuation unboxed_params
    unboxed_return unboxed_function_slot =
  let my_closure_duid = Flambda_debug_uid.none in
  let rec box_params params params_arity param_modes params_unboxing body =
    match params, params_arity, param_modes, params_unboxing with
    | [], [], [], [] -> [], [], [], body
    | ( param :: params,
        param_arity :: params_arity,
        param_mode :: param_modes,
        param_unboxing :: params_unboxing ) -> (
      let main_code_params, main_code_params_arity, main_code_param_modes, body
          =
        box_params params params_arity param_modes params_unboxing body
      in
      match (param_unboxing : Function_decl.unboxing_kind option) with
      | None ->
        ( param :: main_code_params,
          param_arity :: main_code_params_arity,
          param_mode :: main_code_param_modes,
          body )
      | Some k ->
        let boxed_variable_name = Variable.name (Bound_parameter.var param) in
        let vars_with_kinds = variables_for_unboxing boxed_variable_name k in
        let body acc =
          let acc, body = body acc in
          let alloc_mode =
            Alloc_mode.For_allocations.from_lambda ~current_region:my_region
              (Alloc_mode.For_types.to_lambda param_mode)
          in
          let param_duid = Flambda_debug_uid.none in
          Let_with_acc.create acc
            (Bound_pattern.singleton
               (Bound_var.create
                  (Bound_parameter.var param)
                  param_duid Name_mode.normal))
            (* CR sspies: In the future, improve the debugging UIDs here if
               possible. *)
            (Named.create_prim
               (boxing_primitive k alloc_mode
                  (List.map (fun (var, _, _) -> var) vars_with_kinds))
               Debuginfo.none)
            ~body
        in
        ( List.map
            (fun (var, var_duid, kind) ->
              Bound_parameter.create var kind var_duid)
            vars_with_kinds
          @ main_code_params,
          List.map (fun (_, _, kind) -> kind) vars_with_kinds
          @ main_code_params_arity,
          (* CR ncourant: is this correct in the presence of records with global
             fields? *)
          List.map (fun _ -> param_mode) vars_with_kinds @ main_code_param_modes,
          body ))
    | ([] | _ :: _), _, _, _ ->
      Misc.fatal_errorf
        "Parameters and unboxed parameters do not have the same length@."
  in
  let main_code_params, main_code_params_arity, main_code_param_modes, body =
    box_params
      (Bound_parameters.to_list params)
      (Flambda_arity.unarize params_arity)
      param_modes unboxed_params compute_body
  in
  (* This function is always fully applied, so use a single non-unarized
     parameter to avoid useless currying functions being generated. *)
  let main_code_params_arity =
    [ Flambda_arity.Component_for_creation.Unboxed_product
        (List.map
           (fun kind -> Flambda_arity.Component_for_creation.Singleton kind)
           main_code_params_arity) ]
  in
  let acc, unboxed_body, result_arity_main_code, unboxed_return_continuation =
    match unboxed_return with
    | None ->
      let acc, body = body acc in
      acc, body, return, return_continuation
    | Some k ->
      let vars_with_kinds = variables_for_unboxing "result" k in
      let unboxed_return_continuation =
        Continuation.create ~sort:Return ~name:"unboxed_return" ()
      in
      let boxed_variable = Variable.create "boxed_result" K.value in
      let boxed_variable_duid = Flambda_debug_uid.none in
      let return =
        match Flambda_arity.unarized_components return with
        | [return] -> return
        | [] | _ :: _ :: _ ->
          Misc.fatal_error
            "Expected a single return for the boxed code of function with \
             unboxed return@."
      in
      let handler_params =
        Bound_parameters.create
          [Bound_parameter.create boxed_variable return boxed_variable_duid]
      in
      let handler acc =
        let acc, apply_cont =
          Apply_cont_with_acc.create acc unboxed_return_continuation
            ~args:
              (List.map
                 (fun (var, _duid, _kind) -> Simple.var var)
                 vars_with_kinds)
            ~dbg:Debuginfo.none
        in
        let acc, apply_cont = Expr_with_acc.create_apply_cont acc apply_cont in
        let (acc, expr), _ =
          List.fold_left
            (fun ((acc, expr), i) (var, var_duid, _kind) ->
              ( Let_with_acc.create acc
                  (Bound_pattern.singleton
                     (Bound_var.create var var_duid Name_mode.normal))
                  (Named.create_prim
                     (unboxing_primitive k boxed_variable i)
                     Debuginfo.none)
                  ~body:expr,
                Targetint_31_63.(add one i) ))
            ((acc, apply_cont), Targetint_31_63.zero)
            vars_with_kinds
        in
        acc, expr
      in
      let acc, unboxed_body =
        Let_cont_with_acc.build_non_recursive acc return_continuation
          ~handler_params ~handler ~body ~is_exn_handler:false ~is_cold:false
      in
      ( acc,
        unboxed_body,
        Flambda_arity.create_singletons
          (List.map (fun (_, _, kind) -> kind) vars_with_kinds),
        unboxed_return_continuation )
  in
  let my_unboxed_closure = Variable.create "my_unboxed_closure" K.value in
  let acc, unboxed_body =
    Let_with_acc.create acc
      (Bound_pattern.singleton
         (Bound_var.create my_closure my_closure_duid Name_mode.normal))
      (Named.create_prim
         (Flambda_primitive.Unary
            ( Project_function_slot
                { move_from = unboxed_function_slot; move_to = function_slot },
              Simple.var my_unboxed_closure ))
         Debuginfo.none)
      ~body:unboxed_body
  in
  ( acc,
    unboxed_body,
    Bound_parameters.create main_code_params,
    Flambda_arity.create main_code_params_arity,
    main_code_param_modes,
    false,
    (* first_complex_local_param = 0, but function should never be partially
       applied anyway *)
    0,
    result_arity_main_code,
    unboxed_return_continuation,
    my_unboxed_closure )

let make_unboxed_function_wrapper acc function_slot ~unarized_params:params
    params_arity ~unarized_param_modes:param_modes return result_arity_main_code
    code_id main_code_id decl loc external_env recursive
    contains_no_escaping_local_allocs cost_metrics dbg is_tupled
    inlining_decision absolute_history relative_history main_code
    by_function_slot function_code_ids unboxed_function_slot unboxed_params
    unboxed_return =
  (* The outside caller gave us the function slot and code ID meant for the
     boxed function, which will be a wrapper. So in this branch everything
     starting with 'main_' refers to the version with unboxed return/params. *)
  let main_function_slot = unboxed_function_slot in
  let main_name = Function_slot.name unboxed_function_slot in
  let main_closure = Variable.create main_name K.value in
  let main_closure_duid = Flambda_debug_uid.none in
  let return_continuation = Continuation.create () in
  let exn_continuation = Continuation.create () in
  let my_closure = Variable.create "my_closure" K.value in
  let my_region =
    if contains_no_escaping_local_allocs
    then None
    else Some (Variable.create "my_region" K.region)
  in
  let my_ghost_region =
    if contains_no_escaping_local_allocs
    then None
    else Some (Variable.create "my_ghost_region" K.region)
  in
  let my_depth = Variable.create "my_depth" K.rec_info in
  let rec unbox_params params params_unboxing =
    match params, params_unboxing with
    | [], [] -> [], [], fun body free_names_of_body -> body, free_names_of_body
    | [], _ :: _ | _ :: _, [] ->
      Misc.fatal_error "params and params_unboxing do not have the same length"
    | param :: params, param_unboxing :: params_unboxing -> (
      let args, args_arity, body_wrapper =
        unbox_params params params_unboxing
      in
      match param_unboxing with
      | None ->
        ( Bound_parameter.simple param :: args,
          Bound_parameter.kind param :: args_arity,
          body_wrapper )
      | Some k ->
        let boxed_variable_name = Variable.name (Bound_parameter.var param) in
        let vars_with_kinds = variables_for_unboxing boxed_variable_name k in
        let new_wrapper body free_names_of_body =
          let body, free_names_of_body = body_wrapper body free_names_of_body in
          let body, free_names_of_body, _ =
            List.fold_left
              (fun (body, free_names_of_body, i) (var, var_duid, _kind) ->
                let named =
                  Named.create_prim
                    (unboxing_primitive k (Bound_parameter.var param) i)
                    Debuginfo.none
                in
                ( Expr.create_let
                    (Let_expr.create
                       (Bound_pattern.singleton
                          (Bound_var.create var var_duid Name_mode.normal))
                       named ~body
                       ~free_names_of_body:(Known free_names_of_body)),
                  Name_occurrences.union (Named.free_names named)
                    (Name_occurrences.remove_var free_names_of_body ~var),
                  Targetint_31_63.(add one i) ))
              (body, free_names_of_body, Targetint_31_63.zero)
              vars_with_kinds
          in
          body, free_names_of_body
        in
        ( List.map (fun (var, _duid, _kind) -> Simple.var var) vars_with_kinds
          @ args,
          List.map (fun (_var, _duid, kind) -> kind) vars_with_kinds
          @ args_arity,
          new_wrapper ))
  in
  let args, args_arity, body_wrapper =
    unbox_params (Bound_parameters.to_list params) unboxed_params
  in
  let args_arity =
    Flambda_arity.create
      [ Flambda_arity.Component_for_creation.Unboxed_product
          (List.map
             (fun kind -> Flambda_arity.Component_for_creation.Singleton kind)
             args_arity) ]
  in
  let make_body cont =
    let main_application =
      Apply_expr.create
        ~callee:(Some (Simple.var main_closure))
        ~continuation:(Return cont)
        (Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[])
        ~args ~args_arity ~return_arity:result_arity_main_code
        ~call_kind:
          (Call_kind.direct_function_call main_code_id
             (Alloc_mode.For_applications.from_lambda
                (Function_decl.result_mode decl)
                ~current_region:my_region ~current_ghost_region:my_ghost_region))
        Debuginfo.none ~inlined:Inlined_attribute.Default_inlined
        ~inlining_state:(Inlining_state.default ~round:0)
        ~probe:None ~position:Normal
        ~relative_history:(Env.relative_history_from_scoped ~loc external_env)
    in
    let projection =
      Named.create_prim
        (Flambda_primitive.Unary
           ( Project_function_slot
               { move_from = function_slot; move_to = main_function_slot },
             Simple.var my_closure ))
        Debuginfo.none
    in
    let body =
      Expr.create_let
        (Let_expr.create
           (Bound_pattern.singleton
              (Bound_var.create main_closure main_closure_duid Name_mode.normal))
           projection
           ~body:(Expr.create_apply main_application)
           ~free_names_of_body:(Known (Apply_expr.free_names main_application)))
    in
    let free_names_of_body =
      Name_occurrences.union
        (Named.free_names projection)
        (Name_occurrences.remove_var
           (Apply_expr.free_names main_application)
           ~var:main_closure)
    in
    body_wrapper body free_names_of_body
  in
  let make_return_wrapper box_result =
    let cont = Continuation.create () in
    let body, free_names_of_body = make_body cont in
    let handler, free_names_of_handler =
      let unboxed_returns =
        Bound_parameters.create
          (List.map
             (fun kind ->
               let var =
                 Variable.create "unboxed_return"
                   (Flambda_kind.With_subkind.kind kind)
               in
               let var_duid = Flambda_debug_uid.none in
               Bound_parameter.create var kind var_duid)
             (Flambda_arity.unarized_components result_arity_main_code))
      in
      let handler, free_names_of_handler =
        let boxed_return = Variable.create "boxed_return" K.value in
        let boxed_return_duid = Flambda_debug_uid.none in
        let return_apply_cont =
          Apply_cont.create return_continuation
            ~args:[Simple.var boxed_return]
            ~dbg:Debuginfo.none
        in
        let box_result_named =
          Named.create_prim
            (box_result (Bound_parameters.vars unboxed_returns))
            Debuginfo.none
        in
        ( Expr.create_let
            (Let_expr.create
               (Bound_pattern.singleton
                  (Bound_var.create boxed_return boxed_return_duid
                     Name_mode.normal))
               box_result_named
               ~body:(Expr.create_apply_cont return_apply_cont)
               ~free_names_of_body:
                 (Known (Apply_cont.free_names return_apply_cont))),
          Name_occurrences.union
            (Named.free_names box_result_named)
            (Name_occurrences.remove_var
               (Apply_cont.free_names return_apply_cont)
               ~var:boxed_return) )
      in
      ( Continuation_handler.create unboxed_returns ~handler
          ~free_names_of_handler:(Known free_names_of_handler)
          ~is_exn_handler:false ~is_cold:false,
        List.fold_left
          (fun free_names param ->
            Name_occurrences.remove_var free_names
              ~var:(Bound_parameter.var param))
          free_names_of_handler
          (Bound_parameters.to_list unboxed_returns) )
    in
    ( Let_cont_expr.create_non_recursive cont handler ~body
        ~free_names_of_body:(Known free_names_of_body),
      Name_occurrences.union free_names_of_handler
        (Name_occurrences.remove_continuation free_names_of_body
           ~continuation:cont) )
  in
  let alloc_mode =
    Alloc_mode.For_allocations.from_lambda
      (Function_decl.result_mode decl)
      ~current_region:my_region
  in
  let body, free_names_of_body =
    match unboxed_return with
    | None -> make_body return_continuation
    | Some k -> make_return_wrapper (boxing_primitive k alloc_mode)
  in
  let wrapper_params_and_body =
    Function_params_and_body.create ~return_continuation ~exn_continuation
      params ~body ~free_names_of_body:(Known free_names_of_body) ~my_closure
      ~my_region ~my_ghost_region ~my_depth
  in
  let free_names_of_params_and_body =
    Name_occurrences.remove_continuation ~continuation:return_continuation
      (Name_occurrences.remove_continuation ~continuation:exn_continuation
         (Name_occurrences.remove_var ~var:my_closure
            (Name_occurrences.remove_var_opt ~var:my_region
               (Name_occurrences.remove_var_opt ~var:my_ghost_region
                  (Name_occurrences.remove_var ~var:my_depth
                     (List.fold_left
                        (fun free_names param ->
                          Name_occurrences.remove_var free_names
                            ~var:(Bound_parameter.var param))
                        free_names_of_body
                        (Bound_parameters.to_list params)))))))
  in
  let wrapper_code =
    Code.create code_id ~params_and_body:wrapper_params_and_body
      ~free_names_of_params_and_body ~params_arity ~param_modes
      ~first_complex_local_param:(Function_decl.first_complex_local_param decl)
      ~result_arity:return ~result_types:Unknown
      ~result_mode:(Function_decl.result_mode decl)
      ~stub:true ~inline:Inline_attribute.Default_inline
      ~poll_attribute:
        (Poll_attribute.from_lambda (Function_decl.poll_attribute decl))
      ~zero_alloc_attribute:
        (Zero_alloc_attribute.from_lambda
           (Function_decl.zero_alloc_attribute decl))
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~is_opaque:false ~recursive ~newer_version_of:None ~cost_metrics
      ~inlining_arguments:(Inlining_arguments.create ~round:0)
      ~dbg ~is_tupled ~is_my_closure_used:true ~inlining_decision
      ~absolute_history ~relative_history ~loopify:Never_loopify
  in
  let main_approx =
    let code = Code_or_metadata.create main_code in
    let meta = Code_or_metadata.remember_only_metadata code in
    if Flambda_features.classic_mode ()
    then (
      Inlining_report.record_decision_at_function_definition ~absolute_history
        ~code_metadata:(Code_or_metadata.code_metadata meta)
        ~pass:After_closure_conversion
        ~are_rebuilding_terms:Are_rebuilding_terms.are_rebuilding
        inlining_decision;
      if Function_decl_inlining_decision_type.must_be_inlined inlining_decision
      then code
      else meta)
    else meta
  in
  (* The wrapper doesn't contain any set of closures *)
  let slot_offsets = Slot_offsets.empty in
  ( wrapper_code,
    Function_slot.Map.add main_function_slot main_approx by_function_slot,
    (main_function_slot, main_code_id) :: function_code_ids,
    Acc.add_code ~code_id:main_code_id ~code:main_code ~slot_offsets acc )

let close_one_function acc ~code_id ~external_env ~by_function_slot
    ~function_code_ids decl ~has_lifted_closure ~value_slots_from_idents
    ~function_slots_from_idents ~approx_map function_declarations =
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let unarized_params = Function_decl.params decl in
  let params_arity = Function_decl.params_arity decl in
  let unarized_param_modes =
    List.map
      (fun (p : Function_decl.param) -> Alloc_mode.For_types.from_lambda p.mode)
      unarized_params
  in
  let return = Function_decl.return decl in
  let calling_convention = Function_decl.calling_convention decl in
  let return_continuation = Function_decl.return_continuation decl in
  let acc, exn_continuation =
    close_exn_continuation acc external_env
      (Function_decl.exn_continuation decl)
  in
  assert (
    match Exn_continuation.extra_args exn_continuation with
    | [] -> true
    | _ :: _ -> false);
  let my_closure = Variable.create "my_closure" K.value in
  let recursive = Function_decl.recursive decl in
  (* Mark function available for loopify only if it is a single recursive
     function *)
  let is_single_recursive_function =
    match recursive, Function_decls.to_list function_declarations with
    | Recursive, [_] -> true
    | Recursive, ([] | _ :: _ :: _) -> false
    | Non_recursive, _ -> false
  in
  let acc =
    Acc.push_closure_info acc ~return_continuation ~exn_continuation ~my_closure
      ~is_purely_tailrec:is_single_recursive_function ~code_id
  in
  let my_region = Function_decl.my_region decl in
  let my_ghost_region = Function_decl.my_ghost_region decl in
  let function_slot = Function_decl.function_slot decl in
  let my_depth = Variable.create "my_depth" K.rec_info in
  let next_depth = Variable.create "next_depth" K.rec_info in
  let next_depth_duid = Flambda_debug_uid.none in
  let our_let_rec_ident = Function_decl.let_rec_ident decl in
  let is_curried =
    match Function_decl.kind decl with Curried _ -> true | Tupled -> false
  in
  (* The free variables are:

     - The parameters: direct substitution by [Variable]s

     - The function being defined: accessible through [my_closure]

     - Other functions in the set being defined: accessible from [my_closure]
     then a [Project_function_slot]

     - Other free variables: accessible using [Project_value_slot] from
     [my_closure].

     Note that free variables corresponding to predefined exception identifiers
     have been filtered out by [close_functions], above. *)
  let (value_slots_to_bind : Value_slot.t Variable.Map.t), vars_for_idents =
    Ident.Map.fold
      (fun id value_slot (value_slots_to_bind, vars_for_idents) ->
        let var =
          Variable.create_with_same_name_as_ident id
            (Value_slot.kind value_slot)
        in
        ( Variable.Map.add var value_slot value_slots_to_bind,
          Ident.Map.add id var vars_for_idents ))
      value_slots_from_idents
      (Variable.Map.empty, Ident.Map.empty)
  in
  let coerce_to_deeper =
    Coercion.change_depth
      ~from:(Rec_info_expr.var my_depth)
      ~to_:(Rec_info_expr.var next_depth)
  in
  if has_lifted_closure && not (Variable.Map.is_empty value_slots_to_bind)
  then
    Misc.fatal_errorf
      "Variables found in closure when trying to lift %a in \
       [Closure_conversion]."
      Ident.print our_let_rec_ident;
  let closure_env = Env.clear_local_bindings external_env in
  (* Add the variables for function projections *)
  let closure_vars_to_bind, closure_env =
    if has_lifted_closure
    then (* No projection needed *)
      Variable.Map.empty, closure_env
    else
      List.fold_left
        (fun (to_bind, env) function_decl ->
          let let_rec_ident = Function_decl.let_rec_ident function_decl in
          let to_bind, var, function_slot =
            if Ident.same our_let_rec_ident let_rec_ident && is_curried
            then
              (* When the function being compiled is tupled, my_closure points
                 to the curried version but let_rec_ident is called with tuple
                 arguments, so the correct closure to bind is the one in the
                 function_slots_from_idents map. *)
              to_bind, my_closure, Function_decl.function_slot decl
              (* my_closure is already bound *)
            else
              let variable =
                Variable.create_with_same_name_as_ident let_rec_ident K.value
              in
              let function_slot =
                Ident.Map.find let_rec_ident function_slots_from_idents
              in
              ( Variable.Map.add variable function_slot to_bind,
                variable,
                function_slot )
          in
          let simple = Simple.with_coercion (Simple.var var) coerce_to_deeper in
          let approx = Function_slot.Map.find function_slot approx_map in
          let env =
            Env.add_simple_to_substitute env let_rec_ident simple
              K.With_subkind.any_value
          in
          let env = Env.add_var_approximation env var approx in
          to_bind, env)
        (Variable.Map.empty, closure_env)
        (Function_decls.to_list function_declarations)
  in
  let closure_env =
    Ident.Map.fold
      (fun id var env ->
        let simple, kind = find_simple_from_id_with_kind external_env id in
        Env.add_var_approximation
          (Env.add_var env id var kind)
          var
          (find_value_approximation_through_symbol acc env simple))
      vars_for_idents closure_env
  in
  let closure_env =
    List.fold_right
      (fun (p : Function_decl.param) env ->
        let env, _var = Env.add_var_like env p.name User_visible p.kind in
        env)
      unarized_params closure_env
  in
  let closure_env, my_region =
    match my_region with
    | None -> closure_env, None
    | Some my_region ->
      let env, region =
        Env.add_var_like closure_env my_region Not_user_visible
          K.With_subkind.region
      in
      env, Some region
  in
  let closure_env, my_ghost_region =
    match my_ghost_region with
    | None -> closure_env, None
    | Some my_ghost_region ->
      let env, region =
        Env.add_var_like closure_env my_ghost_region Not_user_visible
          K.With_subkind.region
      in
      env, Some region
  in
  let closure_env = Env.with_depth closure_env my_depth in
  let closure_env, absolute_history, relative_history =
    let tracker = Env.inlining_history_tracker closure_env in
    let absolute, relative =
      Inlining_history.Tracker.fundecl_of_scoped_location
        ~name:(Function_slot.name function_slot)
        ~path_to_root:(Env.path_to_root closure_env)
        loc tracker
    in
    ( Env.use_inlining_history_tracker closure_env
        (Inlining_history.Tracker.inside_function absolute),
      absolute,
      relative )
  in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are not marked
     as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let unarized_params =
    List.map
      (fun (p : Function_decl.param) ->
        let var = fst (Env.find_var closure_env p.name) in
        BP.create var p.kind p.debug_uid)
      unarized_params
    |> Bound_parameters.create
  in
  let acc = Acc.with_seen_a_function acc false in
  let compute_body acc =
    let acc, body =
      (* XXX seems like this needs to know what [my_region] is *)
      try body acc closure_env
      with Misc.Fatal_error ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "\n\
           %tContext is:%t closure converting function@ with \
           [our_let_rec_ident] %a (function slot %a)\n\n"
          (* @ \ *)
          (* and body:@ %a *)
          Flambda_colours.error Flambda_colours.pop Ident.print
          our_let_rec_ident Function_slot.print function_slot;
        (* print body *)
        Printexc.raise_with_backtrace Misc.Fatal_error bt
    in
    let my_closure' = Simple.var my_closure in
    let acc, body =
      (* CR mshinwell: These Project_function_slot operations should maybe be
         inserted at the point of use rather than at the top of the function. We
         should also check the behaviour of the backend w.r.t. CSE of
         projections from closures. *)
      Variable.Map.fold
        (fun var move_to (acc, body) ->
          let move : Flambda_primitive.unary_primitive =
            Project_function_slot { move_from = function_slot; move_to }
          in
          let var = VB.create var Flambda_debug_uid.none Name_mode.normal in
          (* CR sspies: In the future, improve the debugging UIDs here if
             possible. *)
          let named =
            Named.create_prim (Unary (move, my_closure')) Debuginfo.none
          in
          Let_with_acc.create acc (Bound_pattern.singleton var) named ~body)
        closure_vars_to_bind (acc, body)
    in
    let acc, body =
      Variable.Map.fold
        (fun var value_slot (acc, body) ->
          let var = VB.create var Flambda_debug_uid.none Name_mode.normal in
          (* CR sspies: In the future, improve the debugging UIDs here if
             possible. *)
          let named =
            Named.create_prim
              (Unary
                 ( Project_value_slot
                     { project_from = function_slot; value_slot },
                   my_closure' ))
              Debuginfo.none
          in
          Let_with_acc.create acc (Bound_pattern.singleton var) named ~body)
        value_slots_to_bind (acc, body)
    in
    let next_depth_expr = Rec_info_expr.succ (Rec_info_expr.var my_depth) in
    let bound =
      Bound_pattern.singleton
        (Bound_var.create next_depth next_depth_duid Name_mode.normal)
    in
    Let_with_acc.create acc bound (Named.create_rec_info next_depth_expr) ~body
  in
  let is_tupled =
    match Function_decl.kind decl with Curried _ -> false | Tupled -> true
  in
  let ( acc,
        body,
        main_code_unarized_params,
        main_code_params_arity,
        main_code_unarized_param_modes,
        main_code_is_tupled,
        first_complex_local_param_main_code,
        result_arity_main_code,
        return_continuation,
        my_closure ) =
    match calling_convention with
    | Normal_calling_convention ->
      let acc, body = compute_body acc in
      ( acc,
        body,
        unarized_params,
        params_arity,
        unarized_param_modes,
        is_tupled,
        Function_decl.first_complex_local_param decl,
        return,
        return_continuation,
        my_closure )
    | Unboxed_calling_convention
        (unboxed_params, unboxed_return, unboxed_function_slot) ->
      compute_body_of_unboxed_function acc my_region my_closure ~unarized_params
        params_arity ~unarized_param_modes function_slot compute_body return
        return_continuation unboxed_params unboxed_return unboxed_function_slot
  in
  let contains_subfunctions = Acc.seen_a_function acc in
  let cost_metrics = Acc.cost_metrics acc in
  let inline : Inline_attribute.t =
    (* We make a decision based on [fallback_inlining_heuristic] here to try to
       mimic Closure's behaviour as closely as possible, particularly when there
       are functions involving constant closures, which are not lifted during
       Closure (but will prevent inlining) but will likely have been lifted by
       our other check in [Inlining_cost] (thus preventing us seeing they were
       originally there). Note that while Closure never marks as inlinable
       functions in a set a recursive definitions with more than one function,
       we do not try to reproduce this particular property and can mark as
       inlinable such functions. *)
    if contains_subfunctions
       && Flambda_features.Expert.fallback_inlining_heuristic ()
    then Never_inline
    else Inline_attribute.from_lambda (Function_decl.inline decl)
  in
  let free_names_of_body = Acc.free_names acc in
  let params_and_body =
    Function_params_and_body.create ~return_continuation
      ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      main_code_unarized_params ~body ~my_closure ~my_region ~my_ghost_region
      ~my_depth ~free_names_of_body:(Known free_names_of_body)
  in
  let result_mode = Function_decl.result_mode decl in
  (match my_region with
  | Some _ -> assert (not (Lambda.is_heap_mode result_mode))
  | None -> assert (Lambda.is_heap_mode result_mode));
  (match my_ghost_region with
  | Some _ -> assert (not (Lambda.is_heap_mode result_mode))
  | None -> assert (Lambda.is_heap_mode result_mode));
  let acc =
    List.fold_left
      (fun acc param -> Acc.remove_var_from_free_names (BP.var param) acc)
      acc
      (Bound_parameters.to_list main_code_unarized_params)
    |> Acc.remove_var_from_free_names my_closure
    |> Acc.remove_var_opt_from_free_names my_region
    |> Acc.remove_var_opt_from_free_names my_ghost_region
    |> Acc.remove_var_from_free_names my_depth
    |> Acc.remove_continuation_from_free_names return_continuation
    |> Acc.remove_continuation_from_free_names
         (Exn_continuation.exn_handler exn_continuation)
  in
  let closure_info, acc = Acc.pop_closure_info acc in
  let is_tupled =
    match Function_decl.kind decl with Curried _ -> false | Tupled -> true
  in
  let inlining_decision =
    if Flambda_features.classic_mode ()
    then Inlining.definition_inlining_decision inline cost_metrics
    else if stub
    then Function_decl_inlining_decision_type.Stub
    else Function_decl_inlining_decision_type.Not_yet_decided
  in
  let loopify : Loopify_attribute.t =
    match Function_decl.loop decl with
    | Always_loop -> Always_loopify
    | Never_loop -> Never_loopify
    | Default_loop ->
      if closure_info.is_purely_tailrec
      then Default_loopify_and_tailrec
      else Default_loopify_and_not_tailrec
  in
  let main_code_id =
    match calling_convention with
    | Normal_calling_convention -> code_id
    | Unboxed_calling_convention _ -> Code_id.rename code_id
  in
  let contains_no_escaping_local_allocs =
    match Function_decl.result_mode decl with
    | Alloc_heap -> false
    | Alloc_local -> true
  in
  let main_code =
    Code.create main_code_id ~params_and_body
      ~free_names_of_params_and_body:(Acc.free_names acc)
      ~params_arity:main_code_params_arity
      ~param_modes:main_code_unarized_param_modes
      ~first_complex_local_param:first_complex_local_param_main_code
      ~result_arity:result_arity_main_code ~result_types:Unknown ~result_mode
      ~stub ~inline
      ~poll_attribute:
        (Poll_attribute.from_lambda (Function_decl.poll_attribute decl))
      ~zero_alloc_attribute:
        (Zero_alloc_attribute.from_lambda
           (Function_decl.zero_alloc_attribute decl))
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~is_opaque:(Function_decl.is_opaque decl)
      ~recursive ~newer_version_of:None ~cost_metrics
      ~inlining_arguments:(Inlining_arguments.create ~round:0)
      ~dbg ~is_tupled:main_code_is_tupled
      ~is_my_closure_used:
        (Function_params_and_body.is_my_closure_used params_and_body)
      ~inlining_decision ~absolute_history ~relative_history ~loopify
  in
  let function_code_ids = (function_slot, code_id) :: function_code_ids in
  let code, by_function_slot, function_code_ids, acc =
    match calling_convention with
    | Normal_calling_convention ->
      main_code, by_function_slot, function_code_ids, acc
    | Unboxed_calling_convention
        (unboxed_params, unboxed_return, unboxed_function_slot) ->
      make_unboxed_function_wrapper acc function_slot ~unarized_params
        params_arity ~unarized_param_modes return result_arity_main_code code_id
        main_code_id decl loc external_env recursive
        contains_no_escaping_local_allocs cost_metrics dbg is_tupled
        inlining_decision absolute_history relative_history main_code
        by_function_slot function_code_ids unboxed_function_slot unboxed_params
        unboxed_return
  in
  let approx =
    let code = Code_or_metadata.create code in
    let meta = Code_or_metadata.remember_only_metadata code in
    if Flambda_features.classic_mode ()
    then (
      Inlining_report.record_decision_at_function_definition ~absolute_history
        ~code_metadata:(Code_or_metadata.code_metadata meta)
        ~pass:After_closure_conversion
        ~are_rebuilding_terms:Are_rebuilding_terms.are_rebuilding
        inlining_decision;
      if Function_decl_inlining_decision_type.must_be_inlined inlining_decision
      then code
      else meta)
    else meta
  in
  let acc = Acc.add_code ~code_id ~code acc in
  let acc = Acc.with_seen_a_function acc true in
  ( acc,
    ( Function_slot.Map.add function_slot approx by_function_slot,
      function_code_ids ) )

let close_functions acc external_env ~current_region function_declarations =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let value_slots_from_idents =
    Ident.Set.fold
      (fun id map ->
        (* Filter out predefined exception identifiers and simple substitutions.
           The former will be turned into symbols, and the latter substituted
           when we closure-convert the body *)
        let has_non_var_subst, subst_var, kind =
          match Env.find_simple_to_substitute_exn external_env id with
          | exception Not_found ->
            let _, kind = find_simple_from_id_with_kind external_env id in
            false, None, kind
          | simple, kind ->
            Simple.pattern_match simple
              ~const:(fun _ -> true, None, kind)
              ~name:(fun name ~coercion:_ ->
                Name.pattern_match name
                  ~var:(fun var -> false, Some var, kind)
                  ~symbol:(fun _ -> true, None, kind))
        in
        if has_non_var_subst || Ident.is_predef id
        then map
        else
          let name =
            match subst_var with
            | None -> Ident.name id
            | Some var -> Variable.name var
          in
          let is_always_immediate =
            match[@ocaml.warning "-4"]
              Flambda_kind.With_subkind.non_null_value_subkind kind
            with
            | Tagged_immediate -> true
            | _ -> false
          in
          Ident.Map.add id
            (Value_slot.create compilation_unit ~name ~is_always_immediate
               (Flambda_kind.With_subkind.kind kind))
            map)
      (Function_decls.all_free_idents function_declarations)
      Ident.Map.empty
  in
  let can_be_lifted =
    Ident.Map.is_empty value_slots_from_idents
    && Flambda_features.classic_mode ()
  in
  let func_decl_list = Function_decls.to_list function_declarations in
  let function_slots_from_idents =
    List.fold_left
      (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let function_slot = Function_decl.function_slot decl in
        Ident.Map.add id function_slot map)
      Ident.Map.empty func_decl_list
  in
  let function_code_ids =
    List.fold_left
      (fun map decl ->
        let function_slot = Function_decl.function_slot decl in
        let code_id =
          Code_id.create
            ~name:(Function_slot.to_string function_slot)
            compilation_unit
        in
        Function_slot.Map.add function_slot code_id map)
      Function_slot.Map.empty func_decl_list
  in
  let approx_map =
    List.fold_left
      (fun approx_map decl ->
        (* The only fields of metadata which are used for this pass are
           params_arity, param_modes, is_tupled, first_complex_local_param,
           result_mode, and result_arity. We try to populate the different
           fields as much as possible, but put dummy values when they are not
           yet computed or simply too expensive to compute for the other
           fields. *)
        let function_slot = Function_decl.function_slot decl in
        let code_id = Function_slot.Map.find function_slot function_code_ids in
        let params = Function_decl.params decl in
        let params_arity = Function_decl.params_arity decl in
        let param_modes =
          List.map
            (fun (p : Function_decl.param) ->
              Alloc_mode.For_types.from_lambda p.mode)
            params
        in
        let result_arity = Function_decl.return decl in
        let poll_attribute =
          Poll_attribute.from_lambda (Function_decl.poll_attribute decl)
        in
        let zero_alloc_attribute =
          Zero_alloc_attribute.from_lambda
            (Function_decl.zero_alloc_attribute decl)
        in
        let cost_metrics = Cost_metrics.zero in
        let dbg = Debuginfo.from_location (Function_decl.loc decl) in
        let is_tupled =
          match Function_decl.kind decl with
          | Curried _ -> false
          | Tupled -> true
        in
        let metadata =
          Code_metadata.create code_id ~params_arity
            ~first_complex_local_param:
              (Function_decl.first_complex_local_param decl)
            ~param_modes ~result_arity ~result_types:Unknown
            ~result_mode:(Function_decl.result_mode decl)
            ~stub:(Function_decl.stub decl) ~inline:Never_inline
            ~zero_alloc_attribute ~poll_attribute
            ~is_a_functor:(Function_decl.is_a_functor decl)
            ~is_opaque:(Function_decl.is_opaque decl)
            ~recursive:(Function_decl.recursive decl)
            ~newer_version_of:None ~cost_metrics
            ~inlining_arguments:(Inlining_arguments.create ~round:0)
            ~dbg ~is_tupled ~is_my_closure_used:true
            ~inlining_decision:Recursive
            ~absolute_history:(Inlining_history.Absolute.empty compilation_unit)
            ~relative_history:Inlining_history.Relative.empty
            ~loopify:Never_loopify
        in
        let code = Code_or_metadata.create_metadata_only metadata in
        let approx =
          Value_approximation.Closure_approximation
            { code_id; function_slot; code; symbol = None }
        in
        Function_slot.Map.add function_slot approx approx_map)
      Function_slot.Map.empty func_decl_list
  in
  let acc, external_env, symbol_map =
    if can_be_lifted
    then
      Ident.Map.fold
        (fun ident function_slot (acc, env, symbol_map) ->
          let env, acc, symbol =
            declare_symbol_for_function_slot env acc ident function_slot
          in
          let approx =
            match Function_slot.Map.find function_slot approx_map with
            | Value_approximation.Closure_approximation
                { code_id; function_slot; code; symbol = _ } ->
              Value_approximation.Closure_approximation
                { code_id; function_slot; code; symbol = Some symbol }
            | _ -> assert false
            (* see above *)
          in
          let acc = Acc.add_symbol_approximation acc symbol approx in
          acc, env, Function_slot.Map.add function_slot symbol symbol_map)
        function_slots_from_idents
        (acc, external_env, Function_slot.Map.empty)
    else acc, external_env, Function_slot.Map.empty
  in
  let acc, (approximations, function_code_ids_in_order) =
    List.fold_left
      (fun (acc, (by_function_slot, function_code_ids_in_order)) function_decl ->
        let code_id =
          Function_slot.Map.find
            (Function_decl.function_slot function_decl)
            function_code_ids
        in
        let _, _, acc, approxs_and_code_ids =
          Acc.measure_cost_metrics acc ~f:(fun acc ->
              close_one_function acc ~code_id ~external_env ~by_function_slot
                ~function_code_ids:function_code_ids_in_order function_decl
                ~has_lifted_closure:can_be_lifted ~value_slots_from_idents
                ~function_slots_from_idents ~approx_map function_declarations)
        in
        acc, approxs_and_code_ids)
      (acc, (Function_slot.Map.empty, []))
      func_decl_list
  in
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  let funs =
    function_code_ids_in_order |> List.rev |> Function_slot.Lmap.of_list
    |> Function_slot.Lmap.map
         (fun code_id : Function_declarations.code_id_in_function_declaration ->
           Code_id { code_id; only_full_applications = false })
  in
  let function_decls = Function_declarations.create funs in
  let value_slots =
    Ident.Map.fold
      (fun id value_slot map ->
        let kind = Value_slot.kind value_slot in
        let external_simple, kind' =
          find_simple_from_id_with_kind external_env id
        in
        if not (K.equal kind (K.With_subkind.kind kind'))
        then
          Misc.fatal_errorf "Value slot kinds %a and %a don't match for slot %a"
            K.print kind K.print
            (K.With_subkind.kind kind')
            Value_slot.print value_slot;
        (* We're sure [external_simple] is a variable since
           [value_slot_from_idents] has already filtered constants and symbols
           out. *)
        Value_slot.Map.add value_slot external_simple map)
      value_slots_from_idents Value_slot.Map.empty
  in
  let approximations =
    Function_slot.Map.mapi
      (fun function_slot code ->
        let code_id =
          Code_metadata.code_id (Code_or_metadata.code_metadata code)
        in
        Value_approximation.Closure_approximation
          { code_id; function_slot; code; symbol = None })
      approximations
  in
  let set_of_closures =
    Set_of_closures.create ~value_slots
      (Alloc_mode.For_allocations.from_lambda
         (Function_decls.alloc_mode function_declarations)
         ~current_region)
      function_decls
  in
  let acc =
    Acc.add_set_of_closures_offsets ~is_phantom:false acc set_of_closures
  in
  if can_be_lifted
  then
    let symbols_with_approx =
      Function_slot.Lmap.mapi
        (fun function_slot _ ->
          let sym = Function_slot.Map.find function_slot symbol_map in
          let approx =
            match Function_slot.Map.find function_slot approximations with
            | Value_approximation.Closure_approximation
                { code_id; function_slot; code; symbol = _ } ->
              Value_approximation.Closure_approximation
                { code_id; function_slot; code; symbol = Some sym }
            | _ -> assert false
            (* see above *)
          in
          sym, approx)
        funs
    in
    let symbols = Function_slot.Lmap.map fst symbols_with_approx in
    let acc = Acc.add_lifted_set_of_closures ~symbols ~set_of_closures acc in
    acc, Lifted symbols_with_approx
  else acc, Dynamic (set_of_closures, approximations)

let close_let_rec acc env ~function_declarations
    ~(body : Acc.t -> Env.t -> Expr_with_acc.t) ~current_region =
  let current_region =
    Option.map (fun region -> fst (Env.find_var env region)) current_region
  in
  let env =
    List.fold_right
      (fun decl env ->
        let id = Function_decl.let_rec_ident decl in
        let env, _var =
          Env.add_var_like env id User_visible K.With_subkind.any_value
        in
        env)
      function_declarations env
  in
  let fun_vars_map, ident_map =
    List.fold_left
      (fun (fun_vars_map, ident_map) decl ->
        let ident = Function_decl.let_rec_ident decl in
        let ident_duid = Function_decl.let_rec_debug_uid decl in
        let fun_var =
          VB.create (fst (Env.find_var env ident)) ident_duid Name_mode.normal
        in
        let function_slot = Function_decl.function_slot decl in
        ( Function_slot.Map.add function_slot fun_var fun_vars_map,
          Function_slot.Map.add function_slot ident ident_map ))
      (Function_slot.Map.empty, Function_slot.Map.empty)
      function_declarations
  in
  let alloc_mode =
    (* The closure allocation mode must be the same for all closures in the set
       of closures. *)
    List.fold_left
      (fun (alloc_mode : Lambda.locality_mode option) function_decl ->
        match alloc_mode, Function_decl.closure_alloc_mode function_decl with
        | None, alloc_mode -> Some alloc_mode
        | Some Alloc_heap, Alloc_heap | Some Alloc_local, Alloc_local ->
          alloc_mode
        | Some Alloc_heap, Alloc_local | Some Alloc_local, Alloc_heap ->
          Misc.fatal_errorf
            "let-rec group of [lfunction] declarations have inconsistent alloc \
             modes:@ %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Function_slot.print)
            (List.map Function_decl.function_slot function_declarations))
      None function_declarations
  in
  let alloc_mode =
    match alloc_mode with
    | Some alloc_mode -> alloc_mode
    | None ->
      Misc.fatal_error "let-rec group of [lfunction] declarations is empty"
  in
  let acc, closed_functions =
    close_functions acc env
      (Function_decls.create function_declarations alloc_mode)
      ~current_region
  in
  match closed_functions with
  | Lifted symbols ->
    let acc, env =
      Function_slot.Lmap.fold
        (fun function_slot (symbol, approx) (acc, env) ->
          let ident = Function_slot.Map.find function_slot ident_map in
          let env =
            Env.add_simple_to_substitute env ident (Simple.symbol symbol)
              K.With_subkind.any_value
          in
          Acc.add_symbol_approximation acc symbol approx, env)
        symbols (acc, env)
    in
    body acc env
  | Dynamic (set_of_closures, approximations) ->
    let generated_closures =
      Function_slot.Set.diff
        (Function_slot.Map.keys
           (Function_declarations.funs
              (Set_of_closures.function_decls set_of_closures)))
        (Function_slot.Map.keys fun_vars_map)
    in
    let fun_vars_map =
      Function_slot.Set.fold
        (fun function_slot fun_vars_map ->
          let fun_var =
            VB.create
              (Variable.create "generated" K.value)
              Flambda_debug_uid.none Name_mode.normal
          in
          Function_slot.Map.add function_slot fun_var fun_vars_map)
        generated_closures fun_vars_map
    in
    let bound_vars =
      List.map
        (fun (function_slot, _) ->
          Function_slot.Map.find function_slot fun_vars_map)
        (Function_declarations.funs_in_order
           (Set_of_closures.function_decls set_of_closures)
        |> Function_slot.Lmap.bindings)
    in
    let env =
      Function_slot.Map.fold
        (fun function_slot fun_var env ->
          let approx = Function_slot.Map.find function_slot approximations in
          Env.add_var_approximation env (VB.var fun_var) approx)
        fun_vars_map env
    in
    let acc, body = body acc env in
    let named = Named.create_set_of_closures set_of_closures in
    Let_with_acc.create acc
      (Bound_pattern.set_of_closures bound_vars)
      named ~body

let wrap_partial_application acc env apply_continuation (apply : IR.apply)
    approx ~provided ~provided_arity ~missing_arity ~missing_param_modes
    ~result_arity ~arity ~first_complex_local_param ~result_mode =
  (* In case of partial application, creates a wrapping function from scratch to
     allow inlining and lifting *)
  let wrapper_id = Ident.create_local ("partial_" ^ Ident.name apply.func) in
  let wrapper_id_duid = Flambda_debug_uid.none in
  (* CR sspies: In the future, improve the debugging UIDs here if possible. *)
  let function_slot =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name:(Ident.name wrapper_id) ~is_always_immediate:false K.value
  in
  let num_provided = Flambda_arity.num_params provided_arity in
  let missing_arity_and_param_modes =
    let missing_arity = Flambda_arity.unarize missing_arity in
    if List.compare_lengths missing_arity missing_param_modes <> 0
    then
      Misc.fatal_errorf
        "Mismatch between missing arity and missing param modes@ when wrapping \
         partial application of %a in [Closure_conversion]@ (location: %s):@ \
         provided_arity = %a@ missing_arity (unarized) = (%a)@ \
         missing_param_modes = (%a)"
        Ident.print apply.func
        (Debuginfo.Scoped_location.string_of_scoped_location
           ~include_zero_alloc:false apply.loc)
        Flambda_arity.print provided_arity
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Flambda_kind.With_subkind.print)
        missing_arity
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Alloc_mode.For_types.print)
        missing_param_modes
    else List.combine missing_arity missing_param_modes
  in
  let params =
    List.mapi
      (fun n (kind, mode) : Function_decl.param ->
        { name = Ident.create_local ("param" ^ string_of_int (num_provided + n));
          debug_uid = Flambda_debug_uid.none;
          kind;
          attributes = Lambda.default_param_attribute;
          mode = Alloc_mode.For_types.to_lambda mode
        })
      missing_arity_and_param_modes
  in
  let params_arity = missing_arity in
  let return_continuation = Continuation.create ~sort:Return () in
  let exn_continuation =
    IR.{ exn_handler = Continuation.create (); extra_args = [] }
  in
  let all_args =
    provided @ List.map (fun (p : Function_decl.param) -> IR.Var p.name) params
  in
  let contains_no_escaping_local_allocs =
    match (result_mode : Lambda.locality_mode) with
    | Alloc_heap -> true
    | Alloc_local -> false
  in
  let my_region =
    if contains_no_escaping_local_allocs
    then None
    else Some (Ident.create_local "my_region")
  in
  let my_ghost_region =
    if contains_no_escaping_local_allocs
    then None
    else Some (Ident.create_local "my_ghost_region")
  in
  let fbody acc env =
    close_exact_or_unknown_apply acc env
      { apply with
        kind = Function;
        args = all_args;
        args_arity = arity;
        continuation = return_continuation;
        exn_continuation;
        inlined = Lambda.Default_inlined;
        mode = result_mode;
        return_arity = result_arity;
        region = my_region;
        ghost_region = my_ghost_region
      }
      (Some approx) ~replace_region:None
  in
  let attr =
    Lambda.
      { inline = Default_inline;
        specialise = Default_specialise;
        local = Default_local;
        zero_alloc = Default_zero_alloc;
        loop = Default_loop;
        is_a_functor = false;
        is_opaque = false;
        stub = true;
        poll = Default_poll;
        tmc_candidate = false;
        may_fuse_arity = true;
        unbox_return = false
      }
  in
  let free_idents_of_body =
    List.fold_left
      (fun ids -> function
        | IR.Var id -> Ident.Set.add id ids
        | IR.Const _ -> ids)
      (Ident.Set.singleton apply.func)
      all_args
  in
  let closure_alloc_mode, first_complex_local_param =
    if num_provided <= first_complex_local_param
    then Lambda.alloc_heap, first_complex_local_param - num_provided
    else Lambda.alloc_local, 0
  in
  if not (Lambda.sub_locality_mode closure_alloc_mode apply.IR.mode)
  then
    (* This can happen in a dead GADT match case. *)
    ( acc,
      Expr.create_invalid
        (Partial_application_mode_mismatch_in_lambda
           (Debuginfo.from_location apply.loc)) )
  else
    let function_declarations =
      [ Function_decl.create ~let_rec_ident:(Some wrapper_id)
          ~let_rec_uid:wrapper_id_duid ~function_slot
          ~kind:
            (Lambda.Curried
               { nlocal =
                   Flambda_arity.num_params missing_arity
                   - first_complex_local_param
               })
          ~params ~params_arity ~removed_params:Ident.Set.empty
          ~return:result_arity ~calling_convention:Normal_calling_convention
          ~return_continuation ~exn_continuation ~my_region ~my_ghost_region
          ~body:fbody ~attr ~loc:apply.loc ~free_idents_of_body
          ~closure_alloc_mode ~first_complex_local_param ~result_mode
          Recursive.Non_recursive ]
    in
    let body acc env =
      let arg = find_simple_from_id env wrapper_id in
      let acc, apply_cont =
        Apply_cont_with_acc.create acc
          ~args_approx:[find_value_approximation env arg]
          apply_continuation ~args:[arg] ~dbg:Debuginfo.none
      in
      Expr_with_acc.create_apply_cont acc apply_cont
    in
    close_let_rec acc env ~function_declarations ~body
      ~current_region:apply.region

let wrap_over_application acc env full_call (apply : IR.apply) ~remaining
    ~remaining_arity ~result_mode =
  let wrapper_cont = Continuation.create () in
  let returned_func = Variable.create "func" K.value in
  let returned_func_duid = Flambda_debug_uid.none in
  (* See comments in [Simplify_common.split_direct_over_application] about this
     code for handling local allocations. *)
  let apply_return_continuation =
    Apply.Result_continuation.Return apply.continuation
  in
  let acc, remaining = find_simples acc env remaining in
  let apply_dbg = Debuginfo.from_location apply.loc in
  let needs_region =
    match apply.mode, (result_mode : Lambda.locality_mode) with
    | Alloc_heap, Alloc_local ->
      let over_app_region = Variable.create "over_app_region" K.region in
      let over_app_ghost_region =
        Variable.create "over_app_ghost_region" K.region
      in
      Some (over_app_region, over_app_ghost_region, Continuation.create ())
    | Alloc_heap, Alloc_heap | Alloc_local, _ -> None
  in
  let apply_region, apply_ghost_region =
    match needs_region with
    | None ->
      ( Option.map (fun region -> fst (Env.find_var env region)) apply.region,
        Option.map
          (fun region -> fst (Env.find_var env region))
          apply.ghost_region )
    | Some (region, ghost_region, _) -> Some region, Some ghost_region
  in
  let perform_over_application acc =
    let acc, apply_exn_continuation =
      close_exn_continuation acc env apply.exn_continuation
    in
    let inlined = Inlined_attribute.from_lambda apply.inlined in
    (* Keeping the inlining attributes matches the behaviour of simplify *)
    let probe = Probe.from_lambda apply.probe in
    let position =
      match apply.region_close with
      | Rc_normal | Rc_close_at_apply -> Apply.Position.Normal
      | Rc_nontail -> Apply.Position.Nontail
    in
    let call_kind =
      Call_kind.indirect_function_call_unknown_arity
        (Alloc_mode.For_applications.from_lambda apply.mode
           ~current_region:apply_region ~current_ghost_region:apply_ghost_region)
    in
    let continuation =
      match needs_region with
      | None -> apply_return_continuation
      | Some (_, _, cont) -> Apply.Result_continuation.Return cont
    in
    let over_application =
      Apply.create
        ~callee:(Some (Simple.var returned_func))
        ~continuation apply_exn_continuation ~args:remaining
        ~args_arity:remaining_arity ~return_arity:apply.return_arity ~call_kind
        apply_dbg ~inlined
        ~inlining_state:(Inlining_state.default ~round:0)
        ~probe ~position
        ~relative_history:(Env.relative_history_from_scoped ~loc:apply.loc env)
    in
    match needs_region with
    | None -> Expr_with_acc.create_apply acc over_application
    | Some (region, ghost_region, after_over_application) ->
      let over_application_results =
        List.mapi
          (fun i kind ->
            let result_var =
              Variable.create
                ("result" ^ string_of_int i)
                (Flambda_kind.With_subkind.kind kind)
            in
            let result_var_duid = Flambda_debug_uid.none in
            BP.create result_var kind result_var_duid)
          (Flambda_arity.unarized_components apply.return_arity)
      in
      let handler acc =
        let acc, call_return_continuation =
          let acc, apply_cont_expr =
            Apply_cont_with_acc.create acc apply.continuation
              ~args:(List.map BP.simple over_application_results)
              ~dbg:apply_dbg
          in
          acc, Expr.create_apply_cont apply_cont_expr
        in
        let acc, body =
          Let_with_acc.create acc
            (Bound_pattern.singleton
               (Bound_var.create
                  (Variable.create "unit" K.value)
                  Flambda_debug_uid.none Name_mode.normal))
            (Named.create_prim
               (Unary (End_region { ghost = true }, Simple.var ghost_region))
               apply_dbg)
            ~body:call_return_continuation
        in
        Let_with_acc.create acc
          (Bound_pattern.singleton
             (Bound_var.create
                (Variable.create "unit" K.value)
                Flambda_debug_uid.none Name_mode.normal))
          (Named.create_prim
             (Unary (End_region { ghost = false }, Simple.var region))
             apply_dbg)
          ~body
      in
      Let_cont_with_acc.build_non_recursive acc after_over_application
        ~handler_params:(Bound_parameters.create over_application_results)
        ~handler
        ~body:(fun acc -> Expr_with_acc.create_apply acc over_application)
        ~is_exn_handler:false ~is_cold:false
  in
  let body =
    full_call wrapper_cont ~region:apply_region ~ghost_region:apply_ghost_region
  in
  let acc, both_applications =
    Let_cont_with_acc.build_non_recursive acc wrapper_cont
      ~handler_params:
        ([BP.create returned_func K.With_subkind.any_value returned_func_duid]
        |> Bound_parameters.create)
      ~handler:perform_over_application ~body ~is_exn_handler:false
      ~is_cold:false
  in
  match needs_region with
  | None -> acc, both_applications
  | Some (region, ghost_region, _) ->
    let region_duid = Flambda_debug_uid.none in
    let ghost_region_duid = Flambda_debug_uid.none in
    let acc, body =
      Let_with_acc.create acc
        (Bound_pattern.singleton
           (Bound_var.create ghost_region ghost_region_duid Name_mode.normal))
        (Named.create_prim
           (Variadic (Begin_region { ghost = true }, []))
           apply_dbg)
        ~body:both_applications
    in
    Let_with_acc.create acc
      (Bound_pattern.singleton
         (Bound_var.create region region_duid Name_mode.normal))
      (Named.create_prim
         (Variadic (Begin_region { ghost = false }, []))
         apply_dbg)
      ~body

type call_args_split =
  | Exact of IR.simple list
  | Partial_app of
      { provided : IR.simple list;
        provided_arity : [`Complex] Flambda_arity.t;
        missing_arity : [`Complex] Flambda_arity.t;
        missing_param_modes : Alloc_mode.For_types.t list;
        result_arity : [`Unarized] Flambda_arity.t
      }
  | Over_app of
      { full : IR.simple list;
        provided_arity : [`Complex] Flambda_arity.t;
        remaining : IR.simple list;
        remaining_arity : [`Complex] Flambda_arity.t;
        result_mode : Lambda.locality_mode
      }

let close_apply acc env (apply : IR.apply) : Expr_with_acc.t =
  let callee = find_simple_from_id env apply.func in
  let approx = find_value_approximation_through_symbol acc env callee in
  let code_info =
    match approx with
    | Closure_approximation { code; _ } ->
      let metadata = Code_or_metadata.code_metadata code in
      Some
        ( Code_metadata.params_arity metadata,
          Code_metadata.result_arity metadata,
          Code_metadata.is_tupled metadata,
          Code_metadata.param_modes metadata,
          Code_metadata.first_complex_local_param metadata,
          Code_metadata.result_mode metadata )
    | Value_unknown -> None
    | Value_symbol _ | Value_const _ | Block_approximation _ ->
      if Flambda_features.check_invariants ()
      then
        Misc.fatal_errorf
          "Unexpected approximation for callee %a in [Closure_conversion], \
           expected a closure approximation."
          Simple.print callee
      else None
  in
  match code_info with
  | None -> close_exact_or_unknown_apply acc env apply None ~replace_region:None
  | Some
      ( params_arity,
        result_arity,
        is_tupled,
        param_modes,
        first_complex_local_param,
        result_mode ) -> (
    let split_args =
      let non_unarized_arity, arity =
        let arity =
          if is_tupled
          then
            Flambda_arity.create_singletons
              [ Flambda_kind.With_subkind.block Tag.zero
                  (Flambda_arity.unarize params_arity) ]
          else params_arity
        in
        arity, Flambda_arity.unarize arity
      in
      let split args arity =
        let rec cut n l =
          if n <= 0
          then [], l
          else
            match l with
            | [] -> [], []
            | h :: t ->
              let before, after = cut (n - 1) t in
              h :: before, after
        in
        let args_l = Flambda_arity.num_params apply.args_arity in
        let arity_l = Flambda_arity.num_params non_unarized_arity in
        if args_l = arity_l
        then Exact args
        else if args_l < arity_l
        then
          let missing_arity =
            Flambda_arity.partially_apply non_unarized_arity
              ~num_non_unarized_params_provided:args_l
          in
          let _provided_modes, missing_param_modes =
            cut (List.length args) param_modes
          in
          Partial_app
            { provided = args;
              provided_arity = apply.args_arity;
              missing_arity;
              missing_param_modes;
              result_arity
            }
        else
          let full, remaining = cut (List.length arity) args in
          let remaining_arity =
            Flambda_arity.partially_apply apply.args_arity
              ~num_non_unarized_params_provided:arity_l
          in
          Over_app
            { full;
              provided_arity = non_unarized_arity;
              remaining;
              remaining_arity;
              result_mode
            }
      in
      split apply.args arity
    in
    match split_args with
    | Exact args ->
      close_exact_or_unknown_apply acc env
        { apply with args; continuation = apply.continuation }
        (Some approx) ~replace_region:None
    | Partial_app
        { provided;
          provided_arity;
          missing_arity;
          missing_param_modes;
          result_arity
        } ->
      (match apply.inlined with
      | Always_inlined | Unroll _ ->
        Location.prerr_warning
          (Debuginfo.Scoped_location.to_location apply.loc)
          (Warnings.Inlining_impossible
             Inlining_helpers.(
               inlined_attribute_on_partial_application_msg Inlined))
      | Never_inlined | Hint_inlined | Default_inlined -> ());
      wrap_partial_application acc env apply.continuation apply approx ~provided
        ~provided_arity ~missing_arity ~missing_param_modes ~result_arity
        ~arity:params_arity ~first_complex_local_param ~result_mode
    | Over_app { full; provided_arity; remaining; remaining_arity; result_mode }
      ->
      let full_args_call apply_continuation ~region ~ghost_region acc =
        let replace_region =
          match region, ghost_region with
          | None, None -> None
          | Some region, Some ghost_region -> Some (region, ghost_region)
          | Some _, None | None, Some _ -> Misc.fatal_error "Mismatched regions"
        in
        close_exact_or_unknown_apply acc env
          { apply with
            args = full;
            args_arity = provided_arity;
            continuation = apply_continuation;
            mode = result_mode;
            return_arity =
              Flambda_arity.create_singletons
                [Flambda_kind.With_subkind.any_value]
          }
          (Some approx) ~replace_region
      in
      wrap_over_application acc env full_args_call apply ~remaining
        ~remaining_arity ~result_mode)

module CIS = Code_id_or_symbol
module GroupMap = Numbers.Int.Map
module SCC = Strongly_connected_components.Make (Numbers.Int)

let bind_code_and_sets_of_closures all_code sets_of_closures acc body =
  let fresh_group_id =
    let i = ref 0 in
    fun () ->
      let n = !i in
      incr i;
      n
  in
  (* CR gbury: We assume that no code_ids are deleted later, but even if that
     were to happen, we would only get an over-approximation of the slot offsets
     constraints in the [slot_offsets] field of the acc (which is only used in
     classic mode), and that should be benign. *)
  let acc, group_to_bound_consts, symbol_to_groups =
    Code_id.Lmap.fold
      (fun code_id code (acc, g2c, s2g) ->
        let id = fresh_group_id () in
        let acc = Acc.add_offsets_from_code acc code_id in
        let bound = Bound_static.Pattern.code code_id in
        let const = Static_const_or_code.create_code code in
        ( acc,
          GroupMap.add id (bound, const) g2c,
          CIS.Map.add (CIS.create_code_id code_id) id s2g ))
      all_code
      (acc, GroupMap.empty, CIS.Map.empty)
  in
  let group_to_bound_consts, symbol_to_groups =
    List.fold_left
      (fun (g2c, s2g) (symbols, set_of_closures) ->
        let id = fresh_group_id () in
        let bound = Bound_static.Pattern.set_of_closures symbols in
        let const =
          Static_const_or_code.create_static_const
            (Static_const.set_of_closures set_of_closures)
        in
        ( GroupMap.add id (bound, const) g2c,
          Function_slot.Lmap.fold
            (fun _function_slot symbol s2g ->
              CIS.Map.add (CIS.create_symbol symbol) id s2g)
            symbols s2g ))
      (group_to_bound_consts, symbol_to_groups)
      sets_of_closures
  in
  let graph =
    GroupMap.map
      (fun (_bound, const) ->
        let free_names = Static_const_or_code.free_names const in
        let deps =
          Code_id.Set.fold
            (fun code_id deps ->
              match
                CIS.Map.find (CIS.create_code_id code_id) symbol_to_groups
              with
              | exception Not_found -> deps
              | id -> Numbers.Int.Set.add id deps)
            (Name_occurrences.code_ids free_names)
            Numbers.Int.Set.empty
        in
        Symbol.Set.fold
          (fun symbol deps ->
            match CIS.Map.find (CIS.create_symbol symbol) symbol_to_groups with
            | exception Not_found -> deps
            | id -> Numbers.Int.Set.add id deps)
          (Name_occurrences.symbols free_names)
          deps)
      group_to_bound_consts
  in
  let components = SCC.connected_components_sorted_from_roots_to_leaf graph in
  (* Empirically, our SCC seems to perform a stable sort, so this assumes that
     [components] preserves the original order as much as possible. *)
  Array.fold_left
    (fun (acc, body) (component : SCC.component) ->
      let group_ids =
        match component with
        | No_loop group_id -> [group_id]
        | Has_loop group_ids -> List.sort Int.compare group_ids
      in
      let bound_static, static_consts =
        List.map
          (fun group_id ->
            let bound_symbol, static_const =
              try GroupMap.find group_id group_to_bound_consts
              with Not_found ->
                Misc.fatal_errorf "Unbound static consts group ID %d" group_id
            in
            bound_symbol, static_const)
          group_ids
        |> List.split
      in
      let defining_expr =
        Static_const_group.create static_consts |> Named.create_static_consts
      in
      Let_with_acc.create acc
        (Bound_pattern.static (Bound_static.create bound_static))
        defining_expr ~body)
    (acc, body) components

let wrap_final_module_block acc env ~program ~prog_return_cont
    ~module_block_size_in_words ~return_cont ~module_symbol =
  let module_block_var = Variable.create "module_block" K.value in
  let module_block_var_duid = Flambda_debug_uid.none in
  let module_block_tag = Tag.Scannable.zero in
  let load_fields_body acc =
    let env =
      match Acc.continuation_known_arguments ~cont:prog_return_cont acc with
      | Some [approx] -> Env.add_var_approximation env module_block_var approx
      | None | Some ([] | _ :: _) -> env
    in
    let module_block_simple =
      let simple_var = Simple.var module_block_var in
      match find_value_approximation env simple_var with
      | Value_approximation.Value_symbol s -> Simple.symbol s
      | _ -> simple_var
    in
    let field_vars =
      List.init module_block_size_in_words (fun pos ->
          let pos_str = string_of_int pos in
          ( pos,
            Variable.create ("field_" ^ pos_str) K.value
            (* CR mixed-modules: Find the right kind from the module block
               shape *),
            Flambda_debug_uid.none ))
    in
    let acc, body =
      let static_const : Static_const.t =
        let field_vars =
          List.map
            (fun (_, var, _) ->
              Simple.With_debuginfo.create (Simple.var var) Debuginfo.none)
            field_vars
        in
        Static_const.block module_block_tag Immutable Value_only field_vars
      in
      let acc, apply_cont =
        (* Module initialisers return unit, but since that is taken care of
           during Cmm generation, we can instead "return" [module_symbol] here
           to ensure that its associated "let symbol" doesn't get deleted. *)
        Apply_cont_with_acc.create acc return_cont
          ~args:[Simple.symbol module_symbol]
          ~dbg:Debuginfo.none
      in
      let acc, return = Expr_with_acc.create_apply_cont acc apply_cont in
      let bound_static =
        Bound_static.singleton (Bound_static.Pattern.block_like module_symbol)
      in
      let named =
        Named.create_static_consts
          (Static_const_group.create
             [Static_const_or_code.create_static_const static_const])
      in
      Let_with_acc.create acc
        (Bound_pattern.static bound_static)
        named ~body:return
    in
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known (Targetint_31_63.of_int module_block_size_in_words);
          field_kind = Any_value
        }
    in
    List.fold_left
      (fun (acc, body) (pos, var, var_duid) ->
        let var = VB.create var var_duid Name_mode.normal in
        let pat = Bound_pattern.singleton var in
        let pos = Targetint_31_63.of_int pos in
        let block = module_block_simple in
        match simplify_block_load acc env ~block ~field:pos with
        | Unknown | Not_a_block | Block_but_cannot_simplify _ ->
          let named =
            Named.create_prim
              (Unary
                 ( Block_load
                     { kind = block_access; mut = Immutable; field = pos },
                   block ))
              Debuginfo.none
          in
          Let_with_acc.create acc pat named ~body
        | Field_contents sim ->
          let named = Named.create_simple sim in
          Let_with_acc.create acc pat named ~body)
      (acc, body) (List.rev field_vars)
  in
  let load_fields_handler_param =
    [BP.create module_block_var K.With_subkind.any_value module_block_var_duid]
    |> Bound_parameters.create
  in
  (* This binds the return continuation that is free (or, at least, not bound)
     in the incoming code. The handler for the continuation receives a tuple
     with fields indexed from zero to [module_block_size_in_words]. The handler
     extracts the fields; the variables bound to such fields are then used to
     define the module block symbol. *)
  let body acc = program acc env in
  Let_cont_with_acc.build_non_recursive acc prog_return_cont
    ~handler_params:load_fields_handler_param ~handler:load_fields_body ~body
    ~is_exn_handler:false ~is_cold:false

let close_program (type mode) ~(mode : mode Flambda_features.mode) ~big_endian
    ~cmx_loader ~compilation_unit ~module_block_size_in_words ~program
    ~prog_return_cont ~exn_continuation ~toplevel_my_region
    ~toplevel_my_ghost_region : mode close_program_result =
  let env = Env.create ~big_endian in
  let module_symbol =
    Symbol.create_wrapped
      (Flambda2_import.Symbol.for_compilation_unit compilation_unit)
  in
  let return_cont = Continuation.create ~sort:Toplevel_return () in
  let env, toplevel_my_region =
    Env.add_var_like env toplevel_my_region Not_user_visible
      Flambda_kind.With_subkind.region
  in
  let env, toplevel_my_ghost_region =
    Env.add_var_like env toplevel_my_ghost_region Not_user_visible
      Flambda_kind.With_subkind.region
  in
  let acc = Acc.create ~cmx_loader in
  let acc, body =
    wrap_final_module_block acc env ~program ~prog_return_cont
      ~module_block_size_in_words ~return_cont ~module_symbol
  in
  let module_block_approximation =
    match Acc.continuation_known_arguments ~cont:prog_return_cont acc with
    (* Module symbol may be rebuilt from a lifted block *)
    | Some [Value_approximation.Value_symbol s] ->
      Acc.find_symbol_approximation acc s
    | Some [approx] -> approx
    | _ -> Value_approximation.Value_unknown
  in
  let acc, body =
    bind_code_and_sets_of_closures (Acc.code acc)
      (Acc.lifted_sets_of_closures acc)
      acc body
  in
  (* We must make sure there is always an outer [Let_symbol] binding so that
     lifted constants not in the scope of any other [Let_symbol] binding get put
     into the term and not dropped. Adding this extra binding, which will
     actually be removed by the simplifier, avoids a special case. *)
  let acc =
    match Acc.declared_symbols acc with
    | _ :: _ -> acc
    | [] ->
      (* CR vlaviron/mshinwell: Maybe this could use an empty array.
         Furthermore, can this hack be removed? *)
      let acc, (_sym : Symbol.t) =
        register_const0 acc
          (Static_const.block Tag.Scannable.zero Immutable Value_only [])
          "first_const"
      in
      acc
  in
  let symbols_approximations =
    Symbol.Map.add module_symbol module_block_approximation
      (Acc.symbol_approximations acc)
  in
  let acc, body =
    List.fold_left
      (fun (acc, body) (symbol, static_const) ->
        let bound_static =
          Bound_static.singleton (Bound_static.Pattern.block_like symbol)
        in
        let defining_expr =
          Static_const_group.create
            [Static_const_or_code.create_static_const static_const]
          |> Named.create_static_consts
        in
        Let_with_acc.create acc
          (Bound_pattern.static bound_static)
          defining_expr ~body)
      (acc, body) (Acc.declared_symbols acc)
  in
  if Option.is_some (Acc.top_closure_info acc)
  then
    Misc.fatal_error "Information on nested closures should be empty at the end";
  let get_code_metadata code_id =
    Code_id.Map.find code_id (Acc.code_map acc) |> Code.code_metadata
  in
  let code_slot_offsets = Acc.code_slot_offsets acc in
  match mode with
  | Normal ->
    (* CR chambart/gbury: we could probably get away with not computing some of
       the fields of the Acc, when not in classic mode. For instance, the slot
       offsets constraints accumulation is not needed in "normal" mode. *)
    let unit =
      Flambda_unit.create ~return_continuation:return_cont ~exn_continuation
        ~toplevel_my_region ~toplevel_my_ghost_region ~body ~module_symbol
        ~used_value_slots:Unknown
    in
    { unit; code_slot_offsets; metadata = Normal }
  | Classic ->
    let all_code =
      Exported_code.add_code (Acc.code_map acc)
        ~keep_code:(fun _ -> false)
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
    in
    let Slot_offsets.{ used_value_slots; exported_offsets } =
      let used_slots =
        let free_names = Acc.free_names acc in
        Slot_offsets.
          { function_slots_in_normal_projections =
              Name_occurrences.function_slots_in_normal_projections free_names;
            all_function_slots = Name_occurrences.all_function_slots free_names;
            value_slots_in_normal_projections =
              Name_occurrences.value_slots_in_normal_projections free_names;
            all_value_slots = Name_occurrences.all_value_slots free_names
          }
      in
      Slot_offsets.finalize_offsets (Acc.slot_offsets acc) ~get_code_metadata
        ~used_slots
    in
    let reachable_names, cmx =
      Flambda_cmx.prepare_cmx_from_approx ~approxs:symbols_approximations
        ~module_symbol ~exported_offsets ~used_value_slots all_code
    in
    let unit =
      Flambda_unit.create ~return_continuation:return_cont ~exn_continuation
        ~toplevel_my_region ~toplevel_my_ghost_region ~body ~module_symbol
        ~used_value_slots:(Known used_value_slots)
    in
    { unit;
      code_slot_offsets;
      metadata = Classic (all_code, reachable_names, cmx, exported_offsets)
    }
