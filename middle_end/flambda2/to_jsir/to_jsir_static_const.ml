(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Jsoo_imports.Import

let static_const_not_supported () =
  Misc.fatal_error "This static_const is not yet supported."

let bind_expr_to_symbol ~env ~res symbol expr =
  (* This should already be populated by To_jsir.let_expr_normal *)
  let jvar, res = To_jsir_env.get_symbol_exn env ~res symbol in
  env, To_jsir_result.add_instr_exn res (Jsir.Let (jvar, expr))

let const_or_var ~env ~res ~symbol ~to_jsir_const (x : 'a Or_variable.t) =
  match x with
  | Const c -> bind_expr_to_symbol ~env ~res symbol (Constant (to_jsir_const c))
  | Var (v, dbg) ->
    To_jsir_result.with_debuginfo_exn res dbg ~f:(fun res ->
        (* This should already be populated by To_jsir.let_expr_normal *)
        let symbol_var, res = To_jsir_env.get_symbol_exn env ~res symbol in
        let value_var = To_jsir_env.get_var_exn env v in
        env, To_jsir_result.add_instr_exn res (Assign (symbol_var, value_var)))

let float32_to_jsir_const float32 : Jsir.constant =
  Float32
    (Numeric_types.Float32_by_bit_pattern.to_bits float32 |> Int64.of_int32)

let float_to_jsir_const float : Jsir.constant =
  Float32 (Numeric_types.Float_by_bit_pattern.to_bits float)

let int32_to_jsir_const int32 : Jsir.constant = Int32 int32

let int64_to_jsir_const int64 : Jsir.constant = Int64 int64

let nativeint_to_jsir_const nativeint : Jsir.constant =
  Int32 (Targetint_32_64.to_int32 nativeint)

let block_or_array ~env ~res ~symbol ~tag ~mut ~array_or_not fields =
  let fields = List.map Simple.With_debuginfo.simple fields in
  let all_consts = List.for_all Simple.is_const fields in
  if all_consts && not (Mutability.is_mutable mut)
  then
    let values =
      ListLabels.map fields ~f:(fun x ->
          match Simple.must_be_const x with
          | Some const -> To_jsir_shared.reg_width_const const
          | None ->
            Misc.fatal_error
              "Found a non-constant in a Simple.t, even though we check that \
               all values are constants")
      |> Array.of_list
    in
    bind_expr_to_symbol ~env ~res symbol
      (Constant (Tuple (Tag.to_int tag, values, array_or_not)))
  else
    let expr, env, res = To_jsir_shared.block ~env ~res ~tag ~mut ~fields in
    bind_expr_to_symbol ~env ~res symbol expr

let immutable_float_block_or_array ~env ~res ~symbol values ~array_or_not =
  let all_consts = List.for_all Or_variable.is_const values in
  if all_consts
  then
    let f (x : 'a Or_variable.t) =
      match x with
      | Const c -> Numeric_types.Float_by_bit_pattern.to_bits c
      | Var _ ->
        Misc.fatal_error
          "Found a variable in Or_variable.t, even though we check that all \
           values are constants"
    in
    let values = List.map f values |> Array.of_list in
    bind_expr_to_symbol ~env ~res symbol (Constant (Float_array values))
  else
    let values, res =
      List.fold_right
        (fun x (values, res) ->
          match (x : Numeric_types.Float_by_bit_pattern.t Or_variable.t) with
          | Const c ->
            let bits = Numeric_types.Float_by_bit_pattern.to_bits c in
            let var = Jsir.Var.fresh () in
            ( var :: values,
              To_jsir_result.add_instr_exn res
                (Let (var, Constant (Float bits))) )
          | Var (v, dbg) ->
            To_jsir_result.with_debuginfo_exn res dbg ~f:(fun res ->
                let var = To_jsir_env.get_var_exn env v in
                var :: values, res))
        values ([], res)
    in
    let values = Array.of_list values in
    let tag = Tag.double_array_tag |> Tag.to_int in
    let expr : Jsir.expr = Block (tag, values, array_or_not, Immutable) in
    bind_expr_to_symbol ~env ~res symbol expr

let block_like ~env ~res symbol (const : Static_const.t) =
  match const with
  | Set_of_closures _closures ->
    Misc.fatal_errorf
      "Cannot translate %a: expected a block-like static const, instead found \
       Set_of_closures"
      Static_const.print const
  | Block (tag, mut, _shape, fields) ->
    let tag = Tag.Scannable.to_tag tag in
    block_or_array ~env ~res ~symbol ~tag ~mut ~array_or_not:NotArray fields
  | Boxed_float32 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:float32_to_jsir_const value
  | Boxed_float value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:float_to_jsir_const value
  | Boxed_int32 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:int32_to_jsir_const value
  | Boxed_int64 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:int64_to_jsir_const value
  | Boxed_nativeint value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:nativeint_to_jsir_const value
  | Boxed_vec128 _ | Boxed_vec256 _ | Boxed_vec512 _ ->
    (* Need SIMD *)
    static_const_not_supported ()
  | Immutable_float_block values ->
    immutable_float_block_or_array ~env ~res ~symbol values
      ~array_or_not:NotArray
  | Immutable_float_array values ->
    immutable_float_block_or_array ~env ~res ~symbol values ~array_or_not:Array
  | Immutable_float32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int64_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_nativeint_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec128_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec256_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec512_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_value_array values ->
    block_or_array ~env ~res ~symbol ~tag:Tag.zero ~mut:Immutable
      ~array_or_not:Array values
  | Empty_array kind -> (
    match kind with
    | Values_or_immediates_or_naked_floats | Naked_float32s ->
      bind_expr_to_symbol ~env ~res symbol
        (Prim (Extern "caml_make_vect", [Pc (Int Targetint.zero); Pc Null]))
    | Unboxed_products | Naked_int32s | Naked_int64s | Naked_nativeints
    | Naked_vec128s | Naked_vec256s | Naked_vec512s ->
      (* No SIMD *)
      static_const_not_supported ())
  | Mutable_string { initial_value } ->
    ignore initial_value;
    static_const_not_supported ()
  | Immutable_string value ->
    bind_expr_to_symbol ~env ~res symbol (Constant (String value))

let prepare_code ~env ~res ~code_id code =
  let params_and_body = Code0.params_and_body code in
  Flambda.Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation:_
         ~exn_continuation:_
         bound_params
         ~body:_
         ~my_closure:_
         ~is_my_closure_used:_
         ~my_region:_
         ~my_ghost_region:_
         ~my_depth:_
         ~free_names_of_body:_
       ->
      let params =
        List.init (Bound_parameters.cardinal bound_params) (fun _ ->
            Jsir.Var.fresh ())
      in
      let res, addr = To_jsir_result.reserve_address res in
      let closure = Jsir.Var.fresh () in
      let env = To_jsir_env.add_code_id env code_id ~addr ~params ~closure in
      let free_names = Code0.free_names code in
      let function_slots = Name_occurrences.all_function_slots free_names in
      let value_slots = Name_occurrences.all_value_slots free_names in
      (* We create new variables that represent each function and value slots,
         if they don't exist already, and use them everywhere that the
         corresponding slot is used. We will make sure later (when translating
         [Set_of_closures]) that the closures or values representing these slots
         are bound to the correct variables. *)
      let env =
        Function_slot.Set.fold
          (fun slot env -> To_jsir_env.add_function_slot_if_not_found env slot)
          function_slots env
      in
      let env =
        Value_slot.Set.fold
          (fun slot env -> To_jsir_env.add_value_slot_if_not_found env slot)
          value_slots env
      in
      env, res)

let code ~env ~res ~translate_body ~code_id code =
  let params_and_body = Code0.params_and_body code in
  Flambda.Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         bound_params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_region:_
         ~my_ghost_region:_
         ~my_depth:_
         ~free_names_of_body:_
       ->
      (* This has already been populated by the first phase of the [Static] arm
         of [To_jsir.let_expr_normal] *)
      let ({ addr; params = params_jvar; closure } : To_jsir_env.code_id) =
        To_jsir_env.get_code_id_exn env code_id
      in
      let res = To_jsir_result.new_block_with_addr_exn res ~addr ~params:[] in
      let params_fvar =
        Bound_parameters.to_list bound_params |> List.map Bound_parameter.var
      in
      let env_with_params =
        List.fold_left2 To_jsir_env.add_var env params_fvar params_jvar
      in
      let env_with_params =
        To_jsir_env.set_my_closure env_with_params my_closure closure
      in
      let _env_with_params, res =
        (* Throw away the environment after translating the body *)
        translate_body
          ~env:
            (To_jsir_env.enter_function_body env_with_params
               ~return_continuation ~exn_continuation)
          ~res body
      in
      env, res)
