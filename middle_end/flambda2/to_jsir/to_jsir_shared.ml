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

let bind_expr_to_var' ~env ~res fvar expr =
  let jvar = Jsir.Var.fresh () in
  ( jvar,
    To_jsir_env.add_var env fvar jvar,
    To_jsir_result.add_instr_exn res (Jsir.Let (jvar, expr)) )

let bind_expr_to_var ~env ~res fvar expr =
  let _jvar, env, res = bind_expr_to_var' ~env ~res fvar expr in
  env, res

let target_ocaml_int_to_jsir_const targetint : Jsir.constant =
  let repr =
    Target_ocaml_int.to_targetint Thirty_two_no_gc_tag_bit targetint
    |> Targetint_32_64.repr
  in
  let targetint =
    match repr with
    | Int32 int32 -> Targetint.of_int32_exn int32
    | Int64 int64 -> Targetint.of_int64_exn int64
  in
  Int targetint

let float32_to_jsir_const float32 : Jsir.constant =
  (* Since float32s are just represented as normal floats in JSIR, we first
     convert to [Float_by_bit_pattern] and use its bit pattern. *)
  Numeric_types.Float32_by_bit_pattern.to_float float32
  |> Numeric_types.Float_by_bit_pattern.create
  |> Numeric_types.Float_by_bit_pattern.to_bits
  |> fun bits : Jsir.constant -> Float32 bits

let float_to_jsir_const float : Jsir.constant =
  Float (Numeric_types.Float_by_bit_pattern.to_bits float)

let int8_to_jsir_const int8 : Jsir.constant =
  Int (Targetint.of_int_exn (Numeric_types.Int8.to_int int8))

let int16_to_jsir_const int16 : Jsir.constant =
  Int (Targetint.of_int_exn (Numeric_types.Int16.to_int int16))

let int32_to_jsir_const int32 : Jsir.constant = Int32 int32

let int64_to_jsir_const int64 : Jsir.constant = Int64 int64

let nativeint_to_jsir_const nativeint : Jsir.constant =
  Int32 (Targetint_32_64.to_int32 nativeint)

let rec reg_width_const const : Jsir.constant =
  match Reg_width_const.descr const with
  | Naked_immediate targetint | Tagged_immediate targetint ->
    target_ocaml_int_to_jsir_const targetint
  | Naked_float32 float32 -> float32_to_jsir_const float32
  | Naked_float float -> float_to_jsir_const float
  | Naked_int8 int8 -> int8_to_jsir_const int8
  | Naked_int16 int16 -> int16_to_jsir_const int16
  | Naked_int32 int32 -> int32_to_jsir_const int32
  | Naked_int64 int64 -> int64_to_jsir_const int64
  | Naked_nativeint nativeint -> nativeint_to_jsir_const nativeint
  | Null -> Jsir.Null_
  | Poison (kind, name) ->
    reg_width_const
      (Reg_width_const.of_int_of_kind Thirty_two_no_gc_tag_bit kind
         (String.hash name))
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Naked_mask _ ->
    (* CR selee: SIMD *)
    Misc.fatal_errorf "Unsupported constant %a" Int_ids.Const.print const

let simple ~env ~res simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ -> To_jsir_env.get_var_exn env name, res)
    ~symbol:(fun symbol ~coercion:_ ->
      To_jsir_env.get_symbol_exn env ~res symbol)
    ~const:(fun const ->
      let var = Jsir.Var.fresh () in
      let expr = Jsir.Constant (reg_width_const const) in
      let res = To_jsir_result.add_instr_exn res (Let (var, expr)) in
      var, res)

let simples ~env ~res simples =
  List.fold_right
    (fun s (vars, res) ->
      let var, res = simple ~env ~res s in
      var :: vars, res)
    simples ([], res)

let bound_parameters ~env bound_params =
  (* The natural fold instead of [List.fold_left] to preserve order of
     parameters *)
  List.fold_right
    (fun bound_param (params, env) ->
      let var = Jsir.Var.fresh () in
      let env = To_jsir_env.add_var env (Bound_parameter.var bound_param) var in
      var :: params, env)
    (Bound_parameters.to_list bound_params)
    ([], env)

let repr_of_value_subkind
    (subkind : Flambda_kind.With_subkind.Non_null_value_subkind.t) :
    Optimization_hint.repr =
  match subkind with
  | Boxed_float -> Float
  | Boxed_int32 -> Int32
  | Boxed_int64 -> Int64
  | Boxed_nativeint -> Nativeint
  | Tagged_immediate -> Int
  | Anything | Boxed_float32 | Boxed_vec128 | Boxed_vec256 | Boxed_vec512
  | Boxed_mask | Variant _ | Float_block _ | Float_array | Immediate_array
  | Value_array | Generic_array | Unboxed_float32_array | Untagged_int_array
  | Untagged_int8_array | Untagged_int16_array | Unboxed_int32_array
  | Unboxed_int64_array | Unboxed_nativeint_array | Unboxed_vec128_array
  | Unboxed_vec256_array | Unboxed_vec512_array | Unboxed_mask_array
  | Unboxed_product_array ->
    Value

let repr_of_kind (kind : Flambda_kind.With_subkind.t) :
    Optimization_hint.repr option =
  match Flambda_kind.With_subkind.kind kind with
  | Value -> (
    match Flambda_kind.With_subkind.nullable kind with
    | Nullable -> Some Value
    | Non_nullable ->
      Some
        (repr_of_value_subkind
           (Flambda_kind.With_subkind.non_null_value_subkind kind)))
  | Naked_number Naked_immediate -> Some Int
  | Naked_number Naked_float -> Some Float
  | Naked_number Naked_int32 -> Some Int32
  | Naked_number Naked_int64 -> Some Int64
  | Naked_number Naked_nativeint -> Some Nativeint
  | Naked_number
      ( Naked_float32 | Naked_int8 | Naked_int16 | Naked_vec128 | Naked_vec256
      | Naked_vec512 | Naked_mask )
  | Region | Rec_info ->
    None

let reprs_of_arity arity =
  Flambda_arity.unarize arity |> Misc.Stdlib.List.map_option repr_of_kind

let return_repr_of_arity arity : Optimization_hint.repr option =
  match reprs_of_arity arity with
  | None -> None
  | Some [repr] -> Some repr
  | Some ([] | _ :: _ :: _) ->
    (* Several unarised results are returned as a block. *)
    Some Value

let closure_hint code_metadata ~params : Optimization_hint.closure_hint option =
  let params_arity = Code_metadata.params_arity code_metadata in
  if
    List.compare_length_with params
      (Flambda_arity.cardinal_unarized params_arity)
    <> 0
  then
    Misc.fatal_errorf "%d closure parameters do not match the arity %a"
      (List.length params) Flambda_arity.print params_arity;
  match
    ( reprs_of_arity params_arity,
      return_repr_of_arity (Code_metadata.result_arity code_metadata) )
  with
  | Some param_reprs, Some return ->
    let inline : Optimization_hint.inline_attribute =
      match Code_metadata.inline code_metadata with
      | Always_inline -> Always_inline
      | Available_inline -> Hint_inline
      | Never_inline -> Never_inline
      | Unroll n -> Unroll n
      | Default_inline -> Default_inline
    in
    Some
      { params = param_reprs;
        return;
        inline;
        (* Flambda 2 has no specialise attribute *)
        specialise = Default_specialise;
        is_a_functor = Code_metadata.is_a_functor code_metadata
      }
  | (Some _ | None), (Some _ | None) -> None

let block ~env ~res ~tag ~mut ~fields :
    Jsir.expr * To_jsir_env.t * To_jsir_result.t =
  let tag = Tag.to_int tag in
  let mutability : Jsir.mutability =
    match (mut : Mutability.t) with
    | Mutable -> Maybe_mutable
    | Immutable -> Immutable
    | Immutable_unique -> Immutable
  in
  let fields, res = simples ~env ~res fields in
  Block (tag, Array.of_list fields, NotArray, mutability), env, res
