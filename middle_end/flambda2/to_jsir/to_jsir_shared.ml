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

open! Jsoo_imports

let bind_expr_to_var' ~env ~res fvar expr =
  let jvar = Jsir.Var.fresh () in
  ( jvar,
    To_jsir_env.add_var env fvar jvar,
    To_jsir_result.add_instr_exn res (Jsir.Let (jvar, expr)) )

let bind_expr_to_var ~env ~res fvar expr =
  let _jvar, env, res = bind_expr_to_var' ~env ~res fvar expr in
  env, res

let target_ocaml_int_to_jsir_const targetint : Jsir.constant =
  let check_size ~received =
    let expected = Targetint.num_bits () in
    if received <> expected
    then
      Misc.fatal_errorf
        "BUG: JSIR expected %d-bit integers from flambda, but got a %d-bit int \
         from flambda"
        expected received
  in
  let targetint =
    match Target_ocaml_int.repr targetint with
    | Int31 i ->
      check_size ~received:31;
      Targetint.of_int32_exn i
    | Int32 i ->
      check_size ~received:32;
      Targetint.of_int32_exn i
    | Int63 (_ : int64) ->
      check_size ~received:63;
      assert false
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

let int32_to_jsir_const int32 : Jsir.constant = Int32 int32

let int64_to_jsir_const int64 : Jsir.constant = Int64 int64

let nativeint_to_jsir_const nativeint : Jsir.constant =
  Int32 (Targetint_32_64.to_int32 nativeint)

let reg_width_const const : Jsir.constant =
  match Reg_width_const.descr const with
  | Naked_immediate targetint | Tagged_immediate targetint ->
    target_ocaml_int_to_jsir_const targetint
  | Naked_float32 float32 -> float32_to_jsir_const float32
  | Naked_float float -> float_to_jsir_const float
  | Naked_int32 int32 -> int32_to_jsir_const int32
  | Naked_int64 int64 -> int64_to_jsir_const int64
  | Naked_nativeint nativeint -> nativeint_to_jsir_const nativeint
  | Naked_int8 int8 ->
    (* Int8.t values are already sign-extended when created, so we can use
       directly *)
    let value = Numeric_types.Int8.to_int int8 in
    Int (Targetint.of_int_exn value)
  | Naked_int16 int16 ->
    (* Int16.t values are already sign-extended when created, so we can use
       directly *)
    let value = Numeric_types.Int16.to_int int16 in
    Int (Targetint.of_int_exn value)
  | Null -> Jsir.Null
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ ->
    (* SIMD not supported *)
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
