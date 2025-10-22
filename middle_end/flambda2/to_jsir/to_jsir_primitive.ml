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

module Unboxed_array_tags = struct
  let unboxed_int64_array_tag = 1

  let unboxed_int32_array_even_tag = 2

  let unboxed_int32_array_odd_tag = 3

  let unboxed_float32_array_even_tag = 4

  let unboxed_float32_array_odd_tag = 5

  let unboxed_nativeint_array_tag = 9
end

exception Primitive_not_supported

(** Convert a [Simple.t] into a [Jsir.prim_arg]. *)
let prim_arg ~env ~res simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ ->
      Jsir.Pv (To_jsir_env.get_var_exn env name), res)
    ~symbol:(fun symbol ~coercion:_ ->
      let var, res = To_jsir_env.get_symbol_exn env ~res symbol in
      Jsir.Pv var, res)
    ~const:(fun const -> Jsir.Pc (To_jsir_shared.reg_width_const const), res)

let prim_args ~env ~res simples =
  List.fold_right
    (fun simple (args, res) ->
      let arg, res = prim_arg ~env ~res simple in
      arg :: args, res)
    simples ([], res)

let with_int_prefix_exn ~(kind : Flambda_kind.Standard_int.t) ~percent_for_imms
    op =
  let prefix =
    match kind, percent_for_imms with
    | (Tagged_immediate | Naked_immediate), true -> "%int"
    | (Tagged_immediate | Naked_immediate), false -> "caml_int"
    | Naked_int32, _ -> "caml_int32"
    | Naked_int64, _ -> "caml_int64"
    | Naked_nativeint, _ -> "caml_nativeint"
    | (Naked_int8 | Naked_int16), true -> "%int"
    | (Naked_int8 | Naked_int16), false -> "caml_int"
  in
  prefix ^ "_" ^ op

let with_float_suffix ~(bitwidth : Flambda_primitive.float_bitwidth) op =
  let suffix =
    match bitwidth with Float32 -> "_float32" | Float64 -> "_float"
  in
  "caml_" ^ op ^ suffix

let no_op ~env ~res = None, env, res

let identity ~env ~res x =
  let var, res = To_jsir_shared.simple ~env ~res x in
  Some var, env, res

let unit ~env ~res =
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Let (var, Constant Null))

let use_prim0 ~env ~res prim args =
  let expr : Jsir.expr = Prim (prim, args) in
  let var = Jsir.Var.fresh () in
  var, env, To_jsir_result.add_instr_exn res (Jsir.Let (var, expr))

let use_prim ~env ~res prim args =
  let var, env, res = use_prim0 ~env ~res prim args in
  Some var, env, res

let use_prim' ~env ~res prim simples =
  let args, res = prim_args ~env ~res simples in
  use_prim ~env ~res prim args

(* These work with [Simple.t] for use in [Num_conv] *)

let ensure_var ~env ~res = function
  | Jsir.Pv var -> var, env, res
  | Jsir.Pc const ->
    let var = Jsir.Var.fresh () in
    let res = To_jsir_result.add_instr_exn res (Let (var, Constant const)) in
    var, env, res

module Small_integer = struct
  type t =
    | Int
    | Int16
    | Int8

  let bits = function Int -> Targetint.num_bits () | Int16 -> 16 | Int8 -> 8

  let of_standard_int (kind : Flambda_kind.Standard_int.t) =
    match kind with
    | Tagged_immediate | Naked_immediate -> Some Int
    | Naked_int8 -> Some Int8
    | Naked_int16 -> Some Int16
    | Naked_int32 | Naked_int64 | Naked_nativeint -> None

  let require_standard_int kind =
    match of_standard_int kind with
    | Some width -> width
    | None -> Misc.fatal_error "Expected small integer kind"

  let shift_amount width =
    let bits = bits width in
    let word_bits = Targetint.num_bits () in
    if bits <= 0 || bits > word_bits
    then
      Misc.fatal_errorf "Cannot shift %d-bit value on %d-bit target" bits
        word_bits;
    word_bits - bits

  let mask = function
    | Int -> None
    | Int16 -> Some ((1 lsl 16) - 1)
    | Int8 -> Some ((1 lsl 8) - 1)

  let sign_extend ~env ~res width arg =
    match shift_amount width with
    | 0 -> ensure_var ~env ~res arg
    | shift ->
      let var, env, res = ensure_var ~env ~res arg in
      let shift_const = Jsir.Pc (Int (Targetint.of_int_exn shift)) in
      let shifted = Jsir.Var.fresh () in
      let res =
        To_jsir_result.add_instr_exn res
          (Let (shifted, Prim (Extern "%int_lsl", [Pv var; shift_const])))
      in
      let result = Jsir.Var.fresh () in
      let res =
        To_jsir_result.add_instr_exn res
          (Let (result, Prim (Extern "%int_asr", [Pv shifted; shift_const])))
      in
      result, env, res

  let zero_extend ~env ~res width arg =
    match mask width with
    | None -> ensure_var ~env ~res arg
    | Some mask ->
      let var, env, res = ensure_var ~env ~res arg in
      let mask_const = Jsir.Pc (Int (Targetint.of_int_exn mask)) in
      let result = Jsir.Var.fresh () in
      let res =
        To_jsir_result.add_instr_exn res
          (Let (result, Prim (Extern "%int_and", [Pv var; mask_const])))
      in
      result, env, res

  let sign_extend_var ~env ~res width var = sign_extend ~env ~res width (Pv var)
end

module Num_conv = struct
  type boxed =
    | Int32
    | Int64
    | Nativeint
    | Float
    | Float32

  type repr =
    | Small of Small_integer.t
    | Boxed of boxed

  type call =
    | Identity
    | Runtime of string

  let classify kind =
    let open Flambda_kind.Standard_int_or_float in
    match kind with
    | Tagged_immediate | Naked_immediate -> Small Small_integer.Int
    | Naked_int8 -> Small Small_integer.Int8
    | Naked_int16 -> Small Small_integer.Int16
    | Naked_int32 -> Boxed Int32
    | Naked_int64 -> Boxed Int64
    | Naked_nativeint -> Boxed Nativeint
    | Naked_float -> Boxed Float
    | Naked_float32 -> Boxed Float32

  let runtime_to src dst = Format.sprintf "caml_%s_to_%s" src dst

  let runtime_of dst src = Format.sprintf "caml_%s_of_%s" dst src

  let runtime_to_bytecode src dst =
    Format.sprintf "caml_%s_to_%s_bytecode" src dst

  let runtime_of_bytecode dst src =
    Format.sprintf "caml_%s_of_%s_bytecode" dst src

  let apply_call ~env ~res arg = function
    | Identity -> ensure_var ~env ~res arg
    | Runtime name -> use_prim0 ~env ~res (Extern name) [arg]

  let small_to_boxed = function
    | Int32 -> Runtime (runtime_of "int32" "int")
    | Int64 -> Runtime (runtime_of "int64" "int")
    | Nativeint -> Runtime (runtime_of "nativeint" "int")
    | Float -> Runtime (runtime_of "float" "int")
    | Float32 -> Runtime (runtime_of "float32" "int")

  let boxed_to_small = function
    | Int32 -> Runtime (runtime_to "int32" "int")
    | Int64 -> Runtime (runtime_to "int64" "int")
    | Nativeint -> Runtime (runtime_to "nativeint" "int")
    | Float -> Runtime (runtime_of "int" "float")
    | Float32 -> Runtime (runtime_of "int" "float32")

  let boxed_to_boxed src dst =
    match src, dst with
    | Int32, Int32
    | Int64, Int64
    | Nativeint, Nativeint
    | Float, Float
    | Float32, Float32 ->
      Identity
    | Int32, Int64 -> Runtime (runtime_of "int64" "int32")
    | Int32, Nativeint -> Runtime (runtime_of "nativeint" "int32")
    | Int32, Float32 -> Runtime (runtime_of "float32" "int32")
    | Int32, Float -> Runtime (runtime_to "int32" "float")
    | Int64, Int32 -> Runtime (runtime_to "int64" "int32")
    | Int64, Nativeint -> Runtime (runtime_to "int64" "nativeint")
    | Int64, Float -> Runtime (runtime_to "int64" "float")
    | Int64, Float32 -> Runtime (runtime_of_bytecode "float32" "int64")
    | Nativeint, Int32 -> Runtime (runtime_to "nativeint" "int32")
    | Nativeint, Int64 -> Runtime (runtime_of "int64" "nativeint")
    | Nativeint, Float -> Runtime (runtime_to "nativeint" "float")
    | Nativeint, Float32 -> Runtime (runtime_of "float32" "nativeint")
    | Float, Int32 -> Runtime (runtime_of "int32" "float")
    | Float, Int64 -> Runtime (runtime_of "int64" "float")
    | Float, Nativeint -> Runtime (runtime_of "nativeint" "float")
    | Float, Float32 -> Runtime (runtime_of "float32" "float")
    | Float32, Int32 -> Runtime (runtime_of "int32" "float32")
    | Float32, Int64 -> Runtime (runtime_to_bytecode "float32" "int64")
    | Float32, Nativeint -> Runtime (runtime_of "nativeint" "float32")
    | Float32, Float -> Runtime (runtime_of "float" "float32")

  let convert_small_to_small ~env ~res ~src ~dst arg =
    if Small_integer.bits src <= Small_integer.bits dst
    then ensure_var ~env ~res arg
    else Small_integer.sign_extend ~env ~res dst arg

  let convert ~env ~res arg ~src_kind ~dst_kind =
    match classify src_kind, classify dst_kind with
    | Small src_small, Small dst_small ->
      convert_small_to_small ~env ~res ~src:src_small ~dst:dst_small arg
    | Small _, Boxed dst_boxed ->
      apply_call ~env ~res arg (small_to_boxed dst_boxed)
    | Boxed src_boxed, Small dst_small ->
      let var, env, res = apply_call ~env ~res arg (boxed_to_small src_boxed) in
      Small_integer.sign_extend_var ~env ~res dst_small var
    | Boxed src_boxed, Boxed dst_boxed ->
      apply_call ~env ~res arg (boxed_to_boxed src_boxed dst_boxed)
end

let nullary_exn ~env ~res (f : Flambda_primitive.nullary_primitive) =
  let use_prim' prim = use_prim ~env ~res prim [] in
  match f with
  | Invalid _ -> use_prim' (Extern "caml_invalid_primitive")
  | Optimised_out _ ->
    (* For phantom lets, which are kept around for debugging information for
       pieces of code that were optimised away. *)
    no_op ~env ~res
  | Probe_is_enabled _ -> raise Primitive_not_supported
  | Enter_inlined_apply _ ->
    (* CR selee: we should eventually use this debuginfo *)
    no_op ~env ~res
  | Dls_get -> use_prim' (Extern "caml_domain_dls_get")
  | Poll ->
    (* See [parse_bytecode.ml] in jsoo - treated as a noop *)
    no_op ~env ~res
  | Cpu_relax -> use_prim' (Extern "caml_ml_domain_cpu_relax")

let get_tag ~env ~res x =
  let x, res = prim_arg ~env ~res x in
  use_prim0 ~env ~res (Extern "%direct_obj_tag") [x]

let check_tag ~env ~res x ~tag =
  let tag_var, env, res = get_tag ~env ~res x in
  let expr : Jsir.expr =
    Prim (Eq, [Pv tag_var; Pc (Int (Targetint.of_int_exn tag))])
  in
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let block_access_kind_exn (kind : Flambda_primitive.Block_access_kind.t) :
    Jsir.field_type =
  match kind with
  | Values _ -> Non_float
  | Naked_floats _ -> Float
  | Mixed { field_kind = Value_prefix _; _ } -> Non_float
  | Mixed { field_kind = Flat_suffix (Naked_float | Naked_float32); _ } -> Float
  | Mixed
      { field_kind =
          Flat_suffix
            (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint);
        _
      } ->
    Non_float
  | Mixed { field_kind = Flat_suffix (Naked_int8 | Naked_int16); _ } ->
    Non_float
  | Mixed
      { field_kind = Flat_suffix (Naked_vec128 | Naked_vec256 | Naked_vec512);
        _
      } ->
    raise Primitive_not_supported

let check_my_closure ~env x =
  (* It is not possible to project values out of other functions' closures in
     Jsoo, but certain inlining simplifications in Flambda may cause this. For
     now we disable inlining so this case should never occur. *)
  Simple.pattern_match' x
    ~var:(fun name ~coercion:_ ->
      if not (To_jsir_env.is_my_closure env name)
      then
        Misc.fatal_error
          "Trying to project from a closure that doesn't belong to the body of \
           the function being translated")
    ~symbol:(fun _symbol ~coercion:_ ->
      (* CR selee: We should check here that this is indeed the correct closure
         symbol for the current function being translated, but we don't yet keep
         track of this. We plan to refactor the handling of closures and pieces
         of code so that we only translate the code once we see the
         corresponding closure (treating each closure as a new instance of the
         code), which should make this check much easier. For now, we assume
         that we have the correct symbol. *)
      ())
    ~const:(fun const ->
      Misc.fatal_errorf
        "Trying to project from a closure, but found a constant %a (must be a \
         closure variable or symbol)"
        Reg_width_const.print const)

let unary_exn ~env ~res (f : Flambda_primitive.unary_primitive) x =
  let use_prim' prim = use_prim' ~env ~res prim [x] in
  match f with
  | Block_load { kind; mut = _; field } ->
    let var = Jsir.Var.fresh () in
    let expr, res =
      match prim_arg ~env ~res x with
      | Pv v, res ->
        ( Jsir.Field
            (v, Target_ocaml_int.to_int field, block_access_kind_exn kind),
          res )
      | Pc _, _res -> Misc.fatal_error "Block_load on constant"
    in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Duplicate_block _ | Duplicate_array _ | Obj_dup ->
    use_prim' (Extern "caml_obj_dup")
  | Is_int _ -> use_prim' IsInt
  | Is_null ->
    let x, res = prim_arg ~env ~res x in
    use_prim ~env ~res Eq [x; Pc Null]
  | Get_tag ->
    let var, env, res = get_tag ~env ~res x in
    Some var, env, res
  | Array_length _ -> use_prim' Vectlength
  | Bigarray_length { dimension } ->
    let x, res = prim_arg ~env ~res x in
    use_prim ~env ~res (Extern "caml_ba_dim")
      [x; Pc (Int (Targetint.of_int_exn (dimension - 1)))]
  | String_length _ -> use_prim' (Extern "caml_ml_string_length")
  | Int_as_pointer _ -> use_prim' (Extern "caml_int_as_pointer")
  | Opaque_identity { middle_end_only = true; kind = _ } -> identity ~env ~res x
  | Opaque_identity { middle_end_only = false; kind : Flambda_kind.t = _ } ->
    (* CR selee: treating these as the identity for now *)
    identity ~env ~res x
  | Int_arith (kind, Swap_byte_endianness) -> (
    match kind with
    | Naked_int8 -> identity ~env ~res x
    | ( Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate | Tagged_immediate ) as kind ->
      let extern_name =
        match kind with
        | Naked_int32 -> "caml_int32_bswap"
        | Naked_int64 -> "caml_int64_bswap"
        | Naked_nativeint -> "caml_nativeint_bswap"
        | Naked_int16 | Naked_immediate | Tagged_immediate -> "caml_bswap16"
        | Naked_int8 -> Misc.fatal_error "Unexpected int8 byte swap"
      in
      use_prim' (Extern extern_name))
  | Float_arith (bitwidth, op) ->
    let op_name = match op with Abs -> "abs" | Neg -> "neg" in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern extern_name)
  | Num_conv { src; dst } ->
    let arg, res = prim_arg ~env ~res x in
    let result, env, res =
      Num_conv.convert ~env ~res arg ~src_kind:src ~dst_kind:dst
    in
    Some result, env, res
  | Boolean_not -> use_prim' Not
  | Reinterpret_64_bit_word reinterpret ->
    let extern_name =
      match reinterpret with
      | Unboxed_int64_as_unboxed_float64 -> "caml_int64_float_of_bits"
      | Unboxed_float64_as_unboxed_int64 -> "caml_int64_bits_of_float"
      | Unboxed_int64_as_tagged_int63 ->
        (* JS doesn't have tagged int63 since it's a 32-bit target *)
        raise Primitive_not_supported
      | Tagged_int63_as_unboxed_int64 -> raise Primitive_not_supported
    in
    use_prim' (Extern extern_name)
  | Unbox_number _ | Box_number _ | Untag_immediate | Tag_immediate ->
    (* everything is untagged and "unboxed" in JS: see README *)
    identity ~env ~res x
  | Project_function_slot { move_from = _; move_to } ->
    check_my_closure ~env x;
    Some (To_jsir_env.get_function_slot_exn env move_to), env, res
  | Project_value_slot { project_from = _; value_slot } ->
    check_my_closure ~env x;
    Some (To_jsir_env.get_value_slot_exn env value_slot), env, res
  | Is_boxed_float -> check_tag ~env ~res x ~tag:Obj.double_tag
  | Is_flat_float_array -> check_tag ~env ~res x ~tag:Obj.double_array_tag
  | End_region _ | End_try_region _ -> no_op ~env ~res
  | Get_header ->
    (* CR selee: check [js_of_ocaml/compiler/tests_check_prim/main.output], this
       primitive ("caml_get_header") seems to be missing from jsoo *)
    raise Primitive_not_supported
  | Peek _ ->
    (* Unsupported in bytecode *)
    raise Primitive_not_supported
  | Make_lazy tag ->
    let tag = Flambda_primitive.Lazy_block_tag.to_tag tag in
    let expr, env, res =
      To_jsir_shared.block ~env ~res ~tag ~mut:Mutable ~fields:[x]
    in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let binary_exn ~env ~res (f : Flambda_primitive.binary_primitive) x y =
  let use_prim' prim = use_prim' ~env ~res prim [x; y] in
  match f with
  | Block_set { kind; init = _; field } ->
    let x, res =
      match prim_arg ~env ~res x with
      | Pv x, res -> x, res
      | Pc _, _res -> Misc.fatal_error "Block_set on constant"
    in
    let y, res = To_jsir_shared.simple ~env ~res y in
    ( None,
      env,
      To_jsir_result.add_instr_exn res
        (Set_field
           (x, Target_ocaml_int.to_int field, block_access_kind_exn kind, y)) )
  | Array_load (kind, load_kind, _mut) -> (
    match kind, load_kind with
    | ( ( Immediates | Values | Naked_floats | Naked_float32s | Naked_int32s
        | Naked_int64s | Naked_nativeints | Unboxed_product _ ),
        ( Immediates | Values | Naked_floats | Naked_float32s | Naked_int32s
        | Naked_int64s | Naked_nativeints ) ) ->
      use_prim' Array_get
    | (Naked_vec128s | Naked_vec256s | Naked_vec512s), _
    | _, (Naked_vec128s | Naked_vec256s | Naked_vec512s) ->
      (* No SIMD *)
      raise Primitive_not_supported)
  | String_or_bigstring_load (value, width) ->
    let op_name =
      match width with
      | Eight -> "unsafe_get"
      | Sixteen -> "get16"
      | Thirty_two -> "get32"
      | Single -> "getf32"
      | Sixty_four -> "get64"
      | One_twenty_eight _ | Two_fifty_six _ | Five_twelve _ ->
        raise Primitive_not_supported
    in
    let extern_name =
      match value with
      | String -> "caml_string_" ^ op_name
      | Bytes -> "caml_bytes_" ^ op_name
      | Bigstring -> (
        match width with
        | Eight -> "caml_ba_get_1"
        | Sixteen | Thirty_two | Single | Sixty_four | One_twenty_eight _
        | Two_fifty_six _ | Five_twelve _ ->
          "caml_ba_uint8_" ^ op_name)
    in
    use_prim' (Extern extern_name)
  | Bigarray_load (_dims, _kind, _layout) ->
    (* The index calculation is already done in Flambda, so we are free to
       ignore the parameters. *)
    use_prim' (Extern "caml_ba_get_raw_unsafe")
  | Phys_equal comparison ->
    let prim : Jsir.prim = match comparison with Eq -> Eq | Neq -> Neq in
    use_prim' prim
  | Int_arith (kind, op) -> (
    match kind with
    | Naked_int8 | Naked_int16 ->
      let op_name =
        match op with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
        | Mod -> "mod"
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
      in
      let extern_name = "%int_" ^ op_name in
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
      let result, env, res = use_prim0 ~env ~res (Extern extern_name) [x; y] in
      let width = Small_integer.require_standard_int kind in
      let result, env, res =
        Small_integer.sign_extend_var ~env ~res width result
      in
      Some result, env, res
    | Tagged_immediate | Naked_immediate | Naked_int32 | Naked_int64
    | Naked_nativeint ->
      let op_name =
        match op with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
        | Mod -> "mod"
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
      in
      let extern_name =
        with_int_prefix_exn ~kind op_name ~percent_for_imms:true
      in
      use_prim' (Extern extern_name))
  | Int_shift (kind, op) -> (
    match kind with
    | Naked_int8 | Naked_int16 ->
      let width = Small_integer.require_standard_int kind in
      let shift_op =
        match op with
        | Lsl -> "%int_lsl"
        | Lsr -> "%int_lsr"
        | Asr -> "%int_asr"
      in
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
      let x, env, res =
        match op with
        | Lsr ->
          let var, env, res = Small_integer.zero_extend ~env ~res width x in
          Jsir.Pv var, env, res
        | Lsl | Asr -> x, env, res
      in
      let raw_result = Jsir.Var.fresh () in
      let res =
        To_jsir_result.add_instr_exn res
          (Let (raw_result, Prim (Extern shift_op, [x; y])))
      in
      let result, env, res =
        Small_integer.sign_extend_var ~env ~res width raw_result
      in
      Some result, env, res
    | Tagged_immediate | Naked_immediate | Naked_int32 | Naked_int64
    | Naked_nativeint ->
      let op_name =
        match op with
        | Lsl -> (
          match kind with
          | Tagged_immediate | Naked_immediate -> "lsl"
          | Naked_int32 | Naked_int64 | Naked_nativeint -> "shift_left"
          | Naked_int8 | Naked_int16 ->
            Misc.fatal_error "Unexpected small-int shift left")
        | Lsr -> (
          match kind with
          | Tagged_immediate | Naked_immediate -> "lsr"
          | Naked_int32 | Naked_int64 | Naked_nativeint ->
            "shift_right_unsigned"
          | Naked_int8 | Naked_int16 ->
            Misc.fatal_error "Unexpected small-int logical shift")
        | Asr -> (
          match kind with
          | Tagged_immediate | Naked_immediate -> "asr"
          | Naked_int32 | Naked_int64 | Naked_nativeint -> "shift_right"
          | Naked_int8 | Naked_int16 ->
            Misc.fatal_error "Unexpected small-int arithmetic shift")
      in
      let extern_name =
        with_int_prefix_exn ~kind op_name ~percent_for_imms:true
      in
      use_prim' (Extern extern_name))
  | Int_comp (kind, behaviour) -> (
    match behaviour with
    | Yielding_bool comparison -> (
      let eq, neq, lt, ult, le =
        let open Jsir in
        match kind with
        | Tagged_immediate | Naked_immediate | Naked_int32 | Naked_nativeint ->
          Eq, Neq, Lt, Ult, Le
        | Naked_int8 | Naked_int16 ->
          (* Small ints use regular JS comparisons. For unsigned comparisons, we
             mask the operands first, then use regular signed comparison *)
          Eq, Neq, Lt, Lt, Le
        | Naked_int64 ->
          ( Extern "caml_equal",
            Extern "caml_notequal",
            Extern "caml_lessthan",
            Extern "caml_int64_ult",
            Extern "caml_lessequal" )
      in
      let unsigned_le x y =
        let var_ule = Jsir.Var.fresh () in
        let var_eq = Jsir.Var.fresh () in
        let var_or = Jsir.Var.fresh () in
        let expr_ule : Jsir.expr = Prim (ult, [x; y]) in
        let expr_eq : Jsir.expr = Prim (eq, [x; y]) in
        let res =
          To_jsir_result.add_instr_exn res (Jsir.Let (var_ule, expr_ule))
        in
        let res =
          To_jsir_result.add_instr_exn res (Jsir.Let (var_eq, expr_eq))
        in
        let expr_or : Jsir.expr =
          Prim (Extern "%int_or", [Pv var_ule; Pv var_eq])
        in
        ( Some var_or,
          env,
          To_jsir_result.add_instr_exn res (Jsir.Let (var_or, expr_or)) )
      in
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
      let x, y, env, res =
        match kind, comparison with
        | ( ((Naked_int8 | Naked_int16) as small_kind),
            (Lt Unsigned | Gt Unsigned | Le Unsigned | Ge Unsigned) ) ->
          let width = Small_integer.require_standard_int small_kind in
          let x_var, env, res = Small_integer.zero_extend ~env ~res width x in
          let y_var, env, res = Small_integer.zero_extend ~env ~res width y in
          Jsir.Pv x_var, Jsir.Pv y_var, env, res
        | ( ( Tagged_immediate | Naked_immediate | Naked_int32 | Naked_int64
            | Naked_nativeint ),
            _ )
        | _, (Eq | Neq | Lt Signed | Gt Signed | Le Signed | Ge Signed) ->
          x, y, env, res
      in
      match comparison with
      | Eq -> use_prim ~env ~res eq [x; y]
      | Neq -> use_prim ~env ~res neq [x; y]
      | Lt Signed -> use_prim ~env ~res lt [x; y]
      | Lt Unsigned -> use_prim ~env ~res ult [x; y]
      | Gt Signed -> use_prim ~env ~res lt [y; x]
      | Gt Unsigned -> use_prim ~env ~res ult [y; x]
      | Le Signed -> use_prim ~env ~res le [x; y]
      | Le Unsigned -> unsigned_le x y
      | Ge Signed -> use_prim ~env ~res le [y; x]
      | Ge Unsigned -> unsigned_le y x)
    | Yielding_int_like_compare_functions signed_or_unsigned -> (
      match signed_or_unsigned with
      | Signed ->
        let extern_name =
          with_int_prefix_exn ~kind "compare" ~percent_for_imms:false
        in
        use_prim' (Extern extern_name)
      | Unsigned -> (
        match kind with
        | (Naked_int8 | Naked_int16) as small_kind ->
          (* For small ints, zero-extend both operands then do signed
             comparison *)
          let width = Small_integer.require_standard_int small_kind in
          let x, res = prim_arg ~env ~res x in
          let y, res = prim_arg ~env ~res y in
          let x_var, env, res = Small_integer.zero_extend ~env ~res width x in
          let y_var, env, res = Small_integer.zero_extend ~env ~res width y in
          use_prim ~env ~res (Extern "caml_int_compare") [Pv x_var; Pv y_var]
        | Tagged_immediate | Naked_immediate ->
          (* For regular ints, use unsigned compare by adding min_int to both
             operands to shift them into the positive range, then do signed
             comparison *)
          let x, res = prim_arg ~env ~res x in
          let y, res = prim_arg ~env ~res y in
          let min_int = Targetint.min_int () in
          let x_shifted = Jsir.Var.fresh () in
          let y_shifted = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res
              (Let (x_shifted, Prim (Extern "%int_sub", [x; Pc (Int min_int)])))
          in
          let res =
            To_jsir_result.add_instr_exn res
              (Let (y_shifted, Prim (Extern "%int_sub", [y; Pc (Int min_int)])))
          in
          use_prim ~env ~res (Extern "caml_int_compare")
            [Pv x_shifted; Pv y_shifted]
        | Naked_int32 | Naked_nativeint ->
          (* For int32/nativeint, use the same approach as regular ints *)
          let x, res = prim_arg ~env ~res x in
          let y, res = prim_arg ~env ~res y in
          let x_shifted = Jsir.Var.fresh () in
          let y_shifted = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res
              (Let
                 ( x_shifted,
                   Prim (Extern "caml_int32_sub", [x; Pc (Int32 Int32.min_int)])
                 ))
          in
          let res =
            To_jsir_result.add_instr_exn res
              (Let
                 ( y_shifted,
                   Prim (Extern "caml_int32_sub", [y; Pc (Int32 Int32.min_int)])
                 ))
          in
          use_prim ~env ~res (Extern "caml_int32_compare")
            [Pv x_shifted; Pv y_shifted]
        | Naked_int64 ->
          (* For int64, use the same min_int trick *)
          let x, res = prim_arg ~env ~res x in
          let y, res = prim_arg ~env ~res y in
          let x_shifted = Jsir.Var.fresh () in
          let y_shifted = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res
              (Let
                 ( x_shifted,
                   Prim (Extern "caml_int64_sub", [x; Pc (Int64 Int64.min_int)])
                 ))
          in
          let res =
            To_jsir_result.add_instr_exn res
              (Let
                 ( y_shifted,
                   Prim (Extern "caml_int64_sub", [y; Pc (Int64 Int64.min_int)])
                 ))
          in
          use_prim ~env ~res (Extern "caml_int64_compare")
            [Pv x_shifted; Pv y_shifted])))
  | Float_arith (bitwidth, op) ->
    let op_name =
      match op with Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div"
    in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern extern_name)
  | Float_comp (bitwidth, behaviour) ->
    let extern_name =
      match behaviour with
      | Yielding_bool comparison ->
        let op_name =
          match comparison with
          | Eq -> "eq"
          | Neq -> "neq"
          | Lt () -> "lt"
          | Gt () -> "gt"
          | Le () -> "le"
          | Ge () -> "ge"
        in
        with_float_suffix ~bitwidth op_name
      | Yielding_int_like_compare_functions () -> (
        match bitwidth with
        | Float64 -> "caml_float_compare"
        | Float32 -> "caml_float32_compare")
    in
    use_prim' (Extern extern_name)
  | Atomic_load_field _ -> use_prim' (Extern "caml_atomic_load_field")
  | Bigarray_get_alignment _ ->
    (* Only used for SIMD *)
    raise Primitive_not_supported
  | Poke _ ->
    (* Unsupported in bytecode *)
    raise Primitive_not_supported
  | Read_offset _ ->
    (* CR selee: This is for block indices, which likely requires changes to
       JSOO to support. We will leave this for now. *)
    raise Primitive_not_supported

let ternary_exn ~env ~res (f : Flambda_primitive.ternary_primitive) x y z =
  let use_prim' prim = use_prim' ~env ~res prim [x; y; z] in
  match f with
  | Array_set (kind, set_kind) -> (
    match kind, set_kind with
    | ( ( Immediates | Values | Naked_floats | Naked_float32s | Naked_int32s
        | Naked_int64s | Naked_nativeints | Unboxed_product _ ),
        ( Immediates | Values _ | Naked_floats | Naked_float32s | Naked_int32s
        | Naked_int64s | Naked_nativeints ) ) ->
      let arr, res =
        match prim_arg ~env ~res x with
        | Pv v, res -> v, res
        | Pc _, _res -> Misc.fatal_error "Array_set on constant"
      in
      let index, res = To_jsir_shared.simple ~env ~res y in
      let new_value, res = To_jsir_shared.simple ~env ~res z in
      ( None,
        env,
        To_jsir_result.add_instr_exn res (Array_set (arr, index, new_value)) )
    | (Naked_vec128s | Naked_vec256s | Naked_vec512s), _
    | _, (Naked_vec128s | Naked_vec256s | Naked_vec512s) ->
      (* No SIMD *)
      raise Primitive_not_supported)
  | Bytes_or_bigstring_set (value, width) ->
    let extern_name =
      match value, width with
      | _, One_twenty_eight _ | _, Two_fifty_six _ | _, Five_twelve _ ->
        (* No SIMD *)
        raise Primitive_not_supported
      | Bytes, Eight -> "caml_bytes_unsafe_set"
      | Bytes, Sixteen -> "caml_bytes_set16"
      | Bytes, Thirty_two -> "caml_bytes_set32"
      | Bytes, Single -> "caml_bytes_setf32"
      | Bytes, Sixty_four -> "caml_bytes_set64"
      | Bigstring, Eight -> "caml_ba_set_1"
      | Bigstring, Sixteen -> "caml_ba_uint8_set16"
      | Bigstring, Thirty_two -> "caml_ba_uint8_set32"
      | Bigstring, Single -> "caml_ba_uint8_setf32"
      | Bigstring, Sixty_four -> "caml_ba_uint8_set64"
    in
    use_prim' (Extern extern_name)
  | Bigarray_set (_dims, _kind, _layout) ->
    (* The index calculation is already done in Flambda, so we are free to
       ignore the parameters. *)
    use_prim' (Extern "caml_ba_set_raw_unsafe")
  | Atomic_field_int_arith op ->
    let extern_name =
      match op with
      | Fetch_add -> "caml_atomic_fetch_add_field"
      | Add -> "caml_atomic_add_field"
      | Sub -> "caml_atomic_sub_field"
      | And -> "caml_atomic_land_field"
      | Or -> "caml_atomic_lor_field"
      | Xor -> "caml_atomic_lxor_field"
    in
    use_prim' (Extern extern_name)
  | Atomic_set_field _ ->
    let _var, env, res = use_prim' (Extern "caml_atomic_exchange_field") in
    unit ~env ~res
  | Atomic_exchange_field _ -> use_prim' (Extern "caml_atomic_exchange_field")
  | Write_offset _ ->
    (* CR selee: This is for block indices, which likely requires changes to
       JSOO to support. We will leave this for now. *)
    raise Primitive_not_supported

let quaternary_exn ~env ~res (f : Flambda_primitive.quaternary_primitive) w x y
    z =
  let use_prim' prim = use_prim' ~env ~res prim [w; x; y; z] in
  match f with
  | Atomic_compare_and_set_field _ -> use_prim' (Extern "caml_atomic_cas_field")
  | Atomic_compare_exchange_field _ ->
    use_prim' (Extern "caml_atomic_compare_exchange_field")

let variadic_exn ~env ~res (f : Flambda_primitive.variadic_primitive) xs =
  match f with
  | Begin_region _ | Begin_try_region _ -> no_op ~env ~res
  | Make_block (kind, mut, _alloc_mode) ->
    let tag =
      match kind with
      | Values (tag, _with_subkind) -> Tag.Scannable.to_tag tag
      | Mixed (tag, _mixed_block_shape) -> Tag.Scannable.to_tag tag
      | Naked_floats -> Tag.double_array_tag
    in
    let expr, env, res = To_jsir_shared.block ~env ~res ~tag ~mut ~fields:xs in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Make_array (kind, mut, _mode) ->
    let tag =
      match kind with
      | Immediates | Values -> 0
      | Naked_int32s ->
        if List.length xs mod 2 = 0
        then Unboxed_array_tags.unboxed_int32_array_even_tag
        else Unboxed_array_tags.unboxed_int32_array_odd_tag
      | Naked_int64s -> Unboxed_array_tags.unboxed_int64_array_tag
      | Naked_nativeints -> Unboxed_array_tags.unboxed_nativeint_array_tag
      | Naked_floats -> Tag.double_array_tag |> Tag.to_int
      | Naked_float32s ->
        if List.length xs mod 2 = 0
        then Unboxed_array_tags.unboxed_float32_array_even_tag
        else Unboxed_array_tags.unboxed_float32_array_odd_tag
      | Unboxed_product _ -> 0
      | Naked_vec128s | Naked_vec256s | Naked_vec512s ->
        (* No SIMD *)
        raise Primitive_not_supported
    in
    let mutability : Jsir.mutability =
      match mut with
      | Mutable -> Maybe_mutable
      | Immutable | Immutable_unique -> Immutable
    in
    let xs, res = To_jsir_shared.simples ~env ~res xs in
    let var = Jsir.Var.fresh () in
    ( Some var,
      env,
      To_jsir_result.add_instr_exn res
        (Let (var, Block (tag, Array.of_list xs, Array, mutability))) )

let primitive ~env ~res (prim : Flambda_primitive.t) =
  try
    match prim with
    | Nullary f -> nullary_exn ~env ~res f
    | Unary (f, x) -> unary_exn ~env ~res f x
    | Binary (f, x, y) -> binary_exn ~env ~res f x y
    | Ternary (f, x, y, z) -> ternary_exn ~env ~res f x y z
    | Quaternary (f, w, x, y, z) -> quaternary_exn ~env ~res f w x y z
    | Variadic (f, xs) -> variadic_exn ~env ~res f xs
  with Primitive_not_supported ->
    Misc.fatal_errorf
      "The primitive %a is not supported for JavaScript/WASM compilation."
      Flambda_primitive.print prim

let extern ~env ~res symbol args =
  let args, res = prim_args ~env ~res args in
  let name = Symbol.linkage_name_as_string symbol in
  let var = Jsir.Var.fresh () in
  var, To_jsir_result.add_instr_exn res (Let (var, Prim (Extern name, args)))
