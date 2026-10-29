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

let with_int_prefix ~(kind : Flambda_kind.Standard_int.t) ~percent_for_imms op =
  let prefix =
    match kind, percent_for_imms with
    | (Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16), true ->
      "%int"
    | (Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16), false ->
      "caml_int"
    | Naked_int32, _ -> "caml_int32"
    | Naked_int64, _ -> "caml_int64"
    | Naked_nativeint, _ -> "caml_nativeint"
  in
  prefix ^ "_" ^ op

let with_float_suffix ~(bitwidth : Flambda_primitive.float_bitwidth) op =
  let suffix =
    match bitwidth with Float32 -> "_float32" | Float64 -> "_float"
  in
  "caml_" ^ op ^ suffix

let bigarray_hint ~(kind : Flambda_primitive.Bigarray_kind.t)
    ~(layout : Flambda_primitive.Bigarray_layout.t) : Optimization_hint.ccall =
  let kind : Optimization_hint.Bigarray.kind =
    match kind with
    | Float16 -> Float16
    | Float32 -> Float32
    | Float32_t -> Float32_t
    | Float64 -> Float64
    | Sint8 -> Int8_signed
    | Uint8 -> Int8_unsigned
    | Sint16 -> Int16_signed
    | Uint16 -> Int16_unsigned
    | Int32 -> Int32
    | Int64 -> Int64
    | Int_width_int -> Int
    | Targetint_width_int -> Nativeint
    | Complex32 -> Complex32
    | Complex64 -> Complex64
  in
  let layout : Optimization_hint.Bigarray.layout =
    match layout with C -> C | Fortran -> Fortran
  in
  (* Flambda inserts the bounds checks itself (see
     [Lambda_to_flambda_primitives.bigarray_access]) *)
  Hint_bigarray { unsafe = true; kind; layout }

let bigstring_hint = bigarray_hint ~kind:Uint8 ~layout:C

(* The unchecked variant ([u] suffix) of a string, bytes or bigstring access
   primitive of at least 16 bits, e.g. [caml_string_get16u], with
   [Hint_unsafe]. *)
type wide_access =
  | Get16
  | Get32
  | Getf32
  | Get64
  | Set16
  | Set32
  | Setf32
  | Set64

let unsafe_string_like_access (value : Flambda_primitive.string_like_value)
    (access : wide_access) : Jsir.prim =
  let op_name =
    match access with
    | Get16 -> "get16"
    | Get32 -> "get32"
    | Getf32 -> "getf32"
    | Get64 -> "get64"
    | Set16 -> "set16"
    | Set32 -> "set32"
    | Setf32 -> "setf32"
    | Set64 -> "set64"
  in
  let unchecked prefix : Jsir.prim =
    Extern (prefix ^ op_name ^ "u", Some Hint_unsafe)
  in
  match value with
  | String -> unchecked "caml_string_"
  | Bytes -> unchecked "caml_bytes_"
  | Bigstring -> (
    match access with
    | Get16 | Get64 | Set16 | Set64 ->
      (* The js_of_ocaml runtime only provides these unchecked variants for
         OCaml >= 5.6. *)
      Extern ("caml_ba_uint8_" ^ op_name, None)
    | Get32 | Getf32 | Set32 | Setf32 -> unchecked "caml_ba_uint8_")

let no_op ~env ~res = None, env, res

let identity ~env ~res x =
  let var, res = To_jsir_shared.simple ~env ~res x in
  Some var, env, res

let unit ~env ~res =
  let var = Jsir.Var.fresh () in
  ( Some var,
    env,
    To_jsir_result.add_instr_exn res (Let (var, Constant (Int Targetint.zero)))
  )

(* We only place debug events to primitives that are compiled to runtime calls
   by JSOO. *)
let is_runtime_call (prim : Jsir.prim) =
  match prim with
  | Extern (name, _) -> not (String.starts_with ~prefix:"%" name)
  | Vectlength _ | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult -> false

let use_prim0 ~env ~res prim args =
  let expr : Jsir.expr = Prim (prim, args) in
  let var = Jsir.Var.fresh () in
  let instr : Jsir.instr = Let (var, expr) in
  let res =
    if is_runtime_call prim
    then To_jsir_result.add_call_exn res instr
    else To_jsir_result.add_instr_exn res instr
  in
  var, env, res

let use_prim ~env ~res prim args =
  let var, env, res = use_prim0 ~env ~res prim args in
  Some var, env, res

let use_prim0' ~env ~res prim simples =
  let args, res = prim_args ~env ~res simples in
  use_prim0 ~env ~res prim args

let use_prim' ~env ~res prim simples =
  let args, res = prim_args ~env ~res simples in
  use_prim ~env ~res prim args

(* Smallints ([Naked_int8] and [Naked_int16]) are 32-bit integers that are
   always kept sign-extended, as in bytecode and Cmm. Operations whose result
   might not fit must therefore be followed by [sign_extend]. *)
let smallint_bits (kind : Flambda_kind.Standard_int.t) =
  match kind with
  | Naked_int8 -> Some 8
  | Naked_int16 -> Some 16
  | Tagged_immediate | Naked_immediate | Naked_int32 | Naked_int64
  | Naked_nativeint ->
    None

let int_const n : Jsir.prim_arg = Pc (Int (Targetint.of_int_exn n))

let sign_extend ~env ~res ~bits (x : Jsir.prim_arg) =
  let shift = int_const (Targetint.num_bits () - bits) in
  let shifted, env, res =
    use_prim0 ~env ~res (Extern ("%int_lsl", None)) [x; shift]
  in
  use_prim ~env ~res (Extern ("%int_asr", None)) [Pv shifted; shift]

let zero_extend ~env ~res ~bits (x : Jsir.prim_arg) =
  let mask = int_const ((1 lsl bits) - 1) in
  use_prim0 ~env ~res (Extern ("%int_and", None)) [x; mask]

(* Flambda lowers block index accesses to a (naked int64) byte offset from the
   first field of a block, where every unarised field is 8 bytes wide (see
   [Lambda.array_element_size_in_bytes]). JSIR blocks have one slot per field,
   so the field index is the byte offset divided by 8.

   The base may be null, in which case the offset is a raw address (an external
   pointer). As in bytecode, this is unsupported and raises [Failure] at
   runtime; see [caml_check_idx_base] in the JS runtime. *)
let field_index_of_byte_offset ~env ~res offset =
  let offset, res = prim_arg ~env ~res offset in
  let offset, env, res =
    use_prim0 ~env ~res (Extern ("caml_int64_to_int", None)) [offset]
  in
  use_prim0 ~env ~res (Extern ("%int_lsr", None)) [Pv offset; int_const 3]

let checked_block_index_base ~env ~res base =
  let base, res = prim_arg ~env ~res base in
  use_prim0 ~env ~res (Extern ("caml_check_idx_base", None)) [base]

let atomic_field ~env ~res
    (offset_units : Flambda_primitive.atomic_offset_units) extern_name ~base
    ~offset rest =
  let (base : Jsir.prim_arg), (offset : Jsir.prim_arg), env, res =
    match offset_units with
    | Field_index ->
      let base, res = prim_arg ~env ~res base in
      let offset, res = prim_arg ~env ~res offset in
      base, offset, env, res
    | Byte_offset ->
      let base, env, res = checked_block_index_base ~env ~res base in
      let index, env, res = field_index_of_byte_offset ~env ~res offset in
      Pv base, Pv index, env, res
  in
  let rest, res = prim_args ~env ~res rest in
  use_prim ~env ~res (Extern (extern_name, None)) (base :: offset :: rest)

let nullary_exn ~env ~res (f : Flambda_primitive.nullary_primitive) =
  let use_prim' prim = use_prim ~env ~res prim [] in
  match f with
  | Invalid _ -> use_prim' (Extern ("caml_invalid_primitive", None))
  | Optimised_out _ ->
    (* For phantom lets, which are kept around for debugging information for
       pieces of code that were optimised away. *)
    no_op ~env ~res
  | Probe_is_enabled _ -> raise Primitive_not_supported
  | Enter_inlined_apply _ ->
    (* CR selee: we should eventually use this debuginfo *)
    no_op ~env ~res
  | Dls_get -> use_prim' (Extern ("caml_domain_dls_get", None))
  | Tls_get -> use_prim' (Extern ("caml_domain_tls_get", None))
  | Domain_index -> use_prim' (Extern ("caml_ml_domain_index", None))
  | Poll ->
    (* See [parse_bytecode.ml] in jsoo - treated as a noop *)
    no_op ~env ~res
  | Cpu_relax -> use_prim' (Extern ("caml_ml_domain_cpu_relax", None))

(* [x] must be a block. *)
let get_tag ~env ~res x =
  let x, res = prim_arg ~env ~res x in
  use_prim0 ~env ~res (Extern ("%direct_obj_tag", None)) [x]

(* [x] may be any value, including a JS value that is not an OCaml block, so the
   tag is read with optional chaining. *)
let check_tag ~env ~res x ~tag =
  let x, res = prim_arg ~env ~res x in
  let tag_var, env, res =
    use_prim0 ~env ~res (Extern ("%direct_obj_tag_nullish", None)) [x]
  in
  let expr : Jsir.expr = Prim (Eq, [Pv tag_var; int_const tag]) in
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
            ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32
            | Naked_int64 | Naked_nativeint );
        _
      } ->
    Non_float
  | Mixed
      { field_kind =
          Flat_suffix (Naked_vec128 | Naked_vec256 | Naked_vec512 | Naked_mask);
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
  | Duplicate_block _ | Duplicate_array _ | Obj_dup _ ->
    use_prim' (Extern ("caml_obj_dup", None))
  | Is_int _ -> use_prim' IsInt
  | Is_null ->
    let x, res = prim_arg ~env ~res x in
    use_prim ~env ~res Eq [x; Pc Null_]
  | Get_tag ->
    let var, env, res = get_tag ~env ~res x in
    Some var, env, res
  | Array_length kind ->
    let kind : Optimization_hint.array_kind =
      match kind with
      | Float_array_opt_dynamic -> Generic
      | Array_kind Naked_floats -> Float
      | Array_kind
          ( Immediates | Gc_ignorable_values | Values | Naked_float32s
          | Naked_ints | Naked_int8s | Naked_int16s | Naked_int32s
          | Naked_int64s | Naked_nativeints | Naked_masks | Unboxed_product _ )
        ->
        (* Every array other than a flat float array is an ordinary block in
           JSIR (see [Make_array]) *)
        Value
      | Array_kind (Naked_vec128s | Naked_vec256s | Naked_vec512s) ->
        (* No SIMD *)
        raise Primitive_not_supported
    in
    use_prim' (Vectlength kind)
  | Bigarray_length { dimension } ->
    let x, res = prim_arg ~env ~res x in
    use_prim ~env ~res
      (Extern ("caml_ba_dim", None))
      [x; int_const (dimension - 1)]
  | String_length _ -> use_prim' (Extern ("caml_ml_string_length", None))
  | Int_as_pointer _ -> use_prim' (Extern ("caml_int_as_pointer", None))
  | Opaque_identity { middle_end_only = true; kind = _ } -> identity ~env ~res x
  | Opaque_identity { middle_end_only = false; kind : Flambda_kind.t = _ } ->
    (* CR selee: treating these as the identity for now *)
    identity ~env ~res x
  | Int_arith (kind, Not) ->
    let extern_name = with_int_prefix ~kind "xor" ~percent_for_imms:true in
    let x, res = prim_arg ~env ~res x in
    let minus_one =
      Reg_width_const.of_int_of_kind Thirty_two_no_gc_tag_bit
        (Flambda_kind.Standard_int.to_kind kind)
        (-1)
      |> To_jsir_shared.reg_width_const
    in
    use_prim ~env ~res (Extern (extern_name, None)) [x; Pc minus_one]
  | Int_arith (kind, Swap_byte_endianness) -> (
    match kind with
    | Naked_int8 -> identity ~env ~res x
    | Naked_int16 ->
      (* The swap might change the sign, so we need to sign-extend to maintain
         our representation invariant (see [sign_extend]). *)
      let var, env, res =
        use_prim0' ~env ~res (Extern ("caml_bswap16", None)) [x]
      in
      sign_extend ~env ~res ~bits:16 (Pv var)
    | Naked_immediate | Tagged_immediate ->
      use_prim' (Extern ("caml_bswap16", None))
    | Naked_int32 -> use_prim' (Extern ("caml_int32_bswap", None))
    | Naked_int64 -> use_prim' (Extern ("caml_int64_bswap", None))
    | Naked_nativeint -> use_prim' (Extern ("caml_nativeint_bswap", None)))
  | Float_arith (bitwidth, op) ->
    let op_name = match op with Abs -> "abs" | Neg -> "neg" in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern (extern_name, None))
  | Num_conv { src; dst } -> (
    let caml_of src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_of_%s" src dst, None))
    in
    let caml_of_bytecode src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_of_%s_bytecode" src dst, None))
    in
    let caml_to src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_to_%s" src dst, None))
    in
    let caml_to_bytecode src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_to_%s_bytecode" src dst, None))
    in
    (* Narrowing conversions to smallints: convert to a 32-bit integer first (if
       needed) and then sign-extend. *)
    let narrow ~bits =
      let x, res = prim_arg ~env ~res x in
      sign_extend ~env ~res ~bits x
    in
    let narrow_via extern_name ~bits =
      let var, env, res =
        use_prim0' ~env ~res (Extern (extern_name, None)) [x]
      in
      sign_extend ~env ~res ~bits (Pv var)
    in
    match src, dst with
    | ( Naked_int8,
        ( Naked_int8 | Naked_int16 | Tagged_immediate | Naked_immediate
        | Naked_int32 | Naked_nativeint ) )
    | ( Naked_int16,
        ( Naked_int16 | Tagged_immediate | Naked_immediate | Naked_int32
        | Naked_nativeint ) ) ->
      (* Smallints are kept sign-extended, so widening is the identity *)
      identity ~env ~res x
    | (Naked_int8 | Naked_int16), Naked_int64 -> caml_of "int64" "int32"
    | (Naked_int8 | Naked_int16), Naked_float -> caml_of "float" "int"
    | (Naked_int8 | Naked_int16), Naked_float32 -> caml_of "float32" "int"
    | ( ( Tagged_immediate | Naked_immediate | Naked_int32 | Naked_nativeint
        | Naked_int16 ),
        Naked_int8 ) ->
      narrow ~bits:8
    | ( (Tagged_immediate | Naked_immediate | Naked_int32 | Naked_nativeint),
        Naked_int16 ) ->
      narrow ~bits:16
    | Naked_int64, Naked_int8 -> narrow_via "caml_int64_to_int32" ~bits:8
    | Naked_int64, Naked_int16 -> narrow_via "caml_int64_to_int32" ~bits:16
    | Naked_float, Naked_int8 -> narrow_via "caml_int_of_float" ~bits:8
    | Naked_float, Naked_int16 -> narrow_via "caml_int_of_float" ~bits:16
    | Naked_float32, Naked_int8 -> narrow_via "caml_int_of_float32" ~bits:8
    | Naked_float32, Naked_int16 -> narrow_via "caml_int_of_float32" ~bits:16
    | (Tagged_immediate | Naked_immediate), (Tagged_immediate | Naked_immediate)
    | Naked_float32, Naked_float32
    | Naked_float, Naked_float
    | Naked_int32, Naked_int32
    | Naked_int64, Naked_int64
    | Naked_nativeint, Naked_nativeint ->
      identity ~env ~res x
    | (Tagged_immediate | Naked_immediate), Naked_float32 ->
      caml_of "float32" "int"
    | (Tagged_immediate | Naked_immediate), Naked_float -> caml_of "float" "int"
    | (Tagged_immediate | Naked_immediate), Naked_int32 -> caml_of "int32" "int"
    | (Tagged_immediate | Naked_immediate), Naked_int64 -> caml_of "int64" "int"
    | (Tagged_immediate | Naked_immediate), Naked_nativeint ->
      caml_of "nativeint" "int"
    | Naked_float32, (Tagged_immediate | Naked_immediate) ->
      caml_of "int" "float32"
    | Naked_float32, Naked_float -> caml_of "float" "float32"
    | Naked_float32, Naked_int32 -> caml_of "int32" "float32"
    | Naked_float32, Naked_int64 -> caml_to_bytecode "float32" "int64"
    | Naked_float32, Naked_nativeint -> caml_of "nativeint" "float32"
    | Naked_float, (Tagged_immediate | Naked_immediate) -> caml_of "int" "float"
    | Naked_float, Naked_float32 -> caml_of "float32" "float"
    | Naked_float, Naked_int32 -> caml_of "int32" "float"
    | Naked_float, Naked_int64 -> caml_of "int64" "float"
    | Naked_float, Naked_nativeint -> caml_of "nativeint" "float"
    | Naked_int32, (Tagged_immediate | Naked_immediate) -> caml_to "int32" "int"
    | Naked_int32, Naked_float32 -> caml_of "float32" "int32"
    | Naked_int32, Naked_float -> caml_to "int32" "float"
    | Naked_int32, Naked_int64 -> caml_of "int64" "int32"
    | Naked_int32, Naked_nativeint -> caml_of "nativeint" "int32"
    | Naked_int64, (Tagged_immediate | Naked_immediate) -> caml_to "int64" "int"
    | Naked_int64, Naked_float32 -> caml_of_bytecode "float32" "int64"
    | Naked_int64, Naked_float -> caml_to "int64" "float"
    | Naked_int64, Naked_int32 -> caml_to "int64" "int32"
    | Naked_int64, Naked_nativeint -> caml_to "int64" "nativeint"
    | Naked_nativeint, (Tagged_immediate | Naked_immediate) ->
      caml_to "nativeint" "int"
    | Naked_nativeint, Naked_float32 -> caml_of "float32" "nativeint"
    | Naked_nativeint, Naked_float -> caml_to "nativeint" "float"
    | Naked_nativeint, Naked_int32 -> caml_to "nativeint" "int32"
    | Naked_nativeint, Naked_int64 -> caml_of "int64" "nativeint")
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
    use_prim' (Extern (extern_name, None))
  | Reinterpret_boxed_vector -> identity ~env ~res x
  | Unbox_number _ | Box_number _ | Untag_immediate | Tag_immediate ->
    (* everything is untagged and "unboxed" in JS: see README *)
    identity ~env ~res x
  | Project_function_slot { move_from = _; move_to } ->
    check_my_closure ~env x;
    Some (To_jsir_env.get_function_slot_exn env move_to), env, res
  | Project_value_slot { project_from = _; value_slot } ->
    check_my_closure ~env x;
    Some (To_jsir_env.get_value_slot_exn env value_slot), env, res
  | Is_boxed_float ->
    (* This only arises from [Pmakearray Pgenarray], to decide whether an array
       literal of unknown element type is a flat float array or an ordinary one.
       A boxed float is a plain number in JavaScript, so they can't be
       distinguished, but the JSOO runtime already puts floats in ordinary
       arrays ([caml_array_make], so we return [false] according to this
       convention. *)
    let var = Jsir.Var.fresh () in
    let expr : Jsir.expr = Constant (Int Targetint.zero) in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Is_flat_float_array -> check_tag ~env ~res x ~tag:Obj.double_array_tag
  | End_region _ | End_try_region _ -> no_op ~env ~res
  | Get_header ->
    (* CR selee: check [js_of_ocaml/compiler/tests_check_prim/main.output], this
       primitive ("caml_get_header") seems to be missing from jsoo *)
    raise Primitive_not_supported
  | Peek _ ->
    (* Unsupported in bytecode *)
    raise Primitive_not_supported
  | Make_lazy { lazy_tag; alloc_region = _ } ->
    let tag = Flambda_primitive.Lazy_block_tag.to_tag lazy_tag in
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
    | ( ( Immediates | Gc_ignorable_values | Values | Naked_floats
        | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
        | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_masks
        | Unboxed_product _ ),
        ( Immediates | Gc_ignorable_values | Values | Naked_floats
        | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
        | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_masks ) ) ->
      use_prim' Array_get
    | (Naked_vec128s | Naked_vec256s | Naked_vec512s), _
    | _, (Naked_vec128s | Naked_vec256s | Naked_vec512s) ->
      (* No SIMD *)
      raise Primitive_not_supported)
  | String_or_bigstring_load (value, width) -> (
    (* Flambda inserts the bounds checks itself, so these loads can always use
       the unchecked primitives. *)
    let byte_load : Jsir.prim =
      match value with
      | String -> Extern ("caml_string_unsafe_get", None)
      | Bytes -> Extern ("caml_bytes_unsafe_get", None)
      | Bigstring -> Extern ("caml_ba_get_raw_unsafe", Some bigstring_hint)
    in
    let wide_load access = unsafe_string_like_access value access in
    match width with
    | Eight -> use_prim' byte_load
    | Eight_signed ->
      let var, env, res = use_prim0' ~env ~res byte_load [x; y] in
      sign_extend ~env ~res ~bits:8 (Pv var)
    | Sixteen -> use_prim' (wide_load Get16)
    | Sixteen_signed ->
      let var, env, res = use_prim0' ~env ~res (wide_load Get16) [x; y] in
      sign_extend ~env ~res ~bits:16 (Pv var)
    | Thirty_two -> use_prim' (wide_load Get32)
    | Single -> use_prim' (wide_load Getf32)
    | Sixty_four -> use_prim' (wide_load Get64)
    | One_twenty_eight _ | Two_fifty_six _ | Five_twelve _ | Mask ->
      raise Primitive_not_supported)
  | Bigarray_load (_dims, kind, layout) ->
    (* The index calculation is already done in Flambda, so the raw accessor
       only needs the resulting offset. *)
    use_prim'
      (Extern ("caml_ba_get_raw_unsafe", Some (bigarray_hint ~kind ~layout)))
  | Phys_equal comparison ->
    let prim : Jsir.prim = match comparison with Eq -> Eq | Neq -> Neq in
    use_prim' prim
  | Int_arith (kind, op) -> (
    let op_name =
      match op with
      | Add -> "add"
      | Sub -> "sub"
      | Mul -> "mul"
      | Div Signed -> "div"
      | Div Unsigned -> "unsigned_div"
      | Mod Signed -> "mod"
      | Mod Unsigned -> "unsigned_mod"
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
    in
    let prim : Jsir.prim =
      Extern (with_int_prefix ~kind op_name ~percent_for_imms:true, None)
    in
    match smallint_bits kind, op with
    | None, _ | Some _, (And | Or | Xor | Mod Signed) ->
      (* Bitwise operations on sign-extended operands yield sign-extended
         results, and a signed remainder is always smaller in magnitude than the
         divisor, so it is in range too. *)
      use_prim' prim
    | Some bits, (Add | Sub | Mul | Div Signed) ->
      (* The result may be out of range, e.g. [-128 / -1] for [int8] *)
      let var, env, res = use_prim0' ~env ~res prim [x; y] in
      sign_extend ~env ~res ~bits (Pv var)
    | Some bits, (Div Unsigned | Mod Unsigned) ->
      (* Zero-extend the operands to get their unsigned values *)
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
      let x, env, res = zero_extend ~env ~res ~bits x in
      let y, env, res = zero_extend ~env ~res ~bits y in
      let var, env, res = use_prim0 ~env ~res prim [Pv x; Pv y] in
      sign_extend ~env ~res ~bits (Pv var))
  | Int_shift (kind, op) -> (
    let op_name =
      match kind, op with
      | (Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16), Lsl ->
        "lsl"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsl -> "shift_left"
      | (Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16), Lsr ->
        "lsr"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsr ->
        "shift_right_unsigned"
      | (Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16), Asr ->
        "asr"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Asr -> "shift_right"
    in
    let prim : Jsir.prim =
      Extern (with_int_prefix ~kind op_name ~percent_for_imms:true, None)
    in
    match smallint_bits kind, op with
    | None, _ | Some _, Asr -> use_prim' prim
    | Some bits, Lsl ->
      let var, env, res = use_prim0' ~env ~res prim [x; y] in
      sign_extend ~env ~res ~bits (Pv var)
    | Some bits, Lsr ->
      (* Shift the unsigned value, then sign-extend (e.g. for a shift by 0) *)
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
      let x, env, res = zero_extend ~env ~res ~bits x in
      let var, env, res = use_prim0 ~env ~res prim [Pv x; y] in
      sign_extend ~env ~res ~bits (Pv var))
  | Int_comp (kind, behaviour) -> (
    match behaviour with
    | Yielding_bool comparison -> (
      let eq, neq, lt, ult, le =
        let open Jsir in
        match kind with
        | Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16
        | Naked_int32 | Naked_nativeint ->
          (* Sign extension preserves both signed and unsigned ordering, so
             smallints can be compared directly. 32-bit integers are plain JS
             numbers, so they don't need any hints either. *)
          Eq, Neq, Lt, Ult, Le
        | Naked_int64 ->
          let hint : Optimization_hint.ccall option = Some (Hint_int Int64) in
          ( Extern ("caml_equal", hint),
            Extern ("caml_notequal", hint),
            Extern ("caml_lessthan", hint),
            Extern ("caml_int64_ult", None),
            Extern ("caml_lessequal", hint) )
      in
      let unsigned_le x y =
        let var_ult, env, res = use_prim0 ~env ~res ult [x; y] in
        let var_eq, env, res = use_prim0 ~env ~res eq [x; y] in
        use_prim ~env ~res (Extern ("%int_or", None)) [Pv var_ult; Pv var_eq]
      in
      let x, res = prim_arg ~env ~res x in
      let y, res = prim_arg ~env ~res y in
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
      let compare : Jsir.prim =
        Extern (with_int_prefix ~kind "compare" ~percent_for_imms:false, None)
      in
      match signed_or_unsigned with
      | Signed -> use_prim' compare
      | Unsigned ->
        (* Map the operands to values whose signed ordering is the unsigned
           ordering of the originals. Smallints should be zero-extended, and the
           sign bit of the other kinds should be flipped. *)
        let flip_sign_bit (min_int : Jsir.constant) ~env ~res arg =
          use_prim0 ~env ~res
            (Extern (with_int_prefix ~kind "xor" ~percent_for_imms:true, None))
            [arg; Pc min_int]
        in
        let to_signed_order =
          match kind with
          | Naked_int8 -> zero_extend ~bits:8
          | Naked_int16 -> zero_extend ~bits:16
          | Tagged_immediate | Naked_immediate ->
            flip_sign_bit (Int (Targetint.min_int ()))
          | Naked_int32 | Naked_nativeint -> flip_sign_bit (Int32 Int32.min_int)
          | Naked_int64 -> flip_sign_bit (Int64 Int64.min_int)
        in
        let x, res = prim_arg ~env ~res x in
        let y, res = prim_arg ~env ~res y in
        let x, env, res = to_signed_order ~env ~res x in
        let y, env, res = to_signed_order ~env ~res y in
        use_prim ~env ~res compare [Pv x; Pv y]))
  | Float_arith (bitwidth, op) ->
    let op_name =
      match op with Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div"
    in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern (extern_name, None))
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
    use_prim' (Extern (extern_name, None))
  | Atomic_load (offset_units, _) ->
    atomic_field ~env ~res offset_units "caml_atomic_load_field" ~base:x
      ~offset:y []
  | Bigarray_get_alignment _ ->
    (* Only used for SIMD *)
    raise Primitive_not_supported
  | Poke _ ->
    (* Unsupported in bytecode *)
    raise Primitive_not_supported
  | Read_offset _ ->
    let base, env, res = checked_block_index_base ~env ~res x in
    let index, env, res = field_index_of_byte_offset ~env ~res y in
    use_prim ~env ~res Array_get [Pv base; Pv index]

let ternary_exn ~env ~res (f : Flambda_primitive.ternary_primitive) x y z =
  let use_prim' prim = use_prim' ~env ~res prim [x; y; z] in
  match f with
  | Array_set (kind, set_kind) -> (
    match kind, set_kind with
    | ( ( Immediates | Gc_ignorable_values | Values | Naked_floats
        | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
        | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_masks
        | Unboxed_product _ ),
        ( Immediates | Gc_ignorable_values | Values _ | Naked_floats
        | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
        | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_masks ) ) ->
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
    (* As for [String_or_bigstring_load], these stores are always unsafe. *)
    let string_like_value : Flambda_primitive.string_like_value =
      match value with Bytes -> Bytes | Bigstring -> Bigstring
    in
    let wide_store access =
      unsafe_string_like_access string_like_value access
    in
    let prim : Jsir.prim =
      match value, width with
      | _, One_twenty_eight _ | _, Two_fifty_six _ | _, Five_twelve _ | _, Mask
        ->
        (* No SIMD *)
        raise Primitive_not_supported
      | Bytes, (Eight | Eight_signed) -> Extern ("caml_bytes_unsafe_set", None)
      | Bigstring, (Eight | Eight_signed) ->
        Extern ("caml_ba_set_raw_unsafe", Some bigstring_hint)
      | _, (Sixteen | Sixteen_signed) -> wide_store Set16
      | _, Thirty_two -> wide_store Set32
      | _, Single -> wide_store Setf32
      | _, Sixty_four -> wide_store Set64
    in
    use_prim' prim
  | Bigarray_set (_dims, kind, layout) ->
    (* The index calculation is already done in Flambda, so the raw accessor
       only needs the resulting offset. *)
    use_prim'
      (Extern ("caml_ba_set_raw_unsafe", Some (bigarray_hint ~kind ~layout)))
  | Atomic_int_arith (offset_units, op) ->
    let extern_name =
      match op with
      | Fetch_add -> "caml_atomic_fetch_add_field"
      | Add -> "caml_atomic_add_field"
      | Sub -> "caml_atomic_sub_field"
      | And -> "caml_atomic_land_field"
      | Or -> "caml_atomic_lor_field"
      | Xor -> "caml_atomic_lxor_field"
    in
    atomic_field ~env ~res offset_units extern_name ~base:x ~offset:y [z]
  | Atomic_set (offset_units, _, _) ->
    let _var, env, res =
      atomic_field ~env ~res offset_units "caml_atomic_exchange_field" ~base:x
        ~offset:y [z]
    in
    unit ~env ~res
  | Atomic_exchange (offset_units, _, _) ->
    atomic_field ~env ~res offset_units "caml_atomic_exchange_field" ~base:x
      ~offset:y [z]
  | Write_offset _ ->
    let base, env, res = checked_block_index_base ~env ~res x in
    let index, env, res = field_index_of_byte_offset ~env ~res y in
    let new_value, res = To_jsir_shared.simple ~env ~res z in
    let res =
      To_jsir_result.add_instr_exn res (Array_set (base, index, new_value))
    in
    unit ~env ~res

let quaternary_exn ~env ~res (f : Flambda_primitive.quaternary_primitive) w x y
    z =
  match f with
  | Atomic_compare_and_set (offset_units, _, _) ->
    atomic_field ~env ~res offset_units "caml_atomic_cas_field" ~base:w
      ~offset:x [y; z]
  | Atomic_compare_exchange { offset_units; _ } ->
    atomic_field ~env ~res offset_units "caml_atomic_compare_exchange_field"
      ~base:w ~offset:x [y; z]

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
      | Immediates | Gc_ignorable_values | Values -> 0
      | Naked_ints ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.untagged_int_array_tag
      | Naked_int8s ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.untagged_int8_array_tag
          (List.length xs)
      | Naked_int16s ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.untagged_int16_array_tag
          (List.length xs)
      | Naked_int32s ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.unboxed_int32_array_tag
          (List.length xs)
      | Naked_int64s ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.unboxed_int64_array_tag
      | Naked_nativeints ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.unboxed_nativeint_array_tag
      | Naked_masks ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.unboxed_mask_array_tag
      | Naked_floats -> Tag.double_array_tag |> Tag.to_int
      | Naked_float32s ->
        Cmm_helpers.Unboxed_or_untagged_array_tags.unboxed_float32_array_tag
          (List.length xs)
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
  (* CR-someday selee: Bytecode attaches [Hint_primitive] to the calls of
     externals whose native name differs from the bytecode one, recording the
     name and the representations of the arguments and result. Flambda doesn't
     keep the native name so we don't emit the hint. This should eventually be
     threaded through. *)
  let var = Jsir.Var.fresh () in
  ( var,
    To_jsir_result.add_call_exn res
      (Let (var, Prim (Extern (name, None), args))) )
