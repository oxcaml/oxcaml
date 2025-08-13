let primitive_not_supported () =
  Misc.fatal_error
    "This primitive is not supported for JavaScript/WASM compilation."

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
    | (Tagged_immediate | Naked_immediate), true -> "%int"
    | (Tagged_immediate | Naked_immediate), false -> "caml_int"
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

let no_op ~env ~res = None, env, res

let identity ~env ~res x =
  let var, res = To_jsir_shared.simple ~env ~res x in
  Some var, env, res

let unit ~env ~res =
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Let (var, Constant Null))

let use_prim ~env ~res prim args =
  let expr : Jsir.expr = Prim (prim, args) in
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Jsir.Let (var, expr))

let use_prim' ~env ~res prim simples =
  let args, res = prim_args ~env ~res simples in
  use_prim ~env ~res prim args

let nullary ~env ~res (f : Flambda_primitive.nullary_primitive) =
  let use_prim' prim = use_prim' ~env ~res prim [] in
  match f with
  | Invalid _ -> use_prim' (Extern "caml_invalid_primitive")
  | Optimised_out _ ->
    (* For phantom lets, which are kept around for debugging information for
       pieces of code that were optimised away. *)
    no_op ~env ~res
  | Probe_is_enabled _ -> primitive_not_supported ()
  | Enter_inlined_apply _ ->
    (* CR selee: we should eventually use this debuginfo *)
    no_op ~env ~res
  | Dls_get -> use_prim' (Extern "caml_domain_dls_get")
  | Poll ->
    (* See [parse_bytecode.ml] in jsoo - treated as a noop *)
    no_op ~env ~res
  | Cpu_relax -> use_prim' (Extern "caml_ml_domain_cpu_relax")

let get_tag ~env ~res x =
  let var = Jsir.Var.fresh () in
  let expr, res =
    match prim_arg ~env ~res x with
    | Pv v, res -> Jsir.Field (v, 0, Non_float), res
    | Pc _, _res -> Misc.fatal_error "Get_tag on constant"
  in
  var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let check_tag ~env ~res x ~tag =
  let tag_var, env, res = get_tag ~env ~res x in
  let expr : Jsir.expr =
    Prim (Eq, [Pv tag_var; Pc (Int (Targetint.of_int tag))])
  in
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let block_access_kind (kind : Flambda_primitive.Block_access_kind.t) :
    Jsir.field_type =
  match kind with
  | Values _ -> Non_float
  | Naked_floats _ -> Float
  | Mixed { field_kind = Value_prefix _; _ } -> Non_float
  | Mixed
      { field_kind = Flat_suffix (Naked_int32 | Naked_int64 | Naked_nativeint);
        _
      } ->
    Non_float
  | Mixed { field_kind = Flat_suffix (Naked_float | Naked_float32); _ } -> Float
  | Mixed
      { field_kind = Flat_suffix (Naked_vec128 | Naked_vec256 | Naked_vec512);
        _
      } ->
    primitive_not_supported ()

let unary ~env ~res (f : Flambda_primitive.unary_primitive) x =
  let use_prim' prim = use_prim' ~env ~res prim [x] in
  match f with
  | Block_load { kind; mut = _; field } ->
    let var = Jsir.Var.fresh () in
    let expr, res =
      match prim_arg ~env ~res x with
      | Pv v, res ->
        ( Jsir.Field (v, Targetint_31_63.to_int field, block_access_kind kind),
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
      [x; Pc (Int (Targetint.of_int dimension))]
  | String_length _ -> use_prim' (Extern "caml_ml_string_length")
  | Int_as_pointer _ -> use_prim' (Extern "caml_int_as_pointer")
  | Opaque_identity { middle_end_only = _; kind = _ } ->
    (* CR selee: treating these as the identity for now *)
    identity ~env ~res x
  | Int_arith (kind, op) ->
    let op_name = match op with Swap_byte_endianness -> "bswap" in
    let extern_name = with_int_prefix ~kind op_name ~percent_for_imms:false in
    use_prim' (Extern extern_name)
  | Float_arith (bitwidth, op) ->
    let op_name = match op with Abs -> "abs" | Neg -> "neg" in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern extern_name)
  | Num_conv { src; dst } -> (
    let caml_of src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_of_%s" src dst))
    in
    let caml_of_bytecode src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_of_%s_bytecode" src dst))
    in
    let caml_to src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_to_%s" src dst))
    in
    let caml_to_bytecode src dst =
      use_prim' (Extern (Format.sprintf "caml_%s_to_%s_bytecode" src dst))
    in
    match src, dst with
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
      caml_to "float32" "int"
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
    | Naked_nativeint, Naked_int64 -> caml_to "nativeint" "int64")
  | Boolean_not -> use_prim' Not
  | Reinterpret_64_bit_word reinterpret ->
    let extern_name =
      match reinterpret with
      | Unboxed_int64_as_unboxed_float64 -> "caml_int64_float_of_bits"
      | Unboxed_float64_as_unboxed_int64 -> "caml_int64_bits_of_float"
      | Unboxed_int64_as_tagged_int63 ->
        (* JS doesn't have tagged int63 since it's a 32-bit target *)
        primitive_not_supported ()
      | Tagged_int63_as_unboxed_int64 -> primitive_not_supported ()
    in
    use_prim' (Extern extern_name)
  | Unbox_number _ | Box_number _ | Untag_immediate | Tag_immediate ->
    (* everything is boxed and tagged in JS *)
    identity ~env ~res x
  | Project_function_slot { move_from = _; move_to } ->
    Some (To_jsir_env.get_function_slot_exn env move_to), env, res
  | Project_value_slot { project_from = _; value_slot }
    when Value_slot.is_imported value_slot ->
    let str = Value_slot.to_string value_slot in
    Misc.fatal_errorf "project value slot %s\n" str
  | Project_value_slot { project_from = _; value_slot } ->
    (* CR selee: This is also used to call external functions, will need to
       handle that *)
    Some (To_jsir_env.get_value_slot_exn env value_slot), env, res
  | Is_boxed_float -> check_tag ~env ~res x ~tag:Obj.double_tag
  | Is_flat_float_array -> check_tag ~env ~res x ~tag:Obj.double_array_tag
  | End_region _ | End_try_region _ -> no_op ~env ~res
  | Get_header ->
    (* CR selee: check [js_of_ocaml/compiler/tests_check_prim/main.output], this
       primitive ("caml_get_header") seems to be missing from jsoo *)
    primitive_not_supported ()
  | Atomic_load _ -> use_prim' (Extern "caml_atomic_load")
  | Peek _ ->
    (* Unsupported in bytecode *)
    primitive_not_supported ()
  | Make_lazy tag ->
    let tag = Flambda_primitive.Lazy_block_tag.to_tag tag in
    let expr, env, res =
      To_jsir_shared.block ~env ~res ~tag ~mut:Mutable ~fields:[x]
    in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let binary ~env ~res (f : Flambda_primitive.binary_primitive) x y =
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
        (Set_field (x, Targetint_31_63.to_int field, block_access_kind kind, y))
    )
  | Array_load (kind, load_kind, _mut) -> (
    match kind, load_kind with
    | ( (Immediates | Values | Naked_floats | Naked_float32s),
        (Immediates | Values | Naked_floats | Naked_float32s) ) ->
      use_prim' Array_get
    | ( ( Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
        | Naked_vec256s | Naked_vec512s | Unboxed_product _ ),
        _ )
    | ( _,
        ( Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
        | Naked_vec256s | Naked_vec512s ) ) ->
      (* No SIMD *)
      primitive_not_supported ())
  | String_or_bigstring_load (value, width) ->
    let op_name =
      match width with
      | Eight -> "unsafe_get"
      | Sixteen -> "get16"
      | Thirty_two -> "get32"
      | Single -> "getf32"
      | Sixty_four -> "get64"
      | One_twenty_eight _ -> primitive_not_supported ()
    in
    let extern_name =
      match value with
      | String -> "caml_string_" ^ op_name
      | Bytes -> "caml_bytes_" ^ op_name
      | Bigstring -> (
        match width with
        | Eight -> "caml_ba_get_1"
        | Sixteen | Thirty_two | Single | Sixty_four | One_twenty_eight _ ->
          "caml_ba_uint8_" ^ op_name)
    in
    use_prim' (Extern extern_name)
  | Bigarray_load (dims, kind, _layout) ->
    let extern_prefix =
      match kind with
      | Float32_t -> "caml_ba_float32_get_"
      | Float16 | Float32 | Float64 | Sint8 | Uint8 | Sint16 | Uint16 | Int32
      | Int64 | Int_width_int | Targetint_width_int | Complex32 | Complex64 ->
        "caml_ba_get_"
    in
    use_prim' (Extern (extern_prefix ^ Int.to_string dims))
  | Phys_equal comparison ->
    let prim : Jsir.prim = match comparison with Eq -> Eq | Neq -> Neq in
    use_prim' prim
  | Int_arith (kind, op) ->
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
    let extern_name = with_int_prefix ~kind op_name ~percent_for_imms:true in
    use_prim' (Extern extern_name)
  | Int_shift (kind, op) ->
    let op_name =
      match kind, op with
      | (Tagged_immediate | Naked_immediate), Lsl -> "lsl"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsl -> "shift_left"
      | (Tagged_immediate | Naked_immediate), Lsr -> "lsr"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsr ->
        "shift_right_unsigned"
      | (Tagged_immediate | Naked_immediate), Asr -> "shift_right"
      | (Naked_int32 | Naked_int64 | Naked_nativeint), Asr -> "shift_right"
    in
    let extern_name = with_int_prefix ~kind op_name ~percent_for_imms:true in
    use_prim' (Extern extern_name)
  | Int_comp (kind, behaviour) -> (
    match behaviour with
    | Yielding_bool comparison -> (
      let unsigned_le x y =
        let var_ule = Jsir.Var.fresh () in
        let var_eq = Jsir.Var.fresh () in
        let var_or = Jsir.Var.fresh () in
        let expr_ule : Jsir.expr = Prim (Ult, [x; y]) in
        let expr_eq : Jsir.expr = Prim (Eq, [x; y]) in
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
      match comparison with
      | Eq -> use_prim ~env ~res Eq [x; y]
      | Neq -> use_prim ~env ~res Neq [x; y]
      | Lt Signed -> use_prim ~env ~res Lt [x; y]
      | Lt Unsigned -> use_prim ~env ~res Ult [x; y]
      | Gt Signed -> use_prim ~env ~res Lt [y; x]
      | Gt Unsigned -> use_prim ~env ~res Ult [y; x]
      | Le Signed -> use_prim ~env ~res Le [x; y]
      | Le Unsigned -> unsigned_le x y
      | Ge Signed -> use_prim ~env ~res Le [y; x]
      | Ge Unsigned -> unsigned_le y x)
    | Yielding_int_like_compare_functions signed_or_unsigned -> (
      match signed_or_unsigned with
      | Signed ->
        let extern_name =
          with_int_prefix ~kind "compare" ~percent_for_imms:false
        in
        use_prim' (Extern extern_name)
      | Unsigned ->
        (* Also unimplemented in Cmm. See [To_cmm_primitive]. *)
        (* CR selee: can do this by subtracting [min_int] before doing the
           compare *)
        primitive_not_supported ()))
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
  | Bigarray_get_alignment _ ->
    (* Only used for SIMD *)
    primitive_not_supported ()
  | Atomic_set _ ->
    let _var, env, res = use_prim' (Extern "caml_atomic_exchange") in
    unit ~env ~res
  | Atomic_exchange _ -> use_prim' (Extern "caml_atomic_exchange")
  | Atomic_int_arith op ->
    let extern_name =
      match op with
      | Fetch_add -> "caml_atomic_fetch_add"
      | Add -> "caml_atomic_add"
      | Sub -> "caml_atomic_sub"
      | And -> "caml_atomic_land"
      | Or -> "caml_atomic_lor"
      | Xor -> "caml_atomic_lxor"
    in
    use_prim' (Extern extern_name)
  | Poke _ ->
    (* Unsupported in bytecode *)
    primitive_not_supported ()

let ternary ~env ~res (f : Flambda_primitive.ternary_primitive) x y z =
  let use_prim' prim = use_prim' ~env ~res prim [x; y; z] in
  match f with
  | Array_set (kind, set_kind) -> (
    match kind, set_kind with
    | ( (Immediates | Values | Naked_floats | Naked_float32s),
        (Immediates | Values _ | Naked_floats | Naked_float32s) ) ->
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
    | ( ( Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
        | Naked_vec256s | Naked_vec512s | Unboxed_product _ ),
        _ )
    | ( _,
        ( Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
        | Naked_vec256s | Naked_vec512s ) ) ->
      (* No SIMD *)
      primitive_not_supported ())
  | Bytes_or_bigstring_set (value, width) ->
    let extern_name =
      match value, width with
      | _, One_twenty_eight _ ->
        (* No SIMD *)
        primitive_not_supported ()
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
  | Bigarray_set (dims, kind, _layout) ->
    let extern_prefix =
      match kind with
      | Float32_t -> "caml_ba_float32_set_"
      | Float16 | Float32 | Float64 | Sint8 | Uint8 | Sint16 | Uint16 | Int32
      | Int64 | Int_width_int | Targetint_width_int | Complex32 | Complex64 ->
        "caml_ba_set_"
    in
    use_prim' (Extern (extern_prefix ^ Int.to_string dims))
  | Atomic_compare_and_set _ -> use_prim' (Extern "caml_atomic_cas")
  | Atomic_compare_exchange _ ->
    use_prim' (Extern "caml_atomic_compare_exchange")

let variadic ~env ~res (f : Flambda_primitive.variadic_primitive) xs =
  match f with
  | Begin_region _ | Begin_try_region _ -> no_op ~env ~res
  | Make_block (kind, mut, _alloc_mode) ->
    let tag =
      match kind with
      | Values (tag, _with_subkind) -> Tag.Scannable.to_tag tag
      | Naked_floats -> Tag.double_array_tag
      | Mixed _ -> failwith "unimplemented block kind"
    in
    let expr, env, res = To_jsir_shared.block ~env ~res ~tag ~mut ~fields:xs in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Make_array (kind, mut, _mode) ->
    let tag =
      match kind with
      | Immediates | Values -> Tag.zero
      | Naked_floats | Naked_float32s -> Tag.double_array_tag
      | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
      | Naked_vec256s | Naked_vec512s | Unboxed_product _ ->
        (* No SIMD *)
        primitive_not_supported ()
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
        (Let (var, Block (Tag.to_int tag, Array.of_list xs, Array, mutability)))
    )

let primitive ~env ~res (prim : Flambda_primitive.t) _dbg =
  match prim with
  | Nullary f -> nullary ~env ~res f
  | Unary (f, x) -> unary ~env ~res f x
  | Binary (f, x, y) -> binary ~env ~res f x y
  | Ternary (f, x, y, z) -> ternary ~env ~res f x y z
  | Variadic (f, xs) -> variadic ~env ~res f xs

let extern ~env ~res symbol args =
  let args, res = prim_args ~env ~res args in
  let name = Symbol.linkage_name_as_string symbol in
  let var = Jsir.Var.fresh () in
  var, To_jsir_result.add_instr_exn res (Let (var, Prim (Extern name, args)))
