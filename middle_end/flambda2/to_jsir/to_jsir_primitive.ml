let primitive_not_supported () =
  Misc.fatal_error "This primitive is not yet supported. "

(** Convert a [Simple.t] into a [Jsir.prim_arg]. *)
let prim_arg ~env simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ -> Jsir.Pv (To_jsir_env.get_var_exn env name))
    ~symbol:(fun symbol ~coercion:_ ->
      Jsir.Pv (To_jsir_env.get_symbol_exn env symbol))
    ~const:(fun const -> Jsir.Pc (To_jsir_shared.reg_width_const const))

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

let use_prim ~env ~res prim args =
  let expr : Jsir.expr = Prim (prim, args) in
  let var = Jsir.Var.fresh () in
  Some var, env, To_jsir_result.add_instr_exn res (Jsir.Let (var, expr))

(* CR selee: implement primitives *)

let nullary ~env ~res (f : Flambda_primitive.nullary_primitive) =
  ignore (env, res);
  match f with
  | Invalid kind ->
    ignore kind;
    primitive_not_supported ()
  | Optimised_out kind ->
    ignore kind;
    primitive_not_supported ()
  | Probe_is_enabled { name } ->
    ignore name;
    primitive_not_supported ()
  | Enter_inlined_apply { dbg } ->
    ignore dbg;
    primitive_not_supported ()
  | Dls_get -> primitive_not_supported ()
  | Poll -> primitive_not_supported ()
  | Cpu_relax -> primitive_not_supported ()

let unary ~env ~res (f : Flambda_primitive.unary_primitive) x =
  let use_prim' prim = use_prim ~env ~res prim [prim_arg ~env x] in
  match f with
  | Block_load { kind = _; mut = _; field } ->
    let var = Jsir.Var.fresh () in
    let expr : Jsir.expr =
      match prim_arg ~env x with
      | Pv v -> Field (v, Targetint_31_63.to_int field, Non_float)
      | Pc _ -> Misc.fatal_error "Block_load on constant"
    in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Duplicate_block _ | Duplicate_array _ | Obj_dup ->
    use_prim' (Extern "caml_obj_dup")
  | Is_int _ -> use_prim' IsInt
  | Is_null -> use_prim ~env ~res Eq [prim_arg ~env x; Pc Null]
  | Get_tag ->
    let var = Jsir.Var.fresh () in
    let expr : Jsir.expr =
      match prim_arg ~env x with
      | Pv v -> Field (v, 0, Non_float)
      | Pc _ -> Misc.fatal_error "Get_tag on constant"
    in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Array_length _ -> use_prim' Vectlength
  | Bigarray_length { dimension } ->
    use_prim ~env ~res (Extern "caml_ba_dim")
      [prim_arg ~env x; Pc (Int (Targetint.of_int dimension))]
  | String_length _ -> use_prim' (Extern "caml_ml_string_length")
  | Int_as_pointer mode ->
    ignore mode;
    (* CR selee: [js_of_ocaml/compiler/tests-check-prim/main.5.4.output] seems
       to suggest the runtime function is omitted by choice? *)
    primitive_not_supported ()
  | Opaque_identity { middle_end_only; kind = _ } -> (
    match middle_end_only, prim_arg ~env x with
    | true, _ -> no_op ~env ~res
    | false, Pv v -> Some v, env, res
    | false, Pc c ->
      let var = Jsir.Var.fresh () in
      ( Some var,
        env,
        To_jsir_result.add_instr_exn res (Jsir.Let (var, Constant c)) ))
  | Int_arith (kind, op) ->
    let op_name = match op with Swap_byte_endianness -> "bswap" in
    let extern_name = with_int_prefix ~kind op_name ~percent_for_imms:false in
    use_prim' (Extern extern_name)
  | Float_arith (bitwidth, op) ->
    let op_name = match op with Abs -> "abs" | Neg -> "neg" in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern extern_name)
  | Num_conv { src; dst } ->
    ignore (src, dst);
    primitive_not_supported ()
  | Boolean_not -> use_prim' Not
  | Reinterpret_64_bit_word reinterpret ->
    let extern_name =
      match reinterpret with
      | Unboxed_int64_as_unboxed_float64 -> "caml_int64_float_of_bits"
      | Unboxed_float64_as_unboxed_int64 -> "caml_int64_bits_of_float"
      | Unboxed_int64_as_tagged_int63 -> primitive_not_supported ()
      | Tagged_int63_as_unboxed_int64 -> primitive_not_supported ()
    in
    use_prim' (Extern extern_name)
  | Unbox_number kind ->
    ignore kind;
    primitive_not_supported ()
  | Box_number (kind, mode) ->
    ignore (kind, mode);
    primitive_not_supported ()
  | Untag_immediate -> primitive_not_supported ()
  | Tag_immediate -> primitive_not_supported ()
  | Project_function_slot { move_from = _; move_to } ->
    Some (To_jsir_env.get_function_slot_exn env move_to), env, res
  | Project_value_slot { project_from = _; value_slot } ->
    (* CR selee: This is also used to call external functions, will need to
       handle that *)
    Some (To_jsir_env.get_value_slot_exn env value_slot), env, res
  | Is_boxed_float -> primitive_not_supported ()
  | Is_flat_float_array -> primitive_not_supported ()
  | End_region _ | End_try_region _ -> no_op ~env ~res
  | Get_header -> primitive_not_supported ()
  | Atomic_load _ -> use_prim' (Extern "caml_atomic_load")
  | Peek kind ->
    ignore kind;
    primitive_not_supported ()
  | Make_lazy tag ->
    let tag =
      match
        Flambda_primitive.Lazy_block_tag.to_tag tag |> Tag.Scannable.of_tag
      with
      | None -> Misc.fatal_error "Lazy tag is not scannable"
      | Some tag -> tag
    in
    let expr, env, res =
      To_jsir_shared.block ~env ~res ~tag ~mut:Mutable ~fields:[x]
    in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))

let binary ~env ~res (f : Flambda_primitive.binary_primitive) x y =
  let use_prim' prim =
    use_prim ~env ~res prim [prim_arg ~env x; prim_arg ~env y]
  in
  match f with
  | Block_set { kind = _; init = _; field } ->
    let x =
      match prim_arg ~env x with
      | Pc _ -> Misc.fatal_error "Block_set on constant"
      | Pv x -> x
    in
    let y, res =
      match prim_arg ~env y with
      | Pv y -> y, res
      | Pc c ->
        let var = Jsir.Var.fresh () in
        var, To_jsir_result.add_instr_exn res (Let (var, Constant c))
    in
    ( None,
      env,
      To_jsir_result.add_instr_exn res
        (Set_field (x, Targetint_31_63.to_int field, Non_float, y)) )
  | Array_load (kind, load_kind, mut) ->
    ignore (kind, load_kind, mut);
    primitive_not_supported ()
  | String_or_bigstring_load (value, width) ->
    ignore (value, width);
    primitive_not_supported ()
  | Bigarray_load (dims, kind, layout) ->
    ignore (dims, kind, layout);
    primitive_not_supported ()
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
      let x = prim_arg ~env x in
      let y = prim_arg ~env y in
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
    | Yielding_int_like_compare_functions signed_or_unsigned ->
      let env, res =
        match signed_or_unsigned with
        | Signed -> env, res
        | Unsigned ->
          (* Also unimplemented in Cmm. See [To_cmm_primitive]. *)
          primitive_not_supported ()
      in
      let extern_name =
        with_int_prefix ~kind ~percent_for_imms:false "compare"
      in
      use_prim ~env ~res (Extern extern_name) [prim_arg ~env x])
  | Float_arith (bitwidth, op) ->
    let op_name =
      match op with Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div"
    in
    let extern_name = with_float_suffix ~bitwidth op_name in
    use_prim' (Extern extern_name)
  | Float_comp (bitwidth, behaviour) ->
    ignore (bitwidth, behaviour);
    primitive_not_supported ()
  | Bigarray_get_alignment dim ->
    ignore dim;
    primitive_not_supported ()
  | Atomic_set kind ->
    ignore kind;
    primitive_not_supported ()
  | Atomic_exchange kind ->
    ignore kind;
    primitive_not_supported ()
  | Atomic_int_arith op ->
    ignore op;
    primitive_not_supported ()
  | Poke kind ->
    ignore kind;
    primitive_not_supported ()

let ternary ~env ~res (f : Flambda_primitive.ternary_primitive) x y z =
  ignore (env, res, x, y, z);
  match f with
  | Array_set (kind, set_kind) ->
    ignore (kind, set_kind);
    primitive_not_supported ()
  | Bytes_or_bigstring_set (value, width) ->
    ignore (value, width);
    primitive_not_supported ()
  | Bigarray_set (dims, kind, layout) ->
    ignore (dims, kind, layout);
    primitive_not_supported ()
  | Atomic_compare_and_set kind ->
    ignore kind;
    primitive_not_supported ()
  | Atomic_compare_exchange { atomic_kind; args_kind } ->
    ignore (atomic_kind, args_kind);
    primitive_not_supported ()

let variadic ~env ~res (f : Flambda_primitive.variadic_primitive) xs =
  ignore (env, res, xs);
  match f with
  | Begin_region _ | Begin_try_region _ -> no_op ~env ~res
  | Make_block (kind, mut, _alloc_mode) ->
    let tag =
      match kind with
      | Values (tag, _with_subkind) -> tag
      | Naked_floats | Mixed _ -> failwith "unimplemented block kind"
    in
    let expr, env, res = To_jsir_shared.block ~env ~res ~tag ~mut ~fields:xs in
    let var = Jsir.Var.fresh () in
    Some var, env, To_jsir_result.add_instr_exn res (Let (var, expr))
  | Make_array (kind, mut, mode) ->
    ignore (kind, mut, mode);
    primitive_not_supported ()

let primitive ~env ~res (prim : Flambda_primitive.t) _dbg =
  match prim with
  | Nullary f -> nullary ~env ~res f
  | Unary (f, x) -> unary ~env ~res f x
  | Binary (f, x, y) -> binary ~env ~res f x y
  | Ternary (f, x, y, z) -> ternary ~env ~res f x y z
  | Variadic (f, xs) -> variadic ~env ~res f xs
