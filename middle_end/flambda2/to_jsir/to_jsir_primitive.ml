let primitive_not_supported () =
  Misc.fatal_error "This primitive is not yet supported. "

(** Convert a [Simple.t] into a [Jsir.prim_arg]. *)
let prim_arg ~env simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ -> Jsir.Pv (To_jsir_env.get_var_exn env name))
    ~symbol:(fun symbol ~coercion:_ ->
      Jsir.Pv (To_jsir_env.get_symbol_exn env symbol))
    ~const:(fun const -> Jsir.Pc (To_jsir_shared.reg_width_const const))

let with_int_prefix (kind : Flambda_kind.Standard_int.t) ~percent_for_imms op =
  let prefix =
    match kind, percent_for_imms with
    | (Tagged_immediate | Naked_immediate), true -> "%int"
    | (Tagged_immediate | Naked_immediate), false -> "caml_int"
    | Naked_int32, _ -> "caml_int32"
    | Naked_int64, _ -> "caml_int64"
    | Naked_nativeint, _ -> "caml_nativeint"
  in
  prefix ^ "_" ^ op

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
  ignore (env, res, x);
  match f with
  | Block_load { kind; mut; field } ->
    ignore (kind, mut, field);
    primitive_not_supported ()
  | Duplicate_block { kind } ->
    ignore kind;
    primitive_not_supported ()
  | Duplicate_array { kind; source_mutability; destination_mutability } ->
    ignore (kind, source_mutability, destination_mutability);
    primitive_not_supported ()
  | Is_int { variant_only } ->
    ignore variant_only;
    primitive_not_supported ()
  | Is_null -> primitive_not_supported ()
  | Get_tag -> primitive_not_supported ()
  | Array_length kind ->
    ignore kind;
    primitive_not_supported ()
  | Bigarray_length { dimension } ->
    ignore dimension;
    primitive_not_supported ()
  | String_length str ->
    ignore str;
    primitive_not_supported ()
  | Int_as_pointer mode ->
    ignore mode;
    primitive_not_supported ()
  | Opaque_identity { middle_end_only; kind } ->
    ignore (middle_end_only, kind);
    primitive_not_supported ()
  | Int_arith (kind, op) ->
    ignore (kind, op);
    primitive_not_supported ()
  | Float_arith (bitwidth, op) ->
    ignore (bitwidth, op);
    primitive_not_supported ()
  | Num_conv { src; dst } ->
    ignore (src, dst);
    primitive_not_supported ()
  | Boolean_not -> primitive_not_supported ()
  | Reinterpret_64_bit_word op ->
    ignore op;
    primitive_not_supported ()
  | Unbox_number kind ->
    ignore kind;
    primitive_not_supported ()
  | Box_number (kind, mode) ->
    ignore (kind, mode);
    primitive_not_supported ()
  | Untag_immediate -> primitive_not_supported ()
  | Tag_immediate -> primitive_not_supported ()
  | Project_function_slot { move_from = _; move_to } ->
    To_jsir_env.get_function_slot_exn env move_to, env, res
  | Project_value_slot { project_from = _; value_slot } ->
    To_jsir_env.get_value_slot_exn env value_slot, env, res
  | Is_boxed_float -> primitive_not_supported ()
  | Is_flat_float_array -> primitive_not_supported ()
  | End_region { ghost } ->
    ignore ghost;
    primitive_not_supported ()
  | End_try_region { ghost } ->
    ignore ghost;
    primitive_not_supported ()
  | Obj_dup -> primitive_not_supported ()
  | Get_header -> primitive_not_supported ()
  | Atomic_load kind ->
    ignore kind;
    primitive_not_supported ()
  | Peek kind ->
    ignore kind;
    primitive_not_supported ()
  | Make_lazy tag ->
    ignore tag;
    primitive_not_supported ()

let binary ~env ~res (f : Flambda_primitive.binary_primitive) x y =
  let use_prim' ~env ~res prim x y =
    let expr : Jsir.expr = Prim (prim, [x; y]) in
    let var = Jsir.Var.fresh () in
    var, env, To_jsir_result.add_instr_exn res (Jsir.Let (var, expr))
  in
  let use_prim ~env ~res prim = use_prim' ~env ~res prim x y in
  match f with
  | Block_set { kind; init; field } ->
    ignore (kind, init, field);
    primitive_not_supported ()
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
    use_prim ~env ~res prim
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
    let extern_name = with_int_prefix kind op_name ~percent_for_imms:true in
    use_prim ~env ~res (Extern extern_name)
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
    let extern_name = with_int_prefix kind op_name ~percent_for_imms:true in
    use_prim ~env ~res (Extern extern_name)
  | Int_comp (kind, behaviour) -> (
    match behaviour with
    | Yielding_bool comparison -> (
      match comparison with
      | Eq -> use_prim' ~env ~res Eq x y
      | Neq -> use_prim' ~env ~res Neq x y
      | Lt Signed -> use_prim' ~env ~res Lt x y
      | Lt Unsigned -> use_prim' ~env ~res Ult x y
      | Gt Signed -> use_prim' ~env ~res Lt y x
      | Gt Unsigned -> use_prim' ~env ~res Ult y x
      | Le Signed -> use_prim' ~env ~res Le x y
      | Le Unsigned -> failwith "bruh"
      | Ge Signed -> use_prim' ~env ~res Le y x
      | Ge Unsigned -> failwith "bruh")
    | Yielding_int_like_compare_functions signed_or_unsigned ->
      let env, res =
        match signed_or_unsigned with
        | Signed -> env, res
        | Unsigned -> failwith "bruh"
      in
      let extern_name =
        with_int_prefix kind "compare" ~percent_for_imms:false
      in
      use_prim ~env ~res (Extern extern_name))
  | Float_arith (bitwidth, op) ->
    ignore (bitwidth, op);
    primitive_not_supported ()
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
  | Begin_region { ghost } ->
    ignore ghost;
    primitive_not_supported ()
  | Begin_try_region { ghost } ->
    ignore ghost;
    primitive_not_supported ()
  | Make_block (kind, mut, mode) ->
    ignore (kind, mut, mode);
    primitive_not_supported ()
  | Make_array (kind, mut, mode) ->
    ignore (kind, mut, mode);
    primitive_not_supported ()

let primitive ~env ~res (prim : Flambda_primitive.t) _dbg =
  match prim with
  | Nullary f -> nullary ~env ~res f
  | Unary (f, x) ->
    Simple.pattern_match x
      ~name:(fun name ~coercion:_ ->
        print_endline "name: ==";
        Name.print Format.std_formatter name)
      ~const:ignore;
    unary ~env ~res f (prim_arg ~env x)
  | Binary (f, x, y) -> binary ~env ~res f (prim_arg ~env x) (prim_arg ~env y)
  | Ternary (f, x, y, z) ->
    ternary ~env ~res f (prim_arg ~env x) (prim_arg ~env y) (prim_arg ~env z)
  | Variadic (f, xs) -> variadic ~env ~res f (List.map (prim_arg ~env) xs)
