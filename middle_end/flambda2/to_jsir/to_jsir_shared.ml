let bind_expr_to_var' ~env ~res fvar expr =
  let jvar = Jsir.Var.fresh () in
  ( jvar,
    To_jsir_env.add_var env fvar jvar,
    To_jsir_result.add_instr_exn res (Jsir.Let (jvar, expr)) )

let bind_expr_to_var ~env ~res fvar expr =
  let _jvar, env, res = bind_expr_to_var' ~env ~res fvar expr in
  env, res

let bind_expr_to_symbol ~env ~res symbol expr =
  let jvar = Jsir.Var.fresh () in
  ( To_jsir_env.add_symbol env symbol jvar,
    To_jsir_result.add_instr_exn res (Jsir.Let (jvar, expr)) )

let reg_width_const const : Jsir.constant =
  match Reg_width_const.descr const with
  | Naked_immediate targetint | Tagged_immediate targetint ->
    (* CR selee: check that we can treat naked/tagged as same *)
    let repr = Targetint_31_63.to_targetint targetint |> Targetint_32_64.repr in
    let targetint =
      match repr with
      | Int32 int32 -> Targetint.of_int32 int32
      | Int64 int64 -> Targetint.of_int64 int64
    in
    Jsir.Int targetint
  | Naked_float32 float32 ->
    Jsir.Float32
      (Int64.of_int32 (Numeric_types.Float32_by_bit_pattern.to_bits float32))
  | Naked_float float ->
    Jsir.Float (Numeric_types.Float_by_bit_pattern.to_bits float)
  | Naked_int32 int32 -> Jsir.Int32 int32
  | Naked_int64 int64 -> Jsir.Int64 int64
  | Naked_nativeint nativeint ->
    Jsir.NativeInt (Targetint_32_64.to_int32 nativeint)
  | Null -> Jsir.Null
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ ->
    Misc.fatal_errorf "Unsupported constant %a" Int_ids.Const.print const

let simple ~env ~res simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ -> To_jsir_env.get_var_exn env name, res)
    ~symbol:(fun symbol ~coercion:_ ->
      To_jsir_env.get_symbol_exn env symbol, res)
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
  (* CR selee: is it ok to ignore shape? *)
  let tag = Tag.to_int tag in
  let mutability : Jsir.mutability =
    match (mut : Mutability.t) with
    | Mutable -> Maybe_mutable
    | Immutable -> Immutable
    | Immutable_unique ->
      (* CR selee: check *)
      Immutable
  in
  let fields, res = simples ~env ~res fields in
  Block (tag, Array.of_list fields, NotArray, mutability), env, res
