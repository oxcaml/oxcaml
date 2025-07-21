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
  List.fold_left
    (fun (vars, res) s ->
      let var, res = simple ~env ~res s in
      var :: vars, res)
    ([], res) simples
