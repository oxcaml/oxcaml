open! Flambda2_terms.Flambda.Import

let reg_width_const const : Jsir.constant =
  match Flambda2_identifiers.Reg_width_const.descr const with
  | Naked_immediate targetint ->
    let targetint = Flambda2_numbers.Targetint_31_63.to_targetint targetint in
    (* Jsir.Int targetint; *)
    failwith "hi"
  | Tagged_immediate targetint -> failwith "???"
  | Naked_float32 float32 ->
    (* CR selee: check *)
    Jsir.Float32
      (Int64.of_int32
         (Flambda2_numbers.Numeric_types.Float32_by_bit_pattern.to_bits float32))
  | Naked_float float ->
    Jsir.Float
      (Flambda2_numbers.Numeric_types.Float_by_bit_pattern.to_bits float)
  | Naked_int32 int32 -> Jsir.Int32 int32
  | Naked_int64 int64 -> Jsir.Int64 int64
  | Naked_nativeint nativeint ->
    (* CR selee: check *)
    Jsir.NativeInt (Flambda2_numbers.Targetint_32_64.to_int32 nativeint)
  | Null -> Jsir.Null
  | (Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _) as descr ->
    Misc.fatal_errorf "Unsupported constant %a"
      Flambda2_identifiers.Int_ids.Const.print const

(** Bind a JSIR variable to the result of translating a [Simple] into JSIR. *)
let simple ~env ~res fvar simple =
  let expr =
    Flambda2_term_basics.Simple.pattern_match' simple
      ~var:(fun name ~coercion -> failwith "var")
      ~symbol:(fun symbol ~coercion -> failwith "symbol")
      ~const:(fun const -> Jsir.Constant (reg_width_const const))
  in
  let jvar = Jsir.Var.fresh () in
  ( To_jsir_env.add_var env fvar jvar,
    To_jsir_result.add_instr res (Jsir.Let (jvar, expr)) )

let rec expr ~env ~res e =
  match Expr.descr e with
  | Let e' -> let_expr ~env ~res e'
  | Let_cont e' -> let_cont ~env ~res e'
  | Apply e' -> apply_expr ~env ~res e'
  | Apply_cont e' -> apply_cont ~env ~res e'
  | Switch e' -> switch ~env ~res e'
  | Invalid { message } -> invalid ~env ~res message

and let_expr ~env ~res e =
  Let.pattern_match' e
    ~f:(fun bound_pattern ~num_normal_occurrences_of_bound_vars ~body ->
      ignore num_normal_occurrences_of_bound_vars;
      match
        Flambda2_bound_identifiers.Bound_pattern.name_mode bound_pattern
      with
      | Normal ->
        let_expr_normal ~env ~res e ~bound_pattern
          ~num_normal_occurrences_of_bound_vars ~body
      | Phantom -> expr ~env ~res body
      | In_types ->
        Misc.fatal_errorf "Cannot bind In_types variables in terms:@ %a"
          Let.print e)

and let_expr_normal ~env ~res e
    ~(bound_pattern : Flambda2_bound_identifiers.Bound_pattern.t)
    ~num_normal_occurrences_of_bound_vars ~body =
  (* CR selee: translate to [Let] *)
  ignore num_normal_occurrences_of_bound_vars;
  match bound_pattern, Let.defining_expr e with
  | Singleton v, Simple s ->
    let fvar = Flambda2_bound_identifiers.Bound_var.var v in
    let env, res = simple ~env ~res fvar s in
    expr ~env ~res body
  | Singleton v, Prim (p, dbg) ->
    ignore (v, p, dbg);
    failwith "unimplemented"
  | Set_of_closures bound_vars, Set_of_closures soc ->
    ignore (bound_vars, soc);
    failwith "unimplemented"
  | Static bound_static, Static_consts consts ->
    ignore (bound_static, consts);
    failwith "unimplemented"
  | Singleton _, Rec_info _ -> expr ~env ~res body
  | Singleton _, (Set_of_closures _ | Static_consts _)
  | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
  | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
    Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
      Let.print e

and let_cont ~env ~res e =
  (* CR selee: probably make a new block *)
  ignore (env, res, e);
  failwith "unimplemented"

and apply_expr ~env ~res e =
  (* CR selee: translate to [Apply] *)
  ignore (env, res, e);
  failwith "unimplemented"

and apply_cont ~env ~res e =
  (* CR selee: branching at the end of the block. if raising,
     [Pushtrap]/[Posttrap] info comes from [Trap_action.t] *)
  ignore (env, res, e);
  failwith "unimplemented"

and switch ~env ~res e =
  (* CR selee: translate to [Switch], but beware that flambda allows arbitrary
     arms whereas Jsir requires 0..n (double check) *)
  ignore (env, res, e);
  failwith "unimplemented"

and invalid ~env ~res msg =
  (* CR selee: can probably ignore *)
  ignore (env, res, msg);
  failwith "unimplemented"

let unit flambda_unit =
  let env = To_jsir_env.create () in
  let res = To_jsir_result.create () in
  ignore (expr ~env ~res (Flambda2_terms.Flambda_unit.body flambda_unit));
  ignore flambda_unit;
  failwith "unimplemented"
