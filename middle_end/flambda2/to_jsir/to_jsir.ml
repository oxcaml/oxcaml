open! Flambda2_terms.Flambda.Import

let rec expr ~env ~res e =
  match Expr.descr e with
  | Let e' -> let_expr ~env ~res e'
  | Let_cont e' -> let_cont ~env ~res e'
  | Apply e' -> apply_expr ~env ~res e'
  | Apply_cont e' -> apply_cont ~env ~res e'
  | Switch e' -> switch ~env ~res e'
  | Invalid { message } -> invalid ~env ~res message

and let_expr ~env ~res e =
  (* CR selee: translate to [Let] *)
  ignore (env, res, e);
  failwith "unimplemented"

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
