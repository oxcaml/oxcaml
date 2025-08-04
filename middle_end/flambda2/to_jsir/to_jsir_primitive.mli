(** Translate a flambda primitive call to a JSIR variable containing the result. *)
val primitive :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Flambda_primitive.t ->
  Debuginfo.t ->
  Jsir.Var.t option * To_jsir_env.t * To_jsir_result.t
