(** Translate a flambda primitive call to a JSIR variable containing the result. *)
val primitive :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Flambda_primitive.t ->
  Jsir.Var.t option * To_jsir_env.t * To_jsir_result.t

(** Call an external function. *)
val extern :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Simple.t list ->
  Jsir.Var.t * To_jsir_result.t
