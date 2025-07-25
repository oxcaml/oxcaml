(** Translate a flambda primitive call to a JSIR expr. *)

val primitive :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Flambda_primitive.t ->
  Debuginfo.t ->
  Jsir.expr * To_jsir_env.t * To_jsir_result.t
