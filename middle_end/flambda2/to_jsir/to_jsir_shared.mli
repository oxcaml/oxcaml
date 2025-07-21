(** Convert a Flambda [Reg_width_const.t] into a [Jsir.constant]. *)
val reg_width_const : Reg_width_const.t -> Jsir.constant

(** Convert a Flambda [Simple.t] into a [Jsir.Var.t], potentially by adding new instructions in the result. *)
val simple :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Simple.t ->
  Jsir.Var.t * To_jsir_result.t

val simples :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Simple.t list ->
  Jsir.Var.t list * To_jsir_result.t
