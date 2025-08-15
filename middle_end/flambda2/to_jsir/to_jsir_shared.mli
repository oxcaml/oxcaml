(** Bind a fresh JSIR variable to [expr], and map [fvar] to this new variable in the
    environment. *)
val bind_expr_to_var :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Variable.t ->
  Jsir.expr ->
  To_jsir_env.t * To_jsir_result.t

val bind_expr_to_var' :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Variable.t ->
  Jsir.expr ->
  Jsir.Var.t * To_jsir_env.t * To_jsir_result.t

(** Convert a Flambda [Reg_width_const.t] into a [Jsir.constant]. *)
val reg_width_const : Reg_width_const.t -> Jsir.constant

(** Convert a Flambda [Simple.t] into a [Jsir.Var.t], potentially by adding new
    instructions in the result. *)
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

(** Take in [Bound_parameters.t] and bind each parameter to fresh JSIR variables in the
    environment, and return the variables in order *)
val bound_parameters :
  env:To_jsir_env.t -> Bound_parameters.t -> Jsir.Var.t list * To_jsir_env.t

(** Make a new block. *)
val block :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  tag:Tag.t ->
  mut:Mutability.t ->
  fields:Simple.t list ->
  Jsir.expr * To_jsir_env.t * To_jsir_result.t
