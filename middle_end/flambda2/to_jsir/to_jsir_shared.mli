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

val bind_expr_to_symbol :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Jsir.expr ->
  To_jsir_env.t * To_jsir_result.t

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
  tag:Tag.Scannable.t ->
  mut:Mutability.t ->
  fields:Simple.t list ->
  Jsir.expr * To_jsir_env.t * To_jsir_result.t

(** Convert a [Symbol.t] into their compilation unit and symbol names, in a format
    that we can pass to the [caml_get_symbol] and [caml_register_symbol] runtime
    functions. *)
val symbol_to_native_strings :
  Symbol.t -> Jsir.Native_string.t * Jsir.Native_string.t

(** Register a [Symbol.t] to the global symbol table. *)
val register_symbol :
  res:To_jsir_result.t -> Symbol.t -> Jsir.Var.t -> To_jsir_result.t
