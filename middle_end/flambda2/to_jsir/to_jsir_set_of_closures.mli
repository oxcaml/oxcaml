(** Translate a dynamically allocated [Set_of_closures.t] into a JSIR [Closure]. *)
val dynamic_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  bound_vars:Bound_var.t list ->
  Set_of_closures.t ->
  To_jsir_env.t * To_jsir_result.t

(** Translate a statically allocated [Set_of_closures.t] into a JSIR [Closure]. *)
val static_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  closure_symbols:Symbol.t Function_slot.Lmap.t ->
  Set_of_closures.t ->
  To_jsir_env.t * To_jsir_result.t
