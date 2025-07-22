(** Bind a fresh variable to the result of [Static_const.t], and map the symbol to this
    new variable in the environment, for "block-like" constants (i.e. not
    [Set_of_closures]).

    See [Static_const.match_against_bound_static_pattern] *)
val block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_env.t * To_jsir_result.t
