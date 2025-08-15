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

(** Prepare a static block of code to be translated: create a new block and parameter
    variables and add them to the environment, and also add any value or function slots
    that are used into the environment. *)
val prepare_code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  code_id:Code_id.t ->
  Flambda.function_params_and_body Code0.t ->
  To_jsir_env.t * To_jsir_result.t

(** Translate a static block of code. *)
val code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  translate_body:
    (env:To_jsir_env.t ->
    res:To_jsir_result.t ->
    Flambda.expr ->
    To_jsir_env.t * To_jsir_result.t) ->
  code_id:Code_id.t ->
  Flambda.function_params_and_body Code0.t ->
  To_jsir_env.t * To_jsir_result.t
