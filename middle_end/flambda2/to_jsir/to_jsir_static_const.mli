(** Translate a Flambda [Static_const.t] into a JSIR expr, for "block-like" constants
    (i.e. not [Set_of_closures]).

    See [Flambda2_terms.Static_const.match_against_bound_static_pattern] *)

val block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Flambda2_terms.Static_const.t ->
  Jsir.expr * To_jsir_env.t * To_jsir_result.t
