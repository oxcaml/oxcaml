type expected_benefit = { removed_operations : Removed_operations.t }

val eval :
  Downwards_env.t ->
  Param_use.params_uses ->
  args:Simple.t list ->
  expected_benefit
