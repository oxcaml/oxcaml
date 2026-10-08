(** The edge counters of pseudo-instrumentation for FDO (see [Fdo_counter]),
    attached to the switch arms and applications of a compilation unit right
    before simplification, the first pass that moves code around (inlining,
    specialization). Within each function body, and in the unit's toplevel code,
    the constructs are numbered in traversal order, starting from the function's
    id (the entry counter of its code, from [Lambda_to_flambda]) or the unit's.
    Calls in module-binding scopes instead use the produced module's path and an
    index within that scope, to keep functor specializations stable across
    unrelated bindings. Functions whose code has no counter are left alone. *)
val add_to_unit :
  compilation_unit:Compilation_unit.t ->
  function_body_hash:Fdo_counter.Function_body_hash.t ->
  Flambda_unit.t ->
  Flambda_unit.t
