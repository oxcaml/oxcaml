(** Tail-call detection.

    Replaces [Call]+[Return] sequences with [Tailcall_self] or [Tailcall_func].
*)

val run :
  keep_unused_ops:bool ->
  (module Ssa.Finished_graph) ->
  (module Ssa.Finished_graph)
