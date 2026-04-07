(** Tail-call detection.

    Replaces [Call]+[Return] sequences that satisfy the calling-convention and
    trap-stack constraints with [Tailcall_self] or [Tailcall_func]. *)

val run : (module Ssa.Finished_graph) -> (module Ssa.Finished_graph)
