val terminator_pure_control : Ssa.finished Ssa.Terminator.t -> bool

val terminator_args :
  Ssa.finished Ssa.Terminator.t -> Ssa.finished Ssa.Value.t list list
