open SsaModel

val terminator_pure_control : Ssa.finished Ssa.Terminator.t -> bool

val is_param : block -> int -> Ssa.finished Ssa.Value.t -> bool

val instr_defines_value :
  Ssa.finished Ssa.Instruction.t -> Ssa.finished Ssa.Value.t -> bool

val args_of_instr :
  Ssa.finished Ssa.Instruction.t -> Ssa.finished Ssa.Value.t list

val terminator_args :
  Ssa.finished Ssa.Terminator.t -> Ssa.finished Ssa.Value.t list list

val arg_is : Ssa.finished Ssa.Value.t -> Ssa.finished Ssa.Terminator.arg -> bool

val arg_value :
  Ssa.finished Ssa.Terminator.arg list -> int -> Ssa.finished Ssa.Value.t option

val goto_args : block -> block -> Ssa.finished Ssa.Terminator.arg list option

val goto_passes_only_at :
  block -> int -> Ssa.finished Ssa.Value.t -> block -> bool

val all_uses_ok :
  block list ->
  (Ssa.finished Ssa.Value.t -> bool) ->
  (Ssa.finished Ssa.Instruction.t -> bool) ->
  (block -> Ssa.finished Ssa.Value.t list -> bool) ->
  bool

val visit_all :
  (blockset -> block -> blockset option) ->
  blockset ->
  block list ->
  blockset option

val visit_acyclic :
  block list ->
  blockset ->
  block ->
  blockset ->
  blockset ->
  block ->
  blockset option

val acyclic_except : block list -> blockset -> block -> bool
