open SsaModel

val supported_operation : Operation.t -> 'a1 list -> bool

val supported_instruction : Ssa.finished Ssa.Instruction.t -> bool

val supported_block : block -> bool

val context_instruction_supported : Ssa.finished Ssa.Instruction.t -> bool
