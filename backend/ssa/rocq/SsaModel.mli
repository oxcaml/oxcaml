type block = Ssa.finished Ssa.Block.t

type instr_id = Ssa.finished Ssa.Instruction.Id.t

type blockset = Ssa.Block.Set.t

val params : block -> Ssa.finished Ssa.Value.t list

val block_body : block -> Ssa.finished Ssa.Instruction.t list

type raise_kind = Lambda.raise_kind

module Terminator : sig end

type op_def = (Ssa.finished, Ssa.finished Ssa.Block.t) Ssa.Instruction.Id.Tbl.t
