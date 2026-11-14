type block = Ssa.finished Ssa.Block.t

type instr_id = Ssa.finished Ssa.Instruction.Id.t

type blockset = Ssa.Block.Set.t

(** val params : block -> Ssa.finished Ssa.Value.t list **)

let params = fun b -> Array.to_list (Ssa.Block.params b)

(** val block_body : block -> Ssa.finished Ssa.Instruction.t list **)

let block_body = fun b -> Array.to_list (Ssa.Block.body b)

type raise_kind = Lambda.raise_kind

module Terminator = struct end

type op_def = (Ssa.finished, Ssa.finished Ssa.Block.t) Ssa.Instruction.Id.Tbl.t
