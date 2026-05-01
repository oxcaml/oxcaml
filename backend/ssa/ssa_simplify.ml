[@@@ocaml.warning "+a-40-41-42"]

module SimplifyConditions (C : Ssa_reducer.Context) = struct
  include Ssa_reducer.Default (C)

  let rewrite_terminator (b : C.Out.unfinished_block) ~dbg
      (t : C.Out.Terminator.t) =
    match[@warning "-fragile-match"] t with
    | Branch { cond = Op { op; args = [| arg |]; _ }; ifso; ifnot } -> (
      match op with
      | Operation.Intop_imm (Icomp Cne, 0) ->
        C.finish_block b ~dbg
          (C.Out.Terminator.Branch { cond = arg; ifso; ifnot });
        `Replaced
      | Intop_imm (Icomp Ceq, 0) ->
        C.finish_block b ~dbg
          (C.Out.Terminator.Branch { cond = arg; ifnot = ifso; ifso = ifnot });
        `Replaced
      | _ -> `Unchanged)
    | _ -> `Unchanged
end

(* Inline single-predecessor blocks: the (unique) predecessor's [Goto] args can
   globally substitute the block's params, so folding the body and terminator
   into the predecessor is sound — all uses of the params (wherever they appear
   in the graph) see the same values. *)
module Inline_merge (C : Ssa_reducer.Context) = struct
  include Ssa_reducer.Default (C)

  let is_inlinable (blk : C.In.Block.t) =
    C.In.Block_set.cardinal blk.predecessors = 1

  let map_args (args : C.In.Instruction.t array) : C.Out.Instruction.t array =
    Array.map C.map_arg args

  let visit_terminator (blk : C.In.Block.t) (b : C.Out.unfinished_block) =
    match[@warning "-fragile-match"] blk.terminator with
    | Goto { goto; args } when is_inlinable goto ->
      C.inline_block goto ~block_args:(map_args args) b;
      `Replaced
    | _ -> `Unchanged
end

let run ssa =
  Ssa_reducer.run
    (Ssa_reducer.combine
       [ (module SimplifyConditions : Ssa_reducer.Reducer);
         (module Inline_merge : Ssa_reducer.Reducer) ])
    ssa
