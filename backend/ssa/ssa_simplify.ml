[@@@ocaml.warning "+a-40-41-42"]

module SimplifyConditions (C : Ssa_reducer.Context) :
  Ssa_reducer.S with type t = C.t = struct
  include Ssa_reducer.Default (C)

  let rewrite_terminator (b : t) ~dbg (t : Ssa.terminator) =
    match[@warning "-fragile-match"] t with
    | Branch { cond = Op { op; args = [| arg |]; _ }; ifso; ifnot } -> (
      match op with
      | Intop_imm (Icomp Cne, 0) ->
        C.finish_block b ~dbg (Ssa.Branch { cond = arg; ifso; ifnot });
        `Replaced
      | Intop_imm (Icomp Ceq, 0) ->
        C.finish_block b ~dbg
          (Ssa.Branch { cond = arg; ifnot = ifso; ifso = ifnot });
        `Replaced
      | _ -> `Unchanged)
    | _ -> `Unchanged
end

(* Inline single-predecessor blocks: the (unique) predecessor's [Goto] args can
   globally substitute the block's params, so folding the body and terminator
   into the predecessor is sound — all uses of the params (wherever they appear
   in the graph) see the same values.

   A multi-predecessor variant (empty body, no params, [Goto] terminator) was
   tried and removed: it interacts badly with single-pred inlining because the
   multi-pred inline copies the merge's [Goto] target into each parent,
   resurrecting a target that may already have been absorbed via the single-pred
   case. *)
module Inline_merge (C : Ssa_reducer.Context) :
  Ssa_reducer.S with type t = C.t = struct
  include Ssa_reducer.Default (C)

  let is_inlinable (blk : Ssa.block) =
    match blk.predecessors with [_] -> true | _ -> false

  let visit_terminator (blk : Ssa.block) (b : t) =
    match[@warning "-fragile-match"] blk.terminator with
    | Goto { goto; args } when is_inlinable goto ->
      C.inline_block goto ~block_args:args b;
      `Replaced
    | _ -> `Unchanged
end

let run ssa =
  Ssa_reducer.run
    (Ssa_reducer.combine
       [ (module SimplifyConditions : Ssa_reducer.Reducer);
         (module Inline_merge : Ssa_reducer.Reducer) ])
    ssa
