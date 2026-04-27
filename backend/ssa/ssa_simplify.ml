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

(* Inline [Merge] blocks that fall into one of two simple cases:

   1. Single-predecessor [Merge]: the (unique) predecessor's [Goto] args can
   globally substitute the block's params. Folding the body and terminator into
   the predecessor is sound because all uses of the params (wherever they appear
   in the graph) see the same values.

   2. Parameter-free empty [Merge] ending in [Goto]: no params to substitute and
   no body to splice; the predecessor's [Goto] simply becomes the merge's [Goto]
   target.

   Anything fancier (multi-predecessor non-empty, or single-predecessor with
   multiple calls) would require reconstructing phi nodes elsewhere in the
   graph, which this reducer does not attempt. *)
module Inline_merge (C : Ssa_reducer.Context) :
  Ssa_reducer.S with type t = C.t = struct
  include Ssa_reducer.Default (C)

  let is_inlinable (blk : Ssa.block) =
    match[@warning "-fragile-match"] blk.desc, blk.predecessors with
    | Merge, [_] -> true
    (* | Merge, _ -> ( Array.length blk.params = 0 && Array.length blk.body = 0
       && match[@warning "-fragile-match"] blk.terminator with | Goto _ -> true
       | _ -> false) *)
    | _ -> false

  let visit_terminator (blk : Ssa.block) (b : t) =
    match[@warning "-fragile-match"] blk.terminator with
    | Goto { goto; args } when is_inlinable goto ->
      C.visit_block goto ~block_args:args b;
      `Replaced
    | _ -> `Unchanged
end

let run ssa =
  Ssa_reducer.run
    (Ssa_reducer.combine
       [ (module SimplifyConditions : Ssa_reducer.Reducer);
         (module Inline_merge : Ssa_reducer.Reducer) ])
    ssa
