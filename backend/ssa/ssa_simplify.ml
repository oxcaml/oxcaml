[@@@ocaml.warning "+a-40-41-42"]

(* Since Branch has zero-comparison semantics, we can fold away nested
   comparisons to zero. We have inserted such trivial comparisons in Ssa_of_cmm
   to preserve the graph shape for CFG comparison validation. Removing them
   enables Cfg_of_ssa to duplicate branch conditions without materializing
   bits. *)
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

let run ssa =
  Ssa_reducer.run (module SimplifyConditions : Ssa_reducer.Reducer) ssa
