[@@@ocaml.warning "+a-40-41-42"]

module SimplifyConditions (A : Ssa_reducer.Assembler) : Ssa_reducer.S = struct
  let emit_instruction (_ : Ssa.instruction) = `Unchanged

  let emit_terminator ~dbg (t : Ssa.terminator) =
    match[@warning "-fragile-match"] t with
    | Branch { cond = Op { op; args = [| arg |]; _ }; ifso; ifnot } -> (
      match op with
      | Intop_imm (Icomp Cne, 0) ->
        A.emit_terminator ~dbg (Ssa.Branch { cond = arg; ifso; ifnot });
        `Replaced
      | Intop_imm (Icomp Ceq, 0) ->
        A.emit_terminator ~dbg (Ssa.Branch { cond = arg; ifnot; ifso });
        `Replaced
      | _ -> `Unchanged)
    | _ -> `Unchanged
end

let run ssa =
  Ssa_reducer.run (module SimplifyConditions : Ssa_reducer.Reducer) ssa
