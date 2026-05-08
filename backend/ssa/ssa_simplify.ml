open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa_reducer

(* Since Branch has zero-comparison semantics, we can fold away nested
   comparisons to zero. We have inserted such trivial comparisons in Ssa_of_cmm
   to preserve the graph shape for CFG comparison validation. Removing them
   enables Cfg_of_ssa to duplicate branch conditions without materializing
   bits. *)
module Simplify_conditions (C : Context) = struct
  include Default (C)

  let rewrite_terminator (c : C.cursor) ~dbg (t : C.Terminator.t) =
    match[@warning "-fragile-match"] t with
    | Branch { cond = Op { op; args = [| arg |]; _ }; ifso; ifnot } -> (
      match op with
      | Operation.Intop_imm (Icomp Cne, 0) ->
        C.finish_block c ~dbg (C.Terminator.Branch { cond = arg; ifso; ifnot });
        Replaced ()
      | Intop_imm (Icomp Ceq, 0) ->
        C.finish_block c ~dbg
          (C.Terminator.Branch { cond = arg; ifnot = ifso; ifso = ifnot });
        Replaced ()
      | _ -> Unchanged)
    | _ -> Unchanged
end

let run ssa =
  Ssa_reducer.run (module Simplify_conditions : Ssa_reducer.Reducer) ssa
