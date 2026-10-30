(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(* See [delete_empty_loops.mli] for the interface.

   The analysis deciding which loops to delete is [DeleteEmptyLoops], generated
   from the Rocq model in [rocq/DeleteEmptyLoops.v] by [rocq/Extract.v] (see
   [rocq/Makefile]); it returns the loop headers to bypass with the exit target
   each should jump to instead. The rewriting of the graph below is not
   modelled.

   [LoopEligibility.find_deletions_eligible] justifies each bypass on its own,
   on the original graph; the simulation argument connecting it to the rewritten
   function, including doing all the bypasses in one pass over the graph, is in
   ROCQ-REVIEW-GUIDE.md, section 4. *)

open Ssa.Export
open Ssa_reducer
module E = DeleteEmptyLoops

(* The table from the id of each [Op] to the block defining it, which the
   analyses read through [SsaModel.op_def_find]. *)
let compute_op_def (ssa : finished Ssa.graph) : SsaModel.op_def =
  let tbl : SsaModel.op_def = Instruction.Id.Tbl.create 64 in
  List.iter
    (fun (bl : finished Block.t) ->
      Array.iter
        (fun (i : finished Instruction.t) ->
          match i with
          | Op { id; _ } -> Instruction.Id.Tbl.replace tbl id bl
          | Push_trap _ | Pop_trap _ -> ())
        (Block.body bl))
    (Ssa.blocks ssa);
  tbl

(* Loop headers to bypass, mapped to the exit target their terminator should
   jump to instead. *)
let find_deletions (ssa : finished Ssa.graph) : finished Block.t Block.Tbl.t =
  let op_def = compute_op_def ssa in
  let deletions = Block.Tbl.create 4 in
  List.iter
    (fun (header, exit_target) ->
      Block.Tbl.replace deletions header exit_target)
    (E.find_deletions op_def (Ssa.blocks ssa));
  deletions

let run ~ppf_dump (ssa : finished Ssa.graph) : finished Ssa.graph * int =
  let deletions = find_deletions ssa in
  if Block.Tbl.length deletions = 0
  then ssa, 0
  else begin
    let module Delete_reducer : Reducer = struct
      include Default_reducer

      let visit_terminator ctx (block : finished Block.t) =
        match Block.Tbl.find_opt deletions block with
        | None -> Unchanged
        | Some exit_target ->
          (* Replace the header's exit branch by an unconditional jump to the
             exit target. Build it over input blocks and let [map_terminator]
             translate it; the loop body becomes unreachable in the output graph
             and is pruned when it is finished. *)
          Reduce
            (fun _c ->
              ( Block.terminator_dbg block,
                Context.map_terminator ctx
                  (Continue { continuation = Goto exit_target; args = [||] }) ))
    end
    in
    let module Runner = Make_run (Delete_reducer) in
    Runner.run ~ppf_dump ssa, Block.Tbl.length deletions
  end
