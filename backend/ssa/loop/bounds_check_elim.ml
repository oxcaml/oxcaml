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

(* See [bounds_check_elim.mli] for the interface.

   The analysis deciding which checks to remove is [BoundsCheckElim], generated
   from the Rocq model in [rocq/BoundsCheckElim.v] by [rocq/Extract.v] (see
   [rocq/Makefile]); it returns the blocks whose [Switch] always takes its true
   edge, with that edge's target. The rewriting of the graph below is not
   modelled; it is the same rewrite as [Delete_empty_loops] performs on a loop
   header, and the simulation argument of ROCQ-REVIEW-GUIDE.md, section 5.5,
   applies to it unchanged. *)

open Ssa.Export
open Ssa_reducer
module E = BoundsCheckElim

(* Blocks whose check always passes, mapped to the target of its true edge. *)
let find_eliminations (ssa : finished Ssa.graph) : finished Block.t Block.Tbl.t
    =
  let op_def = Delete_empty_loops.compute_op_def ssa in
  let eliminations = Block.Tbl.create 4 in
  List.iter
    (fun (block, target) -> Block.Tbl.replace eliminations block target)
    (E.find_eliminations op_def (Ssa.entry ssa) (Ssa.blocks ssa));
  eliminations

let run ~ppf_dump (ssa : finished Ssa.graph) : finished Ssa.graph * int =
  let eliminations = find_eliminations ssa in
  if Block.Tbl.length eliminations = 0
  then ssa, 0
  else begin
    let module Eliminate_reducer : Reducer = struct
      include Default_reducer

      let visit_terminator ctx (block : finished Block.t) =
        match Block.Tbl.find_opt eliminations block with
        | None -> Unchanged
        | Some target ->
          (* Replace the check's branch by an unconditional jump to its true
             target. Switch targets take no arguments. *)
          Reduce
            (fun _c ->
              ( Block.terminator_dbg block,
                Context.map_terminator ctx
                  (Continue { continuation = Goto target; args = [||] }) ))
    end
    in
    let module Runner = Make_run (Eliminate_reducer) in
    Runner.run ~ppf_dump ssa, Block.Tbl.length eliminations
  end
