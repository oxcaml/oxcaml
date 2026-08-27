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

(* See [delete_empty_loops.mli] for the interface. *)

open Ssa.Export
open Ssa_reducer

(* A loop may only be deleted if, besides spinning its (dead) induction
   variables, it performs no observable work. [Dead_induction_var] only checks
   the header parameters; it says nothing about the body, so a loop like [while
   i < n do print_int 7; i <- i + 1 done] has a dead IV [i] yet is emphatically
   not empty. Here we additionally require every body block to contain only
   side-effect-free instructions and to end in a pure control-flow terminator,
   ruling out stores, calls, raises, allocations, early returns and trap
   manipulation. *)
let terminator_pure_control (t : finished Terminator.t) : bool =
  match t with
  | Continue { continuation = Goto _; _ } | Switch _ -> true
  | Continue { continuation = Return | Raise _ | Unreachable; _ }
  | Call _ | Invalid _ ->
    false

let body_effect_free (loop : Induction_var.loop) : bool =
  Block.Set.for_all
    (fun (bl : finished Block.t) ->
      Array.for_all
        (fun (i : finished Instruction.t) ->
          not (Instruction.has_side_effect i))
        (Block.body bl)
      && terminator_pure_control (Block.terminator bl))
    loop.body

(* Loop headers to bypass, mapped to the exit target their terminator should
   jump to instead. The exit target was a [Switch] target, so it can have no
   block parameters; we still check, since a [Goto] with no arguments would be
   malformed otherwise. *)
let find_deletions (ssa : finished Ssa.graph) : finished Block.t Block.Tbl.t =
  let op_def = Induction_var.compute_op_def ssa in
  let loops = Induction_var.analyze ~op_def ssa in
  let dead = Dead_induction_var.analyze ssa loops in
  let deletions = Block.Tbl.create 4 in
  List.iter
    (fun (dr : Dead_induction_var.loop_result) ->
      let loop = dr.loop in
      let bivs =
        List.map (fun (br : Dead_induction_var.biv_result) -> br.biv) dr.bivs
      in
      let terminates =
        match Termination.analyze ~op_def loop bivs with
        | Terminates -> true
        | Unknown -> false
      in
      if dr.useless && terminates && body_effect_free loop
      then
        match Termination.find_exit_branch loop with
        | Some { exit_target; _ }
          when Array.length (Block.params exit_target) = 0 ->
          Block.Tbl.replace deletions loop.header exit_target
        | Some _ | None -> ())
    dead;
  deletions

let run (ssa : finished Ssa.graph) : finished Ssa.graph * int =
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
    Runner.run ssa, Block.Tbl.length deletions
  end
