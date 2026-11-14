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

(* Which loops to delete: [find_deletions], which combines the analyses of
   NaturalLoop.v, InductionVar.v, LoopTermination.v and DeadInductionVar.v into
   the decision. The extraction of this file, DeleteEmptyLoops.ml, is the
   implementation; it needs no review. delete_empty_loops.ml in the parent
   directory wraps it, builds the table of defining blocks ([op_def]) it is
   given, and rewrites the graph at the (header, exit target) pairs it returns
   ([run], over [Ssa_reducer]), which is not modelled.

   What is claimed of the decision is in two Spec files, which are to be
   reviewed: LoopEligibilitySpec.v (the execution of the original graph may
   bypass every loop reported) and DeleteEmptyLoopsSpec.v (the two facts about
   a reported pair that the rewrite and the input contract rely on). What the
   analyses certify of a reported loop, read back from the decision, is
   DeleteEmptyLoopsProofs.v, which needs no review.

   The global applicability gate [graph_supported] checks graph size, which
   leaves room in the affine atom counter, and excludes an instruction shape the
   observation semantics of the code before a loop (SsaContextSpec.v) has no
   rule for: an exactly modelled operation with other than one result. It runs
   before any loop analysis and rejects the whole graph on failure. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaSemanticsSpec
  SsaSemantics.
From OxCamlLoop Require Import NaturalLoop InductionVar LoopTermination
  DeadInductionVar.
Import ListNotations.

(* This leaves room in the affine atom counter even if every graph block
   supplies a two-operand guard. Check before running any loop analysis. *)
Definition graph_block_limit : int := 4294967296%sint63.

Definition graph_supported (blocks : list block) : bool :=
  length_at_most graph_block_limit blocks
  && forallb (fun b => forallb context_instruction_supported (block_body b))
       blocks.

(* Only the integer/control-flow fragment of SsaSemantics.v is eligible. In
   particular, a load is rejected even though the compiler's
   [Ssa.Instruction.has_side_effect] says it has none. The header belongs to
   the body, so its instructions are checked too. *)
Definition body_supported (loop : natural_loop) : bool :=
  set_for_all supported_block loop.(body).

(* Every edge out of a body block stays in the body or goes to [exit].
   [body_supported] alone does not exclude a [Switch] or [Goto] to another
   outside target. Early exits are allowed, but only to this same target. *)
Definition single_exit (loop : natural_loop) (exit : block) : bool :=
  set_for_all
    (fun block =>
      forallb (fun succ => set_mem succ loop.(body) || block_eqb succ exit)
        (all_successors block))
    loop.(body).

(* [LoopTermination.analyze] as a boolean. It is the costly check, so
   [find_deletions] runs it last, once the others have passed. *)
Definition loop_terminates (op_def : op_def) (bivs : list InductionVar.biv)
    : bool :=
  match LoopTermination.analyze op_def bivs with
  | Terminates => true
  | Unknown => false
  end.

(* The loops to delete, as (header, exit target) pairs, given the graph's block
   list and the table of defining blocks. *)
Definition find_deletions (op_def : op_def) (blocks : list block)
    : list (block * block) :=
  if graph_supported blocks then
  let loops := InductionVar.analyze op_def blocks in
  let dead := DeadInductionVar.analyze blocks loops in
  filter_map
    (fun result =>
      let loop := result.(loop) in
      let bivs := map (fun biv_result => biv_result.(biv)) result.(bivs) in
      if result.(useless) && body_supported loop
      then
        match find_exit_branch loop with
        | Some exit_info =>
          match params exit_info.(exit_target) with
          | [] =>
            if single_exit loop exit_info.(exit_target)
               && acyclic_except blocks loop.(body) loop.(header)
               && loop_terminates op_def bivs
            then Some (loop.(header), exit_info.(exit_target))
            else None
          | _ :: _ => None
          end
        | None => None
        end
      else None)
    dead
  else [].
