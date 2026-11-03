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

(* The basic induction variables a loop computes for nothing but its own
   control: used only by the exit comparison and their own updates, with the
   comparison used only by the header's branch. The extraction of this file,
   DeadInductionVar.ml, is the implementation; it needs no review. What a dead
   variable is, and the proofs, are in DeadInductionVarProofs.v. SsaGraph.v
   (backend/ssa/rocq/) has the uses of a value in the graph, and NaturalLoop.v
   and InductionVar.v the loops, their exit branches and the basic induction
   variables this file is given.

   Why the uses of the exit comparison are checked ([exit_comparison_uses_ok]):
   deleting the loop (DeleteEmptyLoops.v) leaves the comparison computed from
   the loop's initial parameters, so a use of it other than the header's branch
   -- an instruction, or a [Return] of it after the loop -- would observe a
   value the iteration that ended the loop did not produce. The first version
   of the analysis did not check this; the error was found while writing the
   model. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph.
From OxCamlLoop Require Import NaturalLoop InductionVar.
Import ListNotations.

(* A BIV with its verdict, and a loop with its BIVs' verdicts and whether every
   header parameter is a dead BIV. After these, the type [biv] of
   InductionVar.v is written [InductionVar.biv], its field [loop]
   [InductionVar.loop]. *)
Record biv_result : Type :=
  { biv : biv;
    dead : bool }.

Record loop_result : Type :=
  { loop : natural_loop;
    bivs : list biv_result;
    useless : bool }.

(* The parameter [parameter_index] of [header] is read only by the exit
   comparison [exit_comparison] and by the [updates], and by no terminator. *)
Definition param_uses_ok (blocks : list block) (header : block)
    (parameter_index : int) (exit_comparison : value)
    (updates : list value) : bool :=
  all_uses_ok blocks (is_param header parameter_index)
    (fun instr =>
      instr_defines_value instr exit_comparison
      || existsb (instr_defines_value instr) updates)
    (fun _ _ => false).

(* The exit comparison is read by no instruction, and by no terminator but the
   header's. *)
Definition exit_comparison_uses_ok (blocks : list block) (header : block)
    (exit_comparison : value) : bool :=
  all_uses_ok blocks (value_eqb exit_comparison)
    (fun _ => false)
    (fun block _ => block_eqb block header).

(* The update value [updated_value] is read by no instruction but the exit
   comparison, and by no terminator but the [Goto] to the header of a back-edge
   source (one of [back_edge_sources]), as the argument at position
   [parameter_index] and at no other position. *)
Definition update_uses_ok (blocks : list block) (header : block)
    (back_edge_sources : blockset) (parameter_index : int)
    (exit_comparison updated_value : value) : bool :=
  all_uses_ok blocks (value_eqb updated_value)
    (fun instr => instr_defines_value instr exit_comparison)
    (fun block _ =>
      set_mem block back_edge_sources && goto_passes_only_at header
        parameter_index updated_value block).

(* Whether a BIV is dead: its parameter, the exit comparison and its update
   values are used only as the loop's own control requires, as [dead_spec]
   (DeadInductionVarProofs.v) spells out. *)
Definition is_dead (blocks : list block) (biv : InductionVar.biv) : bool :=
  match find_exit_branch biv.(InductionVar.loop) with
  | None => false
  | Some exit_info =>
    let cond := exit_info.(condition) in
    let header := biv.(InductionVar.loop).(header) in
    let k := biv.(param_index) in
    let back_edge_sources := back_edge_set biv.(InductionVar.loop) in
    param_uses_ok blocks header k cond biv.(update)
    && exit_comparison_uses_ok blocks header cond
    && forallb (update_uses_ok blocks header back_edge_sources k cond)
         biv.(update)
  end.

(* Every header parameter is a dead BIV. *)
Definition loop_is_useless (loop : natural_loop) (bivs : list biv_result)
    : bool :=
  same_length (params loop.(header)) bivs && forallb dead bivs.

(* The verdicts for each loop given, with its BIVs. *)
Definition analyze (blocks : list block)
    (loops : list (natural_loop * list InductionVar.biv))
    : list loop_result :=
  map
    (fun '(loop, bivs) =>
      let bivs :=
        map (fun biv => {| biv := biv; dead := is_dead blocks biv |}) bivs
      in
      {| loop := loop; bivs := bivs; useless := loop_is_useless loop bivs |})
    loops.
