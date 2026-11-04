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

(* The terminators and edges of an SSA graph: the two readings of a terminator
   the statements of the models are made with, and what the compiler's edge
   tables are taken to guarantee.

   This file is to be reviewed. [terminator_pure_control] and
   [terminator_args] appear in the semantics (SsaSemanticsSpec.v,
   SsaContextSpec.v) and in the well-formedness contract
   (SsaExecutionSpec.v); they are also extracted (SsaGraphSpec.ml) for the
   checks of SsaGraph.v to use. [control_edges], [switch_edge_multiplicity]
   and [op_def_correct] are fields of [finished_graph] (SsaProgressSpec.v):
   what [Ssa.finish_graph] guarantees, the table of defining blocks it fills
   included; section 2.2 of ROCQ-REVIEW-GUIDE.md says where to check each in
   the builder.

   The checks the loop analyses make of a graph are in SsaGraph.v, what they
   establish and the proofs in SsaGraphProofs.v; neither needs review. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel.
Import ListNotations.

(* ======================================================================== *)
(* Observations of a terminator                                             *)
(* ======================================================================== *)

(* A terminator that only transfers control within the function. *)
Definition terminator_pure_control (t : Terminator.t) : bool :=
  match t with
  | Terminator.Continue (Goto _) _ | Terminator.Switch _ _ => true
  | Terminator.Continue (Return | Raise _ | Unreachable) _
  | Terminator.Call _ | Terminator.Invalid _ => false
  end.

(* The values a terminator reads, one list per kind of use: the arguments a
   [Continue] passes, the index of a [Switch], the arguments of a [Call] or an
   [Invalid]. *)
Definition terminator_args (term : Terminator.t) : list (list value) :=
  match term with
  | Terminator.Continue _ args =>
    [filter_map
       (fun arg =>
         match arg with
         | Terminator.Arg v => Some v
         | Terminator.Omitted_since_unused => None
         end)
       args]
  | Terminator.Switch index _ => [[index]]
  | Terminator.Call args => [args]
  | Terminator.Invalid args => [args]
  end.

(* ======================================================================== *)
(* Contracts on the graph                                                   *)
(* ======================================================================== *)

(* The compiler's edge observations agree with the terminators and with one
   another. Extra successors (for example exception edges) are allowed; every
   ordinary control transfer must still appear in the graph. *)
Record control_edges : Prop :=
  { goto_successor : ∀ (source target : block)
      (args : list Terminator.arg),
      terminator source = Terminator.Continue (Goto target) args ->
      target ∈ (all_successors source);
    switch_successor : ∀ (source : block) (index : value)
      (targets : list block) (target : block),
      terminator source = Terminator.Switch index targets ->
      target ∈ targets -> target ∈ (all_successors source);
    predecessor_successor : ∀ (source target : block),
      source ∈ (predecessors target) <-> target ∈ (all_successors source) }.

(* Predecessor metadata retains one occurrence for each Switch edge, including
   repeated targets. Other incoming edges may add occurrences. This is a
   property of graph construction, not of branch execution. *)
Definition switch_edge_multiplicity : Prop :=
  ∀ (source : block) (index : value) (targets : list block)
    (target : block),
  terminator source = Terminator.Switch index targets ->
  (length (filter (block_eqb target) targets)
    <= length (filter (block_eqb source) (predecessors target)))%nat.

(* Every instruction in the supplied graph resolves to its containing block in
   the table built by the OCaml veneer. No claim is needed about IDs that do
   not occur in the graph. *)
Definition op_def_correct (defs : op_def) (blocks : list block) : Prop :=
  ∀ (b : block) (id : instr_id) (op : operation) (args : list value)
    (results : int),
  b ∈ blocks -> (Op id op args results) ∈ (block_body b) ->
  op_def_find defs id = Some b.
