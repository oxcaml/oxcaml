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

(* The contract on the graph the pass receives, [finished_graph], and the
   conditions on its inputs that progress rests on.

   This file is to be reviewed, against [Ssa.finish_graph] (section 5.2 of
   ROCQ-REVIEW-GUIDE.md). [control_layout] formalizes ordinary builder
   guarantees: Goto arguments match parameters, omitted arguments feed
   globally unused parameters, and Switch targets have no parameters.
   [switches_in_range] is the standard defined-input condition for SSA Switch
   (out-of-range indices are undefined behaviour); it asserts neither
   progress nor termination. Nothing here is extracted.

   That a supported state with bound live registers makes progress is proved
   in SsaProgressProofs.v, which needs no review. *)

From Stdlib Require Import ZArith List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaDominanceSpec
  SsaSemanticsSpec SsaExecutionSpec SsaLivenessSpec.
Import ListNotations.
Local Open Scope Z_scope.

(* A register some instruction or terminator of the graph reads. *)
Definition register_used (blocks : list block) (reg : register) : Prop :=
  ∃ (position : program_point) (v : value), (fst position) ∈ blocks
    ∧ operand_at position v ∧ register_of_value v = reg.

Record control_layout (blocks : list block) : Prop :=
  { goto_parameter_layout : ∀ (source target : block)
      (args : list Terminator.arg),
      source ∈ blocks ->
      terminator source = Terminator.Continue (Goto target) args ->
      Forall2 (fun (arg : Terminator.arg) (parameter : value) =>
        match arg with
        | Terminator.Arg _ => True
        | Terminator.Omitted_since_unused =>
          ~ register_used blocks (register_of_value parameter)
        end) args (params target);
    switch_parameterless : ∀ (source : block) (index : value)
      (targets : list block) (target : block),
      source ∈ blocks ->
      terminator source = Terminator.Switch index targets ->
      target ∈ targets -> params target = [] }.

Definition live_registers_defined (position : program_point)
  (regs : registers) : Prop :=
  ∀ (reg : register), register_live_at position reg ->
    ∃ (word : Z), regs reg = Some word.

Definition switches_in_range (initial : state) : Prop :=
  ∀ (reached : state) (index : value) (targets : list block) (word : Z),
  steps initial reached -> remaining reached = [] ->
  terminator (current_block reached) = Terminator.Switch index targets ->
  current_registers reached (register_of_value index) = Some word ->
  0 <= word < Z.of_nat (length targets).

(* What [Ssa.finish_graph] guarantees of the graph the pass receives, as the
   eligibility theorem needs it: [blocks] is the graph's block list, [entry]
   its entry block and [defs] the table of defining blocks the pass builds from
   [blocks]. One field per builder guarantee; section 5.2 of
   ROCQ-REVIEW-GUIDE.md says where to look for each in the builder. *)
Record finished_graph (blocks : list block) (entry : block) (defs : op_def)
    : Prop :=
  { (* Definitions, uses, parameters, reachability, successor closure. *)
    graph_well_formed : well_formed_ssa blocks entry;
    (* The predecessors of a block of the graph are blocks of the graph. *)
    graph_predecessors_closed : ∀ (b pred : block),
      b ∈ blocks -> pred ∈ (predecessors b) -> pred ∈ blocks;
    (* Terminators, successors and predecessors describe the same edges. *)
    graph_edges : control_edges;
    graph_switch_edges : switch_edge_multiplicity;
    (* [dominates] is sound and complete for dominance along paths. *)
    graph_dominance : dominance_semantics entry;
    graph_dominance_complete : ∀ (dominator target : block),
      dominator ∈ blocks -> target ∈ blocks ->
      (∀ (visited_blocks : list block), block_path_from entry
        visited_blocks target -> dominator ∈ visited_blocks) ->
      dominator ▷ target;
    (* [strict_dominators] lists distinct blocks of the graph. *)
    graph_strict_dominators : ∀ (b : block), b ∈ blocks ->
      NoDup (strict_dominators b)
      ∧ (∀ (d : block), d ∈ (strict_dominators b) -> d ∈ blocks);
    (* Goto arguments match parameters; Switch targets have none. *)
    graph_layout : control_layout blocks;
    (* The table maps every instruction to its block. *)
    graph_definitions : op_def_correct defs blocks }.
