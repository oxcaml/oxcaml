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

(* Well-formed SSA: where a value is defined, and what the builder guarantees
   about definitions, uses, parameters and reachability ([well_formed_ssa],
   the first field of [finished_graph]).

   This file is to be reviewed, against [Ssa.finish_graph] (section 2.2 of
   ROCQ-REVIEW-GUIDE.md). A definition site is a program point
   (SsaDominanceSpec.v): the point after an instruction for its results, the
   block's entry for its parameters. The conditions concern definitions,
   instruction positions and graph paths only; they contain no valuations,
   execution invariants or analysis verdicts.

   The equations of the values available at a program point, and the proofs,
   are in SsaExecutionProofs.v, which needs no review. *)

From Stdlib Require Import ZArith List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaDominanceSpec
  SsaSemanticsSpec.
Import ListNotations.

(* The point immediately after an instruction is its definition site. *)
Definition result_definition (id : instr_id) (op : operation)
    (args : list value) (result_count : int) (site : program_point) : Prop :=
  ∃ (prefix suffix : list instr),
    block_body (fst site) = prefix ++ Op id op args result_count :: suffix
    ∧ snd site = length suffix.

(* A reference carries the actual definition's payload, not just its ID.
   Parameters are available at entry; operation results after execution. *)
Definition value_definition (v : value) (site : program_point) : Prop :=
  match v with
  | Res id op args index =>
    ∃ (result_count : int),
      result_definition id op args result_count site
      ∧ (0 <= to_Z index < to_Z result_count)%Z
  | Block_param b index =>
    site = block_entry_point b ∧ nth_int (params b) index = Some v
  end.

Record well_formed_ssa (blocks : list block) (entry : block) : Prop :=
  { entry_in_graph : entry ∈ blocks;
    (* Finished graphs contain only reachable blocks. This is structural
       reachability through program points, not a claim that operations execute
       successfully or that branches are dynamically feasible. *)
    graph_blocks_reachable : ∀ (b : block), b ∈ blocks ->
      program_point_reachable entry (b, 0%nat);
    graph_successors_closed : ∀ (source target : block),
      source ∈ blocks -> target ∈ (all_successors source) ->
      target ∈ blocks;
    graph_parameters : parameter_layout;
    result_sites_unique : ∀ (id : instr_id) (op op' : operation)
      (args args' : list value) (count count' : int)
        (site site' : program_point),
      (fst site) ∈ blocks -> (fst site') ∈ blocks ->
      result_definition id op args count site ->
      result_definition id op' args' count' site' -> site = site';
    operand_definitions : ∀ (id : instr_id) (op : operation)
      (args : list value) (count : int) (site : program_point),
      (fst site) ∈ blocks -> result_definition id op args count site ->
      ∀ (arg : value), arg ∈ args ->
      ∃ (arg_site : program_point),
        value_definition arg arg_site ∧ (fst arg_site) ∈ blocks
        ∧ program_point_dominates entry arg_site site ∧ arg_site <> site;
    terminator_definitions : ∀ (b : block) (args : list value),
      b ∈ blocks -> args ∈ (terminator_args (terminator b)) ->
      ∀ (arg : value), arg ∈ args ->
      ∃ (arg_site : program_point),
        value_definition arg arg_site ∧ (fst arg_site) ∈ blocks
        ∧ program_point_dominates entry arg_site (b, 0%nat) }.
