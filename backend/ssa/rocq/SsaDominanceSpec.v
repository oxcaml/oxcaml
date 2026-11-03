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

(* What dominance means in an SSA graph, as the statements of the models use
   it: paths along the graph's edges, the meaning of the compiler's
   [dominates] ([dominance_semantics], a field of [finished_graph]), and
   program points, the positions within blocks at which definitions become
   available and at which the dominance of a definition over its uses is
   stated ([well_formed_ssa], SsaExecutionSpec.v).

   This file is to be reviewed. The trusted part is SsaModel.v: that
   [predecessors] lists the sources of the edges into a block and
   [all_successors] the targets of the edges out of it. [dominance_semantics]
   is assumed of [dominates] by the theorems, as a field of [finished_graph].

   The two dominance tests the analyses run are in SsaDominance.v and the
   lemmas about paths and program points in SsaDominanceProofs.v; neither
   needs review. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel.
Import ListNotations.

(* ======================================================================== *)
(* Paths and dominance between blocks                                       *)
(* ======================================================================== *)

(* [block_path_from first p last]: [p] is a path from [first] to [last] along
   the graph's edges, listing its blocks from [first] to [last]. An edge [pred
   -> succ] is [pred] being a predecessor of [succ]. *)
Inductive block_path_from : block -> list block -> block -> Prop :=
  | Block_path_one : ∀ (block : block),
      block_path_from block [block] block
  | Block_path_cons : ∀ (pred succ : block) (rest : list block)
      (last : block),
      pred ∈ (predecessors succ) ->
      block_path_from succ (succ :: rest) last ->
      block_path_from pred (pred :: succ :: rest) last.

(* What the compiler's [dominates], written [▷], means, [entry] being
   the graph's entry block: every path from the entry to [target] passes
   through each dominator of [target], and the entry dominates every block,
   each being reachable from it. Nothing is assumed of the entry's
   predecessors: a self-recursive tail call is a back edge to the entry. *)
Record dominance_semantics (entry : block) : Prop :=
  { dominates_spec : ∀ (dominator target : block),
      dominator ▷ target ->
      ∀ (visited_blocks : list block),
      block_path_from entry visited_blocks target ->
      dominator ∈ visited_blocks;
    entry_dominates : ∀ (block : block), entry ▷ block }.

Arguments dominates_spec {entry}.
Arguments entry_dominates {entry}.

(* ======================================================================== *)
(* Program points                                                           *)
(* ======================================================================== *)

(* A program point is a block and the number of instructions still to execute.
   The entry point has the full body remaining; point zero is the terminator.
   Result definitions become available after their instruction, while
   parameters are available at the block entry. *)
Definition program_point : Type := block * nat.

Definition block_entry_point (target : block) : program_point :=
  (target, length (block_body target)).

(* Instruction edges advance through a block. Terminator edges use the same
   successor observation as block dominance, including exceptional successors
   even though the restricted semantics cannot take them. *)
Inductive program_point_edge : program_point -> program_point -> Prop :=
  | Program_point_instruction : ∀ (b : block) (insns_left : nat),
      insns_left < length (block_body b) ->
      program_point_edge (b, S insns_left) (b, insns_left)
  | Program_point_transfer : ∀ (source target : block),
      target ∈ (all_successors source) ->
      program_point_edge (source, 0) (block_entry_point target).

Inductive program_point_path_from
    : program_point -> list program_point -> program_point -> Prop :=
  | Program_point_path_one : ∀ (position : program_point),
      program_point_path_from position [position] position
  | Program_point_path_cons : ∀ (first next last : program_point)
      (rest : list program_point),
      program_point_edge first next ->
      program_point_path_from next (next :: rest) last ->
      program_point_path_from first (first :: next :: rest) last.

Definition program_point_dominates (entry : block)
    (dominator target : program_point) : Prop :=
  ∀ (points : list program_point),
    program_point_path_from (block_entry_point entry) points target ->
    dominator ∈ points.

Definition program_point_reachable (entry : block)
    (target : program_point) : Prop :=
  ∃ (points : list program_point),
    program_point_path_from (block_entry_point entry) points target.
