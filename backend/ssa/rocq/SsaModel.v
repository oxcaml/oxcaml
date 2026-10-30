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

(* The SSA graph as the loop models see it.

   The models of backend/ssa/loop/ run on a finished SSA graph ([Ssa.finished
   Ssa.graph]). Its blocks are OxCaml data the models cannot look inside,
   declared here as an abstract type ([Parameter]) with the few observations
   the passes make of them: predecessors, dominance, parameters, body and
   terminator. Its values, instructions and terminators are declared as
   [Inductive] types copying the compiler's, constructor for constructor, with
   the fields the passes read; the extraction (SsaModelExtract.v) maps each
   back onto the compiler's type, so that a model matching on a value matches
   on the compiler's [Ssa.Value.t] itself, and the operation defining a value
   is the compiler's [Operation.t] (backend/rocq/Operation.v). Each observation
   is realized at extraction by a function of [Ssa] ([Block.predecessors],
   [Block.dominates], ...), at most wrapped to turn an array into a list.

   Nothing here is proved. What the proofs rely on about these observations is
   stated where it is used, as hypotheses of the theorems (what an operation
   computes, what dominance means, how a run of a loop goes). The axioms below
   say only how the equalities and the block sets behave, which is what
   [Ssa.Block.equal] (identity of blocks), [Ssa.Value.equal] (identity of
   values) and [Ssa.Block.Set] (sets ordered by block id) provide.

   [block] stands for the reachable blocks of one finished graph, the only ones
   the pass receives: [finish_graph] prunes the unreachable ones, no reachable
   block refers to them, and the compiler's [dominates] is not even defined on
   them. Hypotheses that quantify over every [block] without a membership
   guard, such as [SsaGraph.control_edges], use this universe. *)

From Stdlib Require Import ZArith Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64.
From OxCamlBackend Require Import IntegerComparison Operation.
Import ListNotations.

(* ======================================================================== *)
(* The types and their equalities                                           *)
(* ======================================================================== *)

(* [Ssa.finished Ssa.Block.t] and [Ssa.finished Ssa.Instruction.Id.t]. *)
Parameter block : Type.
Parameter instr_id : Type.

(* [Ssa.finished Ssa.Value.t]: [Res id op args i] is [Res ({ id; op; args; _ },
   i)], the result [i] of the instruction [id] computing [op] on [args] (as a
   list), and [Block_param b i] is [Block_param (b, i)], the parameter [i] of
   [b]. *)
Inductive value : Type :=
  | Res (id : instr_id) (op : operation) (args : list value) (i : int)
  | Block_param (block : block) (i : int).

(* [Ssa.finished Ssa.Instruction.t]: an [Op]'s operation, arguments and number
   of results. Keeping the operation here lets the semantics execute the
   instruction itself, rather than infer it from a use of its result. *)
Inductive instr : Type :=
  | Op (id : instr_id) (op : operation) (args : list value) (results : int)
  | Push_trap
  | Pop_trap.

(* [Ssa.Block.equal], [Ssa.Value.equal] and [Ssa.Instruction.Id.equal]: the
   identity of the OxCaml data, which the models take as equality.
   [Ssa.Value.equal] compares an operation's result by its instruction (one
   [op_data] per instruction, so by [id]) and result index, and a parameter by
   its block and index; two values of one graph are then equal to it exactly
   when they are the same term of [value]. *)
Parameter block_eqb : block -> block -> bool.
Parameter value_eqb : value -> value -> bool.
Parameter instr_id_eqb : instr_id -> instr_id -> bool.
Axiom block_eqb_spec : ∀ (block block' : SsaModel.block),
  block_eqb block block' = true <-> block = block'.
Axiom value_eqb_spec : ∀ (v w : value), value_eqb v w = true <-> v = w.
Axiom instr_id_eqb_spec : ∀ (id id' : instr_id),
  instr_id_eqb id id' = true <-> id = id'.

(* ======================================================================== *)
(* Sets of blocks: [Ssa.Block.Set]                                          *)
(* ======================================================================== *)

Parameter blockset : Type.
Parameter set_empty : blockset.
Parameter set_singleton : block -> blockset.
Parameter set_add : block -> blockset -> blockset.
Parameter set_mem : block -> blockset -> bool.
Parameter set_for_all : (block -> bool) -> blockset -> bool.
(* Membership: nothing is in the empty set; a singleton holds its element;
   adding a block puts it in and keeps the others; [set_for_all f] holds
   exactly when [f] holds of every member. *)
Axiom set_mem_empty : ∀ (block : block), set_mem block set_empty = false.
Axiom set_mem_singleton : ∀ (block block' : SsaModel.block),
  set_mem block (set_singleton block') = block_eqb block block'.
Axiom set_mem_add : ∀ (block block' : SsaModel.block) (set : blockset),
  set_mem block (set_add block' set)
  = block_eqb block block' || set_mem block set.
Axiom set_for_all_spec : ∀ (f : block -> bool) (set : blockset),
  set_for_all f set = true
  <-> (∀ (block : block), set_mem block set = true -> f block = true).

(* ======================================================================== *)
(* Blocks                                                                   *)
(* ======================================================================== *)

(* [Block.predecessors], [Block.all_successors] (the structural successors and
   the exception successor) and [Block.dominates]: [dominates a b] says that
   [a] dominates [b], [a = b] included. *)
Parameter predecessors : block -> list block.
Parameter all_successors : block -> list block.
Parameter dominates : block -> block -> bool.

(* [dominator ▷ block]: [dominator] dominates [block]. The statements of
   parts 2 to 4 of the models write dominance this way; the code of part 1
   calls [dominates] as the compiler does. *)
Notation "dominator ▷ block" := (dominates dominator block = true)
  (at level 70, no associativity).

(* [Block.strict_dominators]: the blocks that strictly dominate a block,
   nearest first, ending with the graph's entry; the chain
   [AffineSsa.guards_at] walks. *)
Parameter strict_dominators : block -> list block.

(* [Block.params] as a list, [Block.param], and [Block.body] as a list
   ([block_body], as the loop record of NaturalLoop.v has a [body]). *)
Parameter params : block -> list value.
Parameter param : block -> int -> value.
Parameter block_body : block -> list instr.

(* --- Terminators --- *)

(* [Ssa.continuation]; the payload of [Raise] is not inspected. *)
Parameter raise_kind : Type.
Inductive continuation : Type :=
  | Goto (block : block)
  | Return
  | Raise (k : raise_kind)
  | Unreachable.

(* [Ssa.Terminator]: [arg] is [Terminator.arg], and [t] is [Terminator.t] with
   its arrays as lists and only the fields the loop passes read. *)
Module Terminator.
  Inductive arg : Type :=
    | Arg (v : value)
    | Omitted_since_unused.

  Inductive t : Type :=
    | Continue (continuation : continuation) (args : list arg)
    | Switch (index : value) (targets : list block)
    | Call (args : list value)
    | Invalid (args : list value).
End Terminator.

(* [Block.terminator]. *)
Parameter terminator : block -> Terminator.t.

(* ======================================================================== *)
(* The table of defining blocks                                             *)
(* ======================================================================== *)

(* The table from the id of each [Op] to the block defining it, built by the
   pass (delete_empty_loops.ml); [op_def_find] is
   [Instruction.Id.Tbl.find_opt]. *)
Parameter op_def : Type.
Parameter op_def_find : op_def -> instr_id -> option block.
