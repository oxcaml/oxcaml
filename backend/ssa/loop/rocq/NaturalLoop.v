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

(* The natural loops of an SSA graph, from its back edges, and the exit branch
   of a loop's header. The extraction of this file, NaturalLoop.ml, is the
   implementation; it needs no review. What the loops found are, and the
   proofs, are in NaturalLoopProofs.v. SsaModel.v says how the models see an
   SSA graph: blocks are abstract, with [predecessors], [all_successors] and
   [dominates] as the compiler provides them.

   [natural_loop_body] runs a worklist until the worklist is empty, which Rocq
   cannot see terminates on its own; so its [walk_predecessors] also stops when
   a list of "fuel" runs out, and [find_loops] passes the graph's block list as
   fuel. That is enough fuel: every pop of the worklist corresponds to a block
   added to the body, and a block is added at most once
   (NaturalLoopProofs.natural_loop_body_complete proves that nothing is lost).

   The trusted part is SsaModel.v: that [predecessors] and [all_successors] are
   the two ends of the same edges, that the block list given is the whole graph
   (which the theorems state as hypotheses), and that [dominates] is dominance
   in the graph (SsaDominanceSpec.v says what that means). *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaDominanceSpec SsaSemanticsSpec.
Import ListNotations.

(* A natural loop: its header, its body (the set of its blocks, the header
   included) and the sources of its back edges (the blocks of the body with an
   edge to the header). *)
Record natural_loop : Type :=
  { header : block;
    body : blockset;
    back_edge_sources : list block }.

(* Add a block to the loop body, and to the worklist [added], when it is not in
   the body yet. *)
Definition add_new_body_block (acc : blockset * list block) (pred : block)
    : blockset * list block :=
  let '(body, added) := acc in
  if set_mem pred body then (body, added)
  else (set_add pred body, pred :: added).

(* The worklist walk of [natural_loop_body], with the fuel explained above. *)
Fixpoint walk_predecessors (fuel : list block) (body : blockset)
  (worklist : list block)
    : blockset :=
  match fuel, worklist with
  | [], _ | _, [] => body
  | _ :: fuel, block :: rest =>
    let '(body, added) :=
      OxCamlList.fold_left add_new_body_block (body, rest) (predecessors block)
    in
    walk_predecessors fuel body added
  end.

(* The body of the loop with header [header] and back-edge sources
   [back_edge_sources]: the header and every block from which a back-edge
   source can be reached without passing through the header. The order of the
   walk does not affect the body it computes. *)
Definition natural_loop_body (fuel : list block) (header : block)
    (back_edge_sources : list block) : blockset :=
  let '(body, seeds) :=
    OxCamlList.fold_left add_new_body_block (set_singleton header, [])
      back_edge_sources
  in
  walk_predecessors fuel body seeds.

(* Record the back edge [source -> header] in the association list from headers
   to their back-edge sources. *)
Fixpoint add_back_edge (header source : block)
    (table : list (block * list block)) : list (block * list block) :=
  match table with
  | [] => [(header, [source])]
  | (header', back_edge_sources) :: table =>
    if block_eqb header' header
    then (header', source :: back_edge_sources) :: table
    else (header', back_edge_sources) :: add_back_edge header source table
  end.

(* For each block [source] and each of its successors [succ], record the edge
   [source -> succ] if it is a back edge: one whose target dominates its
   source. *)
Definition record_back_edge (source : block)
    (table : list (block * list block)) (succ : block)
    : list (block * list block) :=
  if dominates succ source then add_back_edge succ source table else table.

(* Record every back edge leaving [source]. *)
Definition record_back_edges (table : list (block * list block))
    (source : block) : list (block * list block) :=
  OxCamlList.fold_left (record_back_edge source) table
    (all_successors source).

(* The natural loops of the graph, given its block list ([Ssa.blocks ssa]), in
   the order their headers are first met. *)
Definition find_loops (blocks : list block) : list natural_loop :=
  let header_table := OxCamlList.fold_left record_back_edges [] blocks in
  map
    (fun '(header, back_edge_sources) =>
      {| header := header;
         body := natural_loop_body blocks header back_edge_sources;
         back_edge_sources := back_edge_sources |})
    header_table.

(* The header's exit branch: the comparison it tests, which way continues the
   loop, and where the other way goes. *)
Record exit_branch : Type :=
  { condition : value;
    continue_when_true : bool;
    exit_target : block }.

(* The exit branch of a loop: the header's terminator must be a two-target
   [Switch], [ifnot] its false edge and [ifso] its true edge, one of them into
   the body and the other out of it. *)
Definition find_exit_branch (loop : natural_loop) : option exit_branch :=
  match terminator loop.(header) with
  | Terminator.Switch index [ifnot; ifso] =>
    match set_mem ifso loop.(body), set_mem ifnot loop.(body) with
    | true, false =>
      Some {| condition := index;
              continue_when_true := true;
              exit_target := ifnot |}
    | false, true =>
      Some {| condition := index;
              continue_when_true := false;
              exit_target := ifso |}
    | true, true | false, false => None
    end
  | _ => None
  end.
