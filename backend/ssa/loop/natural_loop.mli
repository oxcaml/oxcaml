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

[@@@ocaml.warning "+a-40-41-42"]

(** Natural-loop discovery over the block graph of a finished SSA graph.

    This module is pure graph theory: it knows nothing about SSA values or
    instruction semantics, only blocks, edges (including the implicit exception
    edges) and the dominance oracle computed by [Ssa.finish_graph].

    A loop's header is a block to which a back edge points (an edge [u -> v]
    where [v] dominates [u]); the loop's body is the header plus every block
    that can reach a back-edge source without passing through the header. *)

open Ssa.Export

type loop =
  { header : finished Block.t;
    body : Block.Set.t;
    back_edges : finished Block.t list
        (** The back-edge {e sources} (blocks with an edge to [header]). *)
  }

(** The natural loop of every back edge in the graph, grouped by header. *)
val find_loops : finished Ssa.graph -> loop list

(** [edge_dominates ~src ~succ ~target] is a sufficient condition for the edge
    [src -> succ] to dominate [target], i.e. for every path from the graph's
    entry to [target] to traverse that specific edge: [succ] dominates [target]
    (all paths pass through [succ]) and [src] is [succ]'s only predecessor (the
    only way into [succ] is that edge). Node-dominance of [succ] alone would not
    be enough: control could reach [succ] via another in-edge when paths
    reconverge. *)
val edge_dominates :
  src:finished Block.t ->
  succ:finished Block.t ->
  target:finished Block.t ->
  bool
