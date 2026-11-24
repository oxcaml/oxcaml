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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(** Escape analysis for turning heap allocations into stack allocations.

    This is formulated as a few extra Datalog rules on top of the relations
    computed by [Points_to_analysis]: an allocation recorded with
    [Escape_graph.Relations.allocated_in] is "local" if the allocated value
    cannot flow to a place that outlives the activation of the function
    containing the allocation.

    The analysis relies on the following properties of the surrounding passes:

    - Every flow of a value known to the points-to analysis appears in its
      [alias] relation (flows through block fields, and through the parameters
      and results of calls, are turned into aliases), and a value with a flow it
      does not know about has [any_usage]. Following [rev_alias] from an
      allocation, and treating [any_usage] as escaping, thus covers every use of
      the allocated value.

    - Mutations ([Block_set], [Array_set], ...) are not modelled by the
      points-to analysis: both the block and the stored value get [any_usage],
      so a value stored into a mutable field escapes.

    - Try-regions are never removed by Simplify, so the handler of a [try]
      always restores the local stack pointer (see the discussion of the
      exceptional path in [escape_analysis.ml]).

    - Regions are closed by restoring a saved stack pointer, so closing the
      outermost of the regions left by a jump also closes the inner ones (see
      [Escape_rebuild]).

    - The curry functions allocate a partial application locally when the code
      of the callee has [nlocal] equal to its arity, and on the heap otherwise.
      The code of a local closure is given the former, as the type checker does
      for its own local closures, and its partial applications are treated as
      allocations in the region scope of their call site (see
      [escape_analysis.ml]), so that no heap block points at the local stack and
      no partial application outlives its region.

    - The generic application functions ([caml_applyN]) only run the successive
      applications of an unknown-arity call in a region of their own when the
      call returns at mode global (see [Cmm_helpers.apply_function_body]): the
      calls whose result is a local partial application are given a local return
      mode by [Escape_rebuild].

    - A function is not on a cycle of tail calls unless each call of the cycle
      is seen by the points-to analysis as a known-arity application with known
      callees, or returns its result through an application that makes the
      values it returns escape (see [cannot_return_local] in
      [escape_analysis.ml]). *)

(** Run the analysis for the given graph on the result of
    [Unboxing_analysis.perform_analysis] (whose relations
    [cannot_change_calling_convention] and [to_unbox] it uses), which must
    itself have been run on the result of [Points_to_analysis.perform_analysis]
    for the [Global_flow_graph] built by the same traversal. The resulting
    database is recorded in the graph (see [Escape_graph.solved]), and printed
    on standard error with the reaper debug flag [escape-db]. Does nothing when
    the analysis is disabled for the graph. *)
val perform_analysis : Escape_graph.t -> Unboxing_analysis.result -> unit

(** The candidate allocations (see [Escape_graph.Relations.allocated_in]), and
    the partial applications treated as such. *)
val candidate_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** Allocations (variables) that may be turned into stack allocations. *)
val local_allocation_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** The local allocations (see [local_allocation_tbl]) that may be the value of
    the given variable. *)
val local_sources :
  Datalog.database -> Code_id_or_name.t -> Code_id_or_name.t list

(** For each function (code id), the local allocations that it may return,
    directly or inside a block that it returns. *)
val local_returned :
  Datalog.database -> Code_id_or_name.Set.t Code_id_or_name.Map.t

(** For each allocation, the local allocations that may be stored in it. *)
val local_contents :
  Datalog.database -> Code_id_or_name.Set.t Code_id_or_name.Map.t

(** The region scopes (code ids of functions, or dummy variables naming
    recursive [Let_cont]s) in which the given allocation takes place (an
    allocation recorded with [allocated_in], or a partial application treated as
    one). *)
val scopes_of_allocation :
  Datalog.database -> Code_id_or_name.t -> Code_id_or_name.t list

(** Region scopes containing at least one such allocation. *)
val scope_with_local_allocation_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** Region scopes containing a call to a function that returns at mode local
    (see [local_return_tbl]), hence needing a region for the result. *)
val scope_with_local_call_result_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** Functions (code ids) that are to allocate their result in their caller's
    region: their calling convention is changed so that they return at mode
    local. *)
val local_return_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** The code of the closures that are to be allocated locally. *)
val local_closure_code_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table
