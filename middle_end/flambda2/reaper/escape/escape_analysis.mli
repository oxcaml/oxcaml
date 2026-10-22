(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
      no partial application outlives its region. *)

(** Whether the analysis is enabled (see [Escape_graph.enabled]). *)
val enabled : unit -> bool

(** Run the analysis on the database of [Unboxing_analysis.perform_analysis]
    (whose relations [cannot_change_calling_convention] and [to_unbox] it uses),
    which must itself have been run on the result of
    [Points_to_analysis.perform_analysis]. Does nothing when the analysis is
    disabled (see [enabled]). *)
val perform_analysis :
  Unboxing_analysis.result ->
  stats:Datalog.Schedule.stats ->
  Unboxing_analysis.result

(** Adjust the metadata of a function according to the decisions of the
    analysis: a function made to return at mode local gets a [result_mode] of
    kind [Maybe_alloc_stack] (its call sites get a [return_mode] of that kind,
    see [Escape_rebuild]), and such a function or the code of a local closure
    gets [nlocal = 1] (see [First_complex_local_param]). *)
val adjust_code_metadata :
  Datalog.database -> Code_id.t -> Code_metadata.t -> Code_metadata.t

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
    (see [returns_local]), hence needing a region for the result. *)
val scope_with_local_call_result_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** Functions (code ids) that are to allocate their result in their caller's
    region. *)
val local_return_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table
