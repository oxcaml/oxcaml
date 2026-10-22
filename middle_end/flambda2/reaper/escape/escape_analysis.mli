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
    containing the allocation. *)

(** Whether the analysis is enabled ([-X reaper-escape=1]). *)
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

(** Allocations (variables) that may be turned into stack allocations. *)
val local_allocation_tbl :
  ( unit Code_id_or_name.Map.t,
    Code_id_or_name.t -> Datalog.nil,
    unit )
  Datalog.table

(** The region scopes (code ids of functions, or dummy variables naming
    recursive [Let_cont]s) in which the given allocation was recorded. *)
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
