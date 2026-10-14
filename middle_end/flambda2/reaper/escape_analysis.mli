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
    [Global_flow_graph.Relations.allocated_in] is "local" if the allocated value
    cannot flow to a place that outlives the activation of the function
    containing the allocation. *)

(** Whether the analysis is enabled ([-X reaper-escape=1]). *)
val enabled : unit -> bool

(** Run the escape analysis rules. The database must already contain the results
    of [Points_to_analysis.perform_analysis]. *)
val perform_analysis :
  Datalog.database -> stats:Datalog.Schedule.stats -> Datalog.database

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
