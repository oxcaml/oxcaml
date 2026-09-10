(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Staged : sig
  module Traverse_rebuild : sig
    type t

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t

    (** Map over the result types of the stored code metadata. Used for
        canonicalisation. *)
    val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
  end

  (** Traverse the compilation unit in preparation for Reaper analysis.
      [free_names] are the free names of the whole compilation unit as output by
      simplify. Returns the dependency graph, the unit's inputs to the
      solve-time slot offsets computation, and the data needed to rebuild the
      unit. *)
  val traverse :
    free_names:Name_occurrences.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    Flambda_unit.t ->
    Global_flow_graph.graph
    * Slot_offsets_analysis.Inputs.t
    * Traverse_rebuild.t

  (** Run Reaper analysis producing a Reaper solution, together with the slot
      offsets of the sets of closures that will be built after rewriting. For
      LTO, the graph and slot offsets inputs are the unions of those of all
      participating units, and [is_local_compilation_unit] is membership of the
      set of participants, so that one consistent assignment of offsets is
      computed for the whole program. *)
  val solve :
    slot_offsets_inputs:Slot_offsets_analysis.Inputs.t ->
    is_local_compilation_unit:(Compilation_unit.t -> bool) ->
    Global_flow_graph.graph ->
    Unboxing_analysis.result * Slot_offsets.result

  (** Use a Reaper solution and traversed compilation unit to rebuild the unit
      with dead code removed. *)
  val rebuild :
    unit_metadata:Flambda_unit.Metadata.t ->
    traverse_rebuild:Traverse_rebuild.t ->
    solved_dep:Unboxing_analysis.result ->
    machine_width:Target_system.Machine_width.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    final_typing_env:Typing_env.t option ->
    Flambda_unit.t * Exported_code.t * Typing_env.t option
end

val run :
  machine_width:Target_system.Machine_width.t ->
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  final_typing_env:Typing_env.t option ->
  free_names:Name_occurrences.t ->
  Flambda_unit.t ->
  Flambda_unit.t * Exported_code.t * Slot_offsets.result * Typing_env.t option
