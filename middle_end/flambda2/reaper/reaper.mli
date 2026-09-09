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
  (** The per-unit inputs of the solve-time code changes computation: the code
      dependencies (which carry the code metadata captured at traverse time) and
      the unit's sets of closures. *)
  module Code_changes_inputs : sig
    type t =
      { code_deps : Traverse_acc.code_dep Code_id.Map.t;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
      }

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t

    (** Map over the result types of the stored code metadata. Used for
        canonicalisation. *)
    val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
  end

  module Traverse_rebuild : sig
    type t

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t
  end

  type solution =
    { uses : Unboxing_analysis.result;
      code_changes : Unboxing_analysis.code_changes
    }

  (** Traverse the compilation unit in preparation for Reaper analysis.
      [free_names] are the free names of the whole compilation unit as output by
      simplify. Returns the dependency graph, the unit's inputs to the
      solve-time slot offsets and code changes computations, and the data needed
      to rebuild the unit. *)
  val traverse :
    free_names:Name_occurrences.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    closed_world:bool ->
    Flambda_unit.t ->
    Global_flow_graph.graph
    * Slot_offsets_analysis.Inputs.t
    * Code_changes_inputs.t
    * Traverse_rebuild.t

  (** Analyse the combined dependency graph and compute rewriting decisions and
      slot offsets. *)
  val solve :
    slot_offsets_inputs:Slot_offsets_analysis.Inputs.t ->
    analysis_scope:Analysis.Scope.t ->
    code_changes_inputs:Code_changes_inputs.t list ->
    Global_flow_graph.graph ->
    solution * Slot_offsets.result

  (** Use a Reaper solution and traversed compilation unit to rebuild the unit
      with dead code removed. [solution.code_changes] must cover the current
      unit and the other participating units whose code ids occur in it.
      [code_deps_for_result_types] supplies the original metadata for rewriting
      result types for export; LTO passes [None] to leave them unknown. *)
  val rebuild :
    unit_metadata:Flambda_unit.Metadata.t ->
    traverse_rebuild:Traverse_rebuild.t ->
    solution:solution ->
    code_deps_for_result_types:Traverse_acc.code_dep Code_id.Map.t option ->
    all_sets_of_closures:
      (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list ->
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
