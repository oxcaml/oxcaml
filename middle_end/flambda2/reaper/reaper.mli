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

    val code_deps : t -> Traverse_acc.code_dep Code_id.Map.t
  end

  type solution =
    { uses : Unboxing_analysis.result;
      code_changes : Unboxing_analysis.code_changes
    }

  (** Traverse the compilation unit in preparation for Reaper analysis. *)
  val traverse : Flambda_unit.t -> Global_flow_graph.graph * Traverse_rebuild.t

  (** Run Reaper analysis for a compilation unit producing a Reaper solution. *)
  val solve :
    Global_flow_graph.graph ->
    code_deps:Traverse_acc.code_dep Code_id.Map.t ->
    solution

  (** Use a Reaper solution and traversed compilation unit to rebuild the unit
      with dead code removed. *)
  val rebuild :
    unit_metadata:Flambda_unit.Metadata.t ->
    traverse_rebuild:Traverse_rebuild.t ->
    solution:solution ->
    machine_width:Target_system.Machine_width.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    final_typing_env:Typing_env.t option ->
    Flambda_unit.t
    * Name_occurrences.t
    * Exported_code.t
    * Slot_offsets.t
    * Typing_env.t option
end

val run :
  machine_width:Target_system.Machine_width.t ->
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  final_typing_env:Typing_env.t option ->
  Flambda_unit.t ->
  Flambda_unit.t
  * Name_occurrences.t
  * Exported_code.t
  * Slot_offsets.t
  * Typing_env.t option
