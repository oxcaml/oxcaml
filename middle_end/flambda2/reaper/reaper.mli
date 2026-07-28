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
  end

  (** Traverse the compilation unit in preparation for Reaper analysis. *)
  val traverse : Flambda_unit.t -> Global_flow_graph.graph * Traverse_rebuild.t

  (** Run Reaper analysis for a compilation unit producing a Reaper solution. *)
  val solve : Global_flow_graph.graph -> Unboxing_analysis.result

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
