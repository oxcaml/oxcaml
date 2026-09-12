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
  (** Per-unit code information and references collected for the solve. *)
  module Solve_inputs : sig
    type t =
      { code_deps : Traverse_acc.code_dep Code_id.Map.t;
        code_references : Traverse_acc.code_reference list;
        rebuild_queries : Rebuild_queries.Requests.t;
        all_sets_of_closures :
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list
      }

    val ids_for_export : t -> Ids_for_export.t

    (** Units mentioned by pending code references. *)
    val referenced_compilation_units : t -> Compilation_unit.Set.t

    val apply_renaming : t -> Renaming.t -> t
  end

  module Traverse_rebuild : sig
    type t

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t
  end

  type solution =
    { uses : Analysis.result;
      code_changes : Unboxing_analysis.code_changes;
      queries : Rebuild_queries.t
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
    * Solve_inputs.t
    * Traverse_rebuild.t

  (** Analyse the combined dependency graph and compute rewriting decisions and
      slot offsets. Mutates the graph by linking code references. *)
  val solve :
    slot_offsets_inputs:Slot_offsets_analysis.Inputs.t ->
    analysis_scope:Analysis.Scope.t ->
    solve_inputs:Solve_inputs.t list ->
    Global_flow_graph.graph ->
    solution * Slot_offsets.result

  (** Use a Reaper solution and traversed compilation unit to rebuild the unit
      with dead code removed. The solution must cover the current unit and the
      other participating units whose identifiers occur in it. [typing] enables
      precise subkind and export-type rewriting for normal Reaper. LTO passes
      [None] for backend-only rebuilding, which needs no type database and
      leaves export types unknown. Returns the rebuilt unit, code, typing
      environment, and free names. *)
  val rebuild :
    unit_metadata:Flambda_unit.Metadata.t ->
    traverse_rebuild:Traverse_rebuild.t ->
    solution:Rebuild_solution.t ->
    typing:Rebuild.typing option ->
    machine_width:Target_system.Machine_width.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    Flambda_unit.t * Exported_code.t * Typing_env.t option * Name_occurrences.t
end

val run :
  machine_width:Target_system.Machine_width.t ->
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  final_typing_env:Typing_env.t option ->
  free_names:Name_occurrences.t ->
  Flambda_unit.t ->
  Flambda_unit.t * Exported_code.t * Slot_offsets.result * Typing_env.t option
