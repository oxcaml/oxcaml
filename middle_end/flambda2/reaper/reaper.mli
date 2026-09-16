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

(** The Reaper as three phases: [traverse] a unit, [solve] the resulting graph
    and [rebuild] the unit from the solution. [run] composes them. *)
module Staged : sig
  (** A unit's inputs to the solve. *)
  module Solve_inputs : sig
    type t
  end

  (** The data needed to rebuild a traversed unit. *)
  module Rebuild_inputs : sig
    type t
  end

  (** The rewriting decisions and slot offsets computed by the solve. *)
  module Solution : sig
    type t
  end

  (** Traverse the compilation unit. [free_names] are the free names of the
      whole unit as output by simplify. *)
  val traverse :
    free_names:Name_occurrences.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    Flambda_unit.t ->
    Solve_inputs.t * Rebuild_inputs.t

  (** Analyse the dependency graph and compute the rewriting decisions and slot
      offsets. Mutates the graph by linking the code references. *)
  val solve : Solve_inputs.t -> Solution.t

  (** Rebuild the traversed unit according to the solution. Returns the rebuilt
      unit, its code and its typing environment. *)
  val rebuild :
    unit:Flambda_unit.t ->
    rebuild_inputs:Rebuild_inputs.t ->
    solution:Rebuild_solution.t ->
    types_rewrite_context:Types_rewriter.rewrite_context ->
    code_deps:Traverse_acc.code_dep Code_id.Map.t ->
    final_typing_env:Typing_env.t option ->
    machine_width:Target_system.Machine_width.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
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
