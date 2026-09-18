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
    and [rebuild] the unit from the solution. [run] performs the same steps on a
    single unit in one go. *)
module Staged : sig
  (** A unit's inputs to the solve. *)
  module Solve_inputs : sig
    type t

    (** Throw away the information the whole-program solve does not need: the
        result types of the code metadata and the sets of closures, which only
        the single-unit Reaper's type rewriting uses. *)
    val prune_for_lto : t -> t

    val ids_for_export : t -> Ids_for_export.t

    (** Fields are hashconsed, so for serialisation the [Field.view] of each one
        needs serialising separately. *)
    val fields_for_export : t -> Field.Set.t

    (** The units mentioned by the code references. *)
    val referenced_compilation_units : t -> Compilation_unit.Set.t

    val apply_renaming :
      t -> Renaming.t -> rename_field:(Field.t -> Field.t) -> t
  end

  (** The data needed to rebuild a traversed unit. *)
  module Rebuild_inputs : sig
    type t

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t
  end

  (** The rewriting decisions and slot offsets computed by the solve. *)
  module Solution : sig
    type t
  end

  (** Traverse the compilation unit. [free_names] are the free names of the
      whole unit as output by simplify. The value passed to the unit's return
      continuation is not marked as used by unknown code: a whole-program
      analysis records those uses from the other units analysed. *)
  val traverse :
    free_names:Name_occurrences.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    Flambda_unit.t ->
    Solve_inputs.t * Rebuild_inputs.t

  (** Analyse the dependency graph and compute the rewriting decisions and slot
      offsets. Mutates the graph by linking the code references.
      [analysis_scope] is the set of compilation units analysed together. No
      typing information is used: the result types of the code whose calling
      convention changes are left unknown and its subkinds are erased. *)
  val solve : analysis_scope:Analysis_scope.t -> Solve_inputs.t -> Solution.t

  (** Rebuild the traversed unit according to the solution. No typing
      information is used, so the exported types of the rebuilt code are left
      unknown. Returns the rebuilt unit, its code and the solved slot offsets.
  *)
  val rebuild :
    unit_metadata:Flambda_unit.Metadata.t ->
    rebuild_inputs:Rebuild_inputs.t ->
    solution:Solution.t ->
    machine_width:Target_system.Machine_width.t ->
    cmx_loader:Flambda_cmx.loader ->
    all_code:Exported_code.t ->
    Flambda_unit.t * Exported_code.t * Slot_offsets.result
end

val run :
  machine_width:Target_system.Machine_width.t ->
  cmx_loader:Flambda_cmx.loader ->
  all_code:Exported_code.t ->
  final_typing_env:Typing_env.t option ->
  free_names:Name_occurrences.t ->
  Flambda_unit.t ->
  Flambda_unit.t * Exported_code.t * Slot_offsets.result * Typing_env.t option
