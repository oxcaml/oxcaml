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

module For_lto : sig
  (** A unit's inputs to the solve. *)
  module Solve_inputs : sig
    type t
  end

  (** The data needed to rebuild a traversed unit. *)
  module Rebuild_inputs : sig
    type t
  end

  (** The decisions computed by the solve. *)
  module Solution : sig
    type t
  end

  val traverse :
    free_names:Name_occurrences.t ->
    Flambda_unit.t ->
    Solve_inputs.t * Rebuild_inputs.t

  val solve : Solve_inputs.t -> Solution.t

  (** Rebuild a single unit with the decisions coming from the solution. *)
  val rebuild :
    unit:Flambda_unit.t ->
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
