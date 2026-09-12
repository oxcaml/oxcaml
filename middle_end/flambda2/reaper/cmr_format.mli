(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Miriam Vellacott, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mvellacott: get rid of CMR files, and put the data in CMX instead *)
type t =
  { unit_metadata : Flambda_unit.Metadata.t;
    all_code : Exported_code.t;
    imported_offsets : Exported_offsets.t;
    deps : Global_flow_graph.graph;
    slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
    solve_inputs : Reaper.Staged.Solve_inputs.t;
    rebuild_data : Reaper.Staged.Traverse_rebuild.t
  }

module Serialisable : sig
  type cmr_format = t

  type t

  (** Import only the inputs needed to rebuild the unit. Code is imported as
      metadata without result types; bodies are stored in [Traverse_rebuild].
      Solve inputs are not reconstructed. *)
  val deserialise_for_rebuild :
    t ->
    Flambda_unit.Metadata.t * Exported_code.t * Reaper.Staged.Traverse_rebuild.t

  (** Deserialises only what the solve invocation needs: the dependency graph,
      the slot offsets inputs and the per-unit solve inputs (including the
      hashcons restore and rename process), together with the stored imported
      offsets. *)
  val deserialise_for_solve :
    t ->
    Global_flow_graph.graph
    * Slot_offsets_analysis.Inputs.t
    * Exported_offsets.t
    * Reaper.Staged.Solve_inputs.t

  (** Get the unit that was being compiled when the file was saved. This is a
      pure projection. *)
  val compilation_unit : t -> Compilation_unit.t
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** Save backend-only data without modifying the live code or solve inputs. *)
val save : filename:string -> t -> unit

(** Read and unmarshal a cmr file from disk. *)
val load : string -> Serialisable.t * Id_stamp_counters.t
