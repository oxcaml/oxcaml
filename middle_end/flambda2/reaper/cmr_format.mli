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
    final_typing_env : Typing_env.t option;
    all_code : Exported_code.t;
    imported_offsets : Exported_offsets.t;
    deps : Global_flow_graph.graph;
    rebuild_data : Reaper.Staged.Traverse_rebuild.t
  }

module Id_stamp_counters : sig
  type t

  (** Restore all stamp counters to the values they had when the file this came
      from was serialised. This can only be called once, before any stamps have
      been created, and will error otherwise. The affected stamp counters are
      for value slots, function slots, variables, code IDs and continuations. *)
  val restore_for_resume : t -> unit
end

module Serialisable : sig
  type cmr_format = t

  type t

  (** Turn serialised file contents back into usable data types, inserting the
      necessary objects into the global hashcons tables and then updating
      hashcons IDs as appropriate. The resuming invocation must use the same
      machine width and compilation unit as the one that wrote the field.*)
  val deserialise :
    machine_width:Target_system.Machine_width.t ->
    resolver:(Compilation_unit.t -> Typing_env.Serializable.t option) ->
    t ->
    cmr_format
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** [used_value_slots] is the set computed by [Slot_offsets.finalize_offsets]
    for the unit being stored; it describes the data written alongside it. *)
val save : filename:string -> used_value_slots:Value_slot.Set.t -> t -> unit

(** Read and unmarshal a cmr file from disk. *)
val load : string -> Serialisable.t * Id_stamp_counters.t
