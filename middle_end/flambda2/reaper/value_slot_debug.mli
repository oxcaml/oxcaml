(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Debug instrumentation for Value_slot.t operations in the reaper.

    Enabled by setting the environment variable REAPER_VALUE_SLOT_DEBUG.

    Optionally filtered by REAPER_VALUE_SLOT_DEBUG_NAME, which when set will
    only print debug output for value slots whose name matches (ignoring the
    numerical stamp). *)

(** Whether debug output is enabled. *)
val enabled : bool

(** [log location vs] prints a debug message for value slot [vs] at the given
    [location] if debugging is enabled and the name filter matches (if set). *)
val log : string -> Value_slot.t -> unit

(** [log_opt location vs_opt] is like [log] but for optional value slots. *)
val log_opt : string -> Value_slot.t option -> unit

(** [log_create location vs] prints a debug message for creating a value slot. *)
val log_create : string -> Value_slot.t -> unit

(** [log_access location vs] prints a debug message for accessing a value slot. *)
val log_access : string -> Value_slot.t -> unit

(** [log_map_iter location vs] prints a debug message when iterating over a
    value slot in a map. *)
val log_map_iter : string -> Value_slot.t -> unit

(** [log_map_add location vs] prints a debug message when adding to a map. *)
val log_map_add : string -> Value_slot.t -> unit

(** [log_map_find location vs] prints a debug message when finding in a map. *)
val log_map_find : string -> Value_slot.t -> unit

(** [log_field_view location vs] prints when a Field.view matches Value_slot. *)
val log_field_view : string -> Value_slot.t -> unit

(** [log_kind location vs] prints when getting the kind of a value slot. *)
val log_kind : string -> Value_slot.t -> unit

(** [log_compilation_unit location vs] prints when getting compilation unit. *)
val log_compilation_unit : string -> Value_slot.t -> unit
