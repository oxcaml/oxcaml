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

(* CR mvellacott: (long term) get rid of CMR files, and put the data in CMX instead *)
(* CR mvellacott: (short term) store actually useful data in CMR files *)
type t = unit

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** [used_value_slots] is the set computed by [Slot_offsets.finalize_offsets]
    for the unit being stored; it describes the data written alongside it. *)
val save : filename:string -> used_value_slots:Value_slot.Set.t -> t -> unit

val restore : filename:string -> t
