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

(** A solution header and a cache of sections imported during one rebuild batch.
    Do not reuse the cache after resetting the identifier tables. *)
type t

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** Store rebuild answers, code changes, and offsets in sections keyed by their
    compilation unit. Each section carries its own identifier and field data. *)
val save :
  filename:string ->
  participants:Compilation_unit.t list ->
  solution:Reaper.Staged.solution ->
  slot_offsets:Slot_offsets.result ->
  unit

(** Read only the header. *)
val load : string -> t

val id_stamp_counters : t -> Id_stamp_counters.t

val participants : t -> Compilation_unit.t list

(** Check that [members] participated in the solve and provide lookups that
    import their owning compilation unit's section on demand. Imported sections
    are shared across the batch. *)
val solution_for_members :
  t -> members:Compilation_unit.t list -> Rebuild_solution.t
