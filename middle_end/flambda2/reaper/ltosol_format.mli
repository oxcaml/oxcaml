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

(** A solution read from a file, before the import of its data. *)
type t

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

val save :
  filename:string ->
  participants:Compilation_unit.Set.t ->
  solution:[`Without_types] Analysis.Solution.t ->
  unit

val load : string -> t

val id_stamp_counters : t -> Flambda_cmx_format.Id_stamp_counters.t

val participants : t -> Compilation_unit.Set.t

val solution_for_members :
  t -> members:Compilation_unit.t list -> [`Without_types] Analysis.Solution.t
