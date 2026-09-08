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

(** An .ltosol file whose header has been read. The solution itself is stored in
    one file section per compilation unit and is only read by
    [solution_for_members]. *)
type t

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** Write an .ltosol file with the given solution to disk, sharded into one file
    section per compilation unit. [participants] should list the compilation
    units included in the solution, each paired with the compilation units its
    dependency graph references; these determine which sections the unit's
    rebuild will need to read. [exported_to_other_units] should contain the
    symbols and code IDs referenced by another participant's code; each unit's
    rebuild demotes all of its other symbols to local. *)
val save :
  filename:string ->
  participants:(Compilation_unit.t * Compilation_unit.Set.t) list ->
  exported_to_other_units:Code_id_or_name.Set.t ->
  solution:Unboxing_analysis.result ->
  unit

(** Read the header of an ltosol file from disk. *)
val load : string -> t

val id_stamp_counters : t -> Id_stamp_counters.t

(** Deserialise the solution needed to rebuild [members], inserting the
    necessary objects into the global hashcons tables. Also returns the members'
    symbols and code IDs that other participants' code references (see [save]).
*)
val solution_for_members :
  t ->
  members:Compilation_unit.t list ->
  Unboxing_analysis.result * Code_id_or_name.Set.t
