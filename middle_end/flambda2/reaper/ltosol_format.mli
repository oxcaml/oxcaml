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

module Serialisable_solution : sig
  type t

  val deserialise : t -> Unboxing_analysis.result
end

module File_contents : sig
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      participants : Compilation_unit.t list;
      solution : Serialisable_solution.t
    }
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(** Write an .ltosol file with the given solution to disk. [participants] should
    list the compilation units included in the solution. *)
val save :
  filename:string ->
  participants:Compilation_unit.t list ->
  solution:Unboxing_analysis.result ->
  unit

(** Read and unmarshal an ltosol file from disk. *)
val load : string -> File_contents.t
