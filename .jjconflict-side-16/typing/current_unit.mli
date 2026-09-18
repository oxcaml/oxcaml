(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The compilation unit currently being compiled. *)

val get : unit -> Unit_info.t option
val get_cu : unit -> Compilation_unit.t option
val get_cu_or_dummy : unit -> Compilation_unit.t
val get_cu_exn : unit -> Compilation_unit.t
val is_current : Compilation_unit.t -> bool
val set : Unit_info.t -> unit
val unset : unit -> unit

module Name : sig
  val get : unit -> string
  val is : string -> bool
  val is_ident : Ident.t -> bool
  val is_path : Path.t -> bool
end

(** It is assumed that the provided [Ident.t] is in the current unit. *)
val symbol_for_local_ident : Ident.t -> Symbol.t
val symbol : unit -> Symbol.t
val symbol_for_new_const : unit -> Symbol.t
