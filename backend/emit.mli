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

(* Generation of assembly code *)

[@@@ocaml.warning "+a-40-41-42"]

val fundecl: Linear.fundecl -> unit
val data: Cmm.data_item list -> unit
val begin_assembly: (module Compiler_owee.Unix_intf.S) -> unit
val end_assembly: unit -> unit

(** Register a callback to receive filtered and formatted asm code, for expect
    tests. The callback is automatically cleared after being invoked once. It is
    possible to re-register the callback inside of the callback. *)
val register_filtered_assembly_callback : (string -> unit) -> unit
