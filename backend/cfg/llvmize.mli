(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Jane Street                               *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generate LLVM IR *)
val cfg : Cfg_with_layout.t -> unit

val data : Cmm.data_item list -> unit

val begin_assembly : unit -> unit

val end_assembly : sourcefile:string option -> unit

val close_out : unit -> unit

val open_out : asm_filename:string -> output_prefix:string -> unit
