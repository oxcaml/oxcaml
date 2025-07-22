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

val begin_assembly : sourcefile:string option -> unit

val end_assembly : unit -> unit

val close_out : unit -> unit

val open_out : asm_filename:string -> unit

val assemble_file : asm_filename:string -> obj_filename:string -> int

val init : output_prefix:string -> ppf_dump:Format.formatter -> unit
