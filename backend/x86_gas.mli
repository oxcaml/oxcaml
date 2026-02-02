(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emit assembly instructions for gas. *)

[@@@ocaml.warning "+a-40-41-42"]

val generate_asm : out_channel -> X86_ast.asm_program -> unit

(** Register a callback to receive filtered assembly output. The callback is
    automatically cleared after being invoked. *)
val register_filtered_assembly_callback : (string -> unit) -> unit

(** Invoke all registered callbacks with filtered assembly from the program.
    Called by x86_proc.generate_code. *)
val invoke_filtered_assembly_callbacks : X86_ast.asm_program -> unit
