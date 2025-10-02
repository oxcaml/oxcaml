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

(** Native compilation for .ml and .mli files. *)

val interface: source_file:string -> output_prefix:string -> unit

val implementation
   : machine_width:Target_system.Machine_width.t
  -> (module Compiler_owee.Unix_intf.S)
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    machine_width:Target_system.Machine_width.t ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Cmm.phrase list)
  -> start_from:Clflags.Compiler_pass.t
  -> source_file:string -> output_prefix:string -> keep_symbol_tables:bool
  -> unit

val instance
   : machine_width:Target_system.Machine_width.t
  -> (module Compiler_owee.Unix_intf.S)
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    machine_width:Target_system.Machine_width.t ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Cmm.phrase list)
  -> source_file:string -> output_prefix:string
  -> compilation_unit:Compilation_unit.t
  -> runtime_args:Translmod.runtime_arg list
  -> main_module_block_size:int
  -> arg_descr:Lambda.arg_descr option
  -> keep_symbol_tables:bool
  -> unit
