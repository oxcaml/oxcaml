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

(** Bytecode compilation for .ml and .mli files. *)

val interface:
  source_file:string -> output_prefix:string -> unit
val implementation:
  start_from:Clflags.Compiler_pass.t ->
  source_file:string -> output_prefix:string -> keep_symbol_tables:bool -> unit
val instance:
  source_file:string -> output_prefix:string ->
  compilation_unit:Compilation_unit.t ->
  runtime_args:Translmod.runtime_arg list ->
  main_module_block_repr:Lambda.module_representation ->
  arg_descr:Lambda.arg_descr option ->
  keep_symbol_tables:bool -> unit


(** {2 Internal functions} **)

val to_bytecode :
  Compile_common.info ->
  Typedtree.implementation ->
  as_arg_for:Global_module.Parameter_name.t option ->
  Instruct.instruction list * Compilation_unit.Set.t *
    Lambda.main_module_block_format *
    Lambda.arg_descr option
(** [to_bytecode info typed] takes a typechecked implementation
    and returns its bytecode.
*)

val emit_bytecode :
  Compile_common.info ->
  Instruct.instruction list * Compilation_unit.Set.t *
    Lambda.main_module_block_format *
    Lambda.arg_descr option ->
    unit
(** [emit_bytecode bytecode] output the bytecode executable. *)

val tlambda_to_bytecode :
  Compile_common.info ->
  Lambda.program ->
  as_arg_for:Global_module.Parameter_name.t option ->
  Instruct.instruction list * Compilation_unit.Set.t *
    Lambda.main_module_block_format *
    Lambda.arg_descr option
(** [tlambda_to_bytecode info program] lowers a Lambda program to bytecode.
    Used by [-functorize] (and [-instantiate]) to compile the bundle's
    synthesised program after [Translmod.transl_functorize]. *)

val with_info :
  source_file:string ->
  output_prefix:string ->
  compilation_unit:Compile_common.compilation_unit_or_inferred ->
  kind:Unit_info.intf_or_impl ->
  dump_ext:string ->
  (Compile_common.info -> 'a) -> 'a
(** Bytecode-backend wrapper around [Compile_common.with_info]. *)
