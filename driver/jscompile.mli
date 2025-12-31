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

(** Js_of_ocaml IR compilation for .ml and .mli files. *)

val interface : source_file:string -> output_prefix:string -> unit

val implementation :
  start_from:Clflags.Compiler_pass.t ->
  source_file:string ->
  output_prefix:string ->
  keep_symbol_tables:bool ->
  unit

val instance :
  source_file:string ->
  output_prefix:string ->
  compilation_unit:Compilation_unit.t ->
  runtime_args:Translmod.runtime_arg list ->
  main_module_block_repr:Lambda.module_representation ->
  arg_descr:Lambda.arg_descr option ->
  keep_symbol_tables:bool ->
  unit

(** {2 Internal functions} **)

val to_jsir :
  Compile_common.info ->
  Typedtree.implementation ->
  as_arg_for:Global_module.Parameter_name.t option ->
  Flambda2_to_jsir.To_jsir_result.program
(** [to_jsir info typed] takes a typechecked implementation and returns its
    Js_of_ocaml IR representation. *)
