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

type backend_result = {
  emit : Compile_common.info -> unit;
  main_module_block_format : Lambda.main_module_block_format;
  arg_descr : Lambda.arg_descr option;
}

type backend = {
  of_typedtree :
    Compile_common.info ->
    Typedtree.implementation ->
    as_arg_for:Global_module.Parameter_name.t option ->
    backend_result;
  of_lambda :
    Compile_common.info ->
    Lambda.program ->
    as_arg_for:Global_module.Parameter_name.t option ->
    backend_result;
}

val interface : source_file:string -> output_prefix:string -> unit

val implementation :
  backend:backend ->
  start_from:Clflags.Compiler_pass.t ->
  source_file:string ->
  output_prefix:string ->
  keep_symbol_tables:bool ->
  unit

val instance :
  backend:backend ->
  source_file:string ->
  output_prefix:string ->
  compilation_unit:Compilation_unit.t ->
  runtime_args:Translmod.runtime_arg list ->
  main_module_block_size:int ->
  arg_descr:Lambda.arg_descr option ->
  keep_symbol_tables:bool ->
  unit

val run_jsoo_exn : args:string list -> unit
