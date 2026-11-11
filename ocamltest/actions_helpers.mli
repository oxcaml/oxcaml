(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Helper functions when writing actions *)

val fail_with_reason : string -> Actions.t

val skip_with_reason : string -> Actions.t

val predicate
  : bool -> string -> string -> Actions.code

val mkreason : string -> string -> int -> string

val testfile : Environments.t -> string

val test_build_directory : Environments.t -> string

val test_source_directory : Environments.t -> string

val words_of_variable : Environments.t -> Variables.t -> string list

val exit_status_of_variable : Environments.t -> Variables.t -> int

val readonly_files : Environments.t -> string list

val setup_symlinks : string -> string -> string list -> unit

val setup_build_env : bool -> ?remove:string list -> string list -> Actions.code

val setup_simple_build_env : bool -> string list -> Actions.code

val run_cmd :
  ?environment : string array ->
  ?stdin_variable : Variables.t ->
  ?stdout_variable : Variables.t ->
  ?stderr_variable : Variables.t ->
  ?append : bool ->
  ?timeout : int ->
  ?exits:(int * Result.t) list ->
  what:string ->
  out_channel -> Environments.t -> string list -> Actions.task

val run_program : Actions.code

val run_script : Actions.code

val run_config_script : Actions.code

val run_hook : string -> Actions.code

val compare_files : string -> string -> string -> Actions.code

val check_output : string -> Variables.t -> Variables.t -> Actions.code

(* Action evaluators *)
val do_setup_build_env :
  source_dir:string ->
  build_dir:string ->
  remove:string list ->
  files:string list ->
  subdirs:string list ->
  Result.t

val do_run_cmd :
  settings:Run_command.settings ->
  message:string ->
  exits:(int * Result.t) list ->
  out_channel ->
  Result.t

val do_compare_files : 
  kind_of_output:string ->
  files:Filecompare.files ->
  tool:Filecompare.tool ->
  promote:bool ->
  out_channel ->
  Result.t
