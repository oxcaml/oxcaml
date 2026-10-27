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

(* Definition of actions, basic blocks for tests *)

type task_item =
  | Chdir of string
  | Check_file_exists of { filename: string; reason: string }
  | Check_file_absent of { filename: string }
  | Copy_file of { src: string; dst: string }
  | Setup_build_env of
      { source_dir: string;
        build_dir: string;
        remove: string list;
        files: string list;
        subdirs: string list }
  | Run_cmd of
      { settings: Run_command.settings;
        message: string;
        exits: (int * Result.t) list }
  | Compare_files of
      { kind_of_output: string;
        files: Filecompare.files;
        tool: Filecompare.tool;
        reference_exists: bool;
        promote: bool }

type task =
  | Static of Result.t
  | Task of task_item list

(** [seq tasks] passes if all of the tasks pass,
    and skips/fails if any tasks skip or fail *)
val seq : task list -> task

(** [par tasks] filters out the skipped tasks,
    and passes if the remaining ones pass *)
val par : task list -> task

type code = out_channel -> Environments.t -> task * Environments.t

type t

val name : t -> string

val description : t -> string

val action_name : Variables.t

val update : t -> code -> t

val make : name:string -> description:string ->
  does_something:bool -> code -> t

val compare : t -> t -> int

val register : t -> unit

val get_registered_actions : unit -> t list

val lookup : string -> t option

val set_hook : string -> code -> unit
val clear_hook : string -> unit
val clear_all_hooks : unit -> unit

val run : out_channel -> Environments.t -> t -> task * Environments.t

val does_something : t -> bool

module ActionSet : Set.S with type elt = t

val init : unit -> unit
