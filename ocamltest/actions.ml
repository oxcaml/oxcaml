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

exception Is_static of Result.t

let combine ~drop_skip tasks =
  let dropped = ref false in
  match
    tasks |> List.filter_map (function
      | Static ({ status = (Fail | Predicate false); _ } as r) ->
         raise (Is_static r)
      | Static { status = (Pass | Predicate true); _ } ->
         None
      | Static ({ status = Skip; _ } as r) ->
         if drop_skip then (dropped := true; None)
         else raise (Is_static r)
      | Task tasks ->
         Some tasks)
  with
  | exception (Is_static r) -> Static r
  | [] -> Static (if !dropped then Result.skip else Result.pass)
  | tasks -> Task (List.concat tasks)

let seq tasks = combine ~drop_skip:false tasks
let par tasks = combine ~drop_skip:true tasks

type code = out_channel -> Environments.t -> task * Environments.t

type t = {
  name : string;
  action_description : string;
  body : code;
  does_something : bool;
  mutable hook : code option
}

let name a = a.name

let description a = a.action_description

let does_something t = t.does_something

let action_name = Variables.make ("action_name", "Name of the current action")

let make ~name ~description ~does_something body =
  { name; body; action_description = description; hook = None;
    does_something }

let update action code = { action with body = code }

let compare a1 a2 = String.compare a1.name a2.name

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register action =
  Hashtbl.add actions action.name action

let get_registered_actions () =
  let f _name action acc = action::acc in
  let unsorted_actions = Hashtbl.fold f actions [] in
  List.sort compare unsorted_actions

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let set_hook name hook =
  let action = (Hashtbl.find actions name) in
  action.hook <- Some hook

let clear_hook name =
  let action = (Hashtbl.find actions name) in
  action.hook <- None

let clear_all_hooks () =
  let f _name action = action.hook <- None in
  Hashtbl.iter f actions

let run log env action =
  let code = match action.hook with
    | None -> action.body
    | Some code -> code in
  let env = Environments.add action_name action.name env in
  code log env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)

let init () = Variables.register_variable action_name
