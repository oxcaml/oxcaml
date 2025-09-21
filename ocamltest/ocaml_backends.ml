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

(* Backends of the OCaml compiler and their properties *)

type t = Native | Bytecode | Javascript

(* Backend filtering *)
module BackendSet = Set.Make(struct
  type nonrec t = t
  let compare = compare
end)

let selected_backends = ref (BackendSet.of_list [Bytecode; Native; Javascript])

let is_bytecode t = t=Bytecode

let is_native t = t=Native

let is_javascript t = t=Javascript

let string_of_backend = function
  | Native -> "native"
  | Bytecode -> "bytecode"
  | Javascript -> "javascript"

(* Creates a function that returns its first argument for Bytecode,          *)
(* its second argument for Native code, and its third for JavaScript         *)
let make_backend_function bytecode_value native_value javascript_value = function
  | Bytecode -> bytecode_value
  | Native -> native_value
  | Javascript -> javascript_value

let module_extension = make_backend_function "cmo" "cmx" "cmjo"

let library_extension = make_backend_function "cma" "cmxa" "cmja"

let executable_extension = make_backend_function "byte" "opt" "js"

let backend_of_string = function
  | "bytecode" -> Some Bytecode
  | "native" -> Some Native
  | "javascript" -> Some Javascript
  | _ -> None

let parse_backends_string s =
  let backend_names = String.split_on_char ',' s in
  let backends = List.fold_left (fun acc name ->
    let name = String.trim name in
    if name = "" then acc
    else match backend_of_string (String.lowercase_ascii name) with
    | Some b -> BackendSet.add b acc
    | None ->
        Printf.eprintf "Warning: unknown backend '%s' ignored\n%!" name;
        acc
  ) BackendSet.empty backend_names in
  backends

let set_enabled_backends backends =
  selected_backends := backends

let is_backend_enabled backend =
  BackendSet.mem backend !selected_backends
