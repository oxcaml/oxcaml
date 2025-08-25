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

type t = Native | Bytecode | JavaScript

let is_bytecode t = t=Bytecode

let is_native t = t=Native

let is_javascript t = t=JavaScript

let string_of_backend = function
  | Native -> "native"
  | Bytecode -> "bytecode"
  | JavaScript -> "javascript"

(* Creates a function that returns its first argument for Bytecode           *)
(* and its second argument for Native code and its third argument for        *)
(* JavaScript                                                                *)
let make_backend_function bytecode_value native_value js_value = function
  | Bytecode -> bytecode_value
  | Native -> native_value
  | JavaScript -> js_value

let module_extension = make_backend_function "cmo" "cmx" "cmj"

let library_extension = make_backend_function "cma" "cmxa" "cmja"

let executable_extension = make_backend_function "byte" "opt" "js"
