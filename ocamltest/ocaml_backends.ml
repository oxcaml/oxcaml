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

type t = Native | Bytecode | Jsir

let is_bytecode t = t=Bytecode

let is_native t = t=Native

let is_jsir t = t=Jsir

let string_of_backend = function
  | Native -> "native"
  | Bytecode -> "bytecode"
  | Jsir -> "jsir"

let module_extension = function
  | Bytecode -> "cmo"
  | Native -> "cmx"
  | Jsir -> "cmjx"

let library_extension = function
  | Bytecode -> "cma"
  | Native -> "cmxa"
  | Jsir -> "cmjxa"

let executable_extension = function
  | Bytecode -> "byte"
  | Native -> "opt"
  | Jsir -> "js"
