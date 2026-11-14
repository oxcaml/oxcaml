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

type t = Native | Bytecode | Jsir_js | Jsir_wasm

let is_bytecode t = t=Bytecode

let is_native t = t=Native

let is_jsir = function
  | Jsir_js | Jsir_wasm -> true
  | Native | Bytecode -> false

let string_of_backend = function
  | Native -> "native"
  | Bytecode -> "bytecode"
  | Jsir_js -> "jsir-js"
  | Jsir_wasm -> "jsir-wasm"

let module_extension = function
  | Bytecode -> "cmo"
  | Native -> "cmx"
  | Jsir_js -> "cmx-js"
  | Jsir_wasm -> "cmx-wasm"

let library_extension = function
  | Bytecode -> "cma"
  | Native -> "cmxa"
  | Jsir_js -> "cmxa-js"
  | Jsir_wasm -> "cmxa-wasm"

let executable_extension = function
  | Bytecode -> "byte"
  | Native -> "opt"
  | Jsir_js -> "js"
  (* wasm_of_ocaml programs are run through a JavaScript wrapper, which loads
     the WebAssembly code from a directory next to it. *)
  | Jsir_wasm -> "js"
