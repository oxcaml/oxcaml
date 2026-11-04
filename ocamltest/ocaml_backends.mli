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

(** [Jsir_js] and [Jsir_wasm] are [ocamlopt -target js_of_ocaml] and
    [ocamlopt -target wasm_of_ocaml] *)
type t = Native | Bytecode | Jsir_js | Jsir_wasm

val is_bytecode : t -> bool

val is_native : t -> bool

(** Whether the backend is [Jsir_js] or [Jsir_wasm] *)
val is_jsir : t -> bool

val string_of_backend : t -> string

val module_extension : t -> string

val library_extension : t -> string

val executable_extension : t -> string
