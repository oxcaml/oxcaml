(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Flags used in OCaml commands *)

(* The value of the [codegen_variant] TSL variable ([None] when unset). *)
val codegen_variant : Environments.t -> string option

(* The configure-time default variant's name, composed from the copied
   utils/config.ml. *)
val default_codegen_variant : string

(* [-nostdlib -I <root>], where <root> honors the [codegen_variant] TSL
   variable: the base standard library, or its variants/<name> subtree when
   a non-default codegen variant is selected. *)
val stdlib : Environments.t -> string

(* The native compiler flags selecting the codegen variant named by the
   [codegen_variant] TSL variable ("" when the variable is unset or the
   backend is bytecode). *)
val codegen_variant_flags : Environments.t -> Ocaml_backends.t -> string

(* Resolves a library directory added by a library modifier
   (<srcdir>/otherlibs/<lib>, e.g. via [include unix]) against the library
   root of the variant named by the [codegen_variant] TSL variable, when a
   non-default variant is selected and the variant tree provides the
   library; returns the directory unchanged otherwise. *)
val variant_library_directory : Environments.t -> string -> string

val include_toplevel_directory : string

val c_includes : string

val target_flags : Ocaml_backends.t -> string

val runtime_flags :
  Environments.t -> Ocaml_backends.t -> bool -> string

val toplevel_default_flags : string

val ocamlobjinfo_default_flags : string
