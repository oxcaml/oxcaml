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

(* The codegen variant of the in-process expect/toplevel host binaries
   (expect/expect.opt, ocaml/ocamlnat): the configure default with the
   AddressSanitizer axis forced off, because the compiler contexts build
   uninstrumented even on an asan-default configure. *)
val toplevel_host_codegen_variant : string

(* The name of the codegen variant a test block's native compilations
   effectively use: the [codegen_variant] TSL variable when set, the
   configure-time default otherwise. *)
val effective_codegen_variant : Environments.t -> string

(* Whether [effective_codegen_variant] enables the given axis ([false] for
   unknown variant names, which the compile actions reject loudly). *)
val effective_codegen_variant_mem :
  Environments.t -> Codegen_variant_name.token -> bool

(* [-nostdlib -I <root>], where <root> honors the [codegen_variant] TSL
   variable: the base standard library, or its variants/<name> subtree when
   a non-default codegen variant is selected. *)
val stdlib : Environments.t -> string

(* Like [stdlib], but for in-process expect/toplevel host binaries: native
   hosts resolve the standard library against
   [toplevel_host_codegen_variant]'s root rather than the configure
   default's, matching the flag defaults baked into those binaries (the
   only [codegen_variant] selection they admit is that same variant). *)
val toplevel_stdlib : Environments.t -> backend:Ocaml_backends.t -> string

(* The native compiler flags selecting the codegen variant named by the
   [codegen_variant] TSL variable ("" when the variable is unset or the
   backend is bytecode). *)
val codegen_variant_flags : Environments.t -> Ocaml_backends.t -> string

(* Like [codegen_variant_flags], but for modules compiled out-of-process for
   loading into in-process expect/toplevel host binaries: native
   compilations target [toplevel_host_codegen_variant] (the hosts' baked-in
   flag defaults) rather than the configure default. *)
val toplevel_codegen_variant_flags :
  Environments.t -> Ocaml_backends.t -> string

(* Resolves a library directory added by a library modifier
   (<srcdir>/otherlibs/<lib>, e.g. via [include unix]) against the library
   root of the variant named by the [codegen_variant] TSL variable, when a
   non-default variant is selected and the variant tree provides the
   library; returns the directory unchanged otherwise. *)
val variant_library_directory : Environments.t -> string -> string

(* Like [variant_library_directory], but for in-process expect/toplevel
   host binaries: native hosts resolve library-modifier directories against
   [toplevel_host_codegen_variant]'s root, matching [toplevel_stdlib]. *)
val toplevel_library_directory :
  Environments.t -> backend:Ocaml_backends.t -> string -> string

val include_toplevel_directory : string

val c_includes : string

val target_flags : Ocaml_backends.t -> string

val runtime_flags :
  Environments.t -> Ocaml_backends.t -> bool -> string

val toplevel_default_flags : string

val ocamlobjinfo_default_flags : string
