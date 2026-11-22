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

(* The configure-time default variant's name, composed from the copied
   utils/config.ml. *)
val default_codegen_variant : string

(* The test's own selection: the value of the [codegen_variant] TSL
   variable ([None] when unset or empty). *)
val test_codegen_variant : Environments.t -> string option

(* The selection for the whole run: the operand of -codegen-variant, when
   it names a variant other than the install's default ([None] otherwise:
   asking for the default is the same as not asking). *)
val suite_codegen_variant : string option

(* The selected variant: the test's selection, else the run's. *)
val codegen_variant : Environments.t -> string option

(* The codegen variant of the in-process expect/toplevel host binaries
   (expect/expect.opt, ocaml/ocamlnat): the configure default, which is
   the variant of the build context they come from. *)
val toplevel_host_codegen_variant : string

(* The name of the codegen variant a test block's native compilations
   effectively use: the selected variant ([codegen_variant]), the
   configure-time default when nothing is selected. *)
val effective_codegen_variant : Environments.t -> string

(* Whether [effective_codegen_variant] enables the given axis ([false] for
   unknown variant names, which the compile actions reject loudly). *)
val effective_codegen_variant_mem :
  Environments.t -> Codegen_variant_name.token -> bool

(* Marks a task as work that depends on a non-default codegen variant.
   Under a suite variant with -codegen-variant-only, [Main.test_file] skips
   a test file whose task carries no such mark ([has_variant_work]): it
   would only repeat the default run. *)
val variant_work : Actions.task -> Actions.task
val has_variant_work : Actions.task -> bool

(* [native_variant_work env backend task] marks [task], which runs a compiler
   for [backend] with [codegen_variant_flags], when that is a native
   compilation for a non-default variant. *)
val native_variant_work :
  Environments.t -> Ocaml_backends.t -> Actions.task -> Actions.task

(* The body of a predicate on the effective variant:
   [codegen_variant_predicate holds pass_reason skip_reason], where [holds]
   is a property of a variant name.  When it holds for the effective
   variant but not for the default one, the result is marked as
   [variant_work]. *)
val codegen_variant_predicate :
  (string -> bool) -> string -> string -> Actions.code

(* [codegen_variant_predicate] for "the given axis is enabled"
   ([~enabled:true]) or "is disabled" ([~enabled:false]). *)
val codegen_variant_axis_predicate :
  Codegen_variant_name.token -> enabled:bool -> string -> string ->
  Actions.code

(* Whether the tested install can compile and link native code for the
   named codegen variant: it is the install's default variant, or the
   install has a library tree for it under <stdlib>/variants/. *)
val codegen_variant_available : string -> bool

(* The names for which [codegen_variant_available] holds: the default
   variant first, then the installed variant trees in sorted order. *)
val available_codegen_variants : unit -> string list

(* [-nostdlib -I <root>], where <root> honors the selected codegen variant:
   the base standard library, or its variants/<name> subtree when a
   non-default codegen variant is selected.  A native compilation follows
   [codegen_variant]; a bytecode one only the test's own selection (the
   suite variant leaves bytecode alone); a JSIR one always gets the base
   root. *)
val stdlib : Environments.t -> backend:Ocaml_backends.t -> string

(* Like [stdlib], but for in-process expect/toplevel host binaries: native
   hosts resolve the standard library against
   [toplevel_host_codegen_variant]'s root rather than the configure
   default's, matching the flag defaults baked into those binaries (the
   only [codegen_variant] selection they admit is that same variant). *)
val toplevel_stdlib : Environments.t -> backend:Ocaml_backends.t -> string

(* The native compiler flags selecting the selected codegen variant ("" when
   nothing is selected or the backend is not native). *)
val codegen_variant_flags : Environments.t -> Ocaml_backends.t -> string

(* Like [codegen_variant_flags], but for modules compiled out-of-process for
   loading into in-process expect/toplevel host binaries: native
   compilations target [toplevel_host_codegen_variant] (the hosts' baked-in
   flag defaults) rather than the configure default. *)
val toplevel_codegen_variant_flags :
  Environments.t -> Ocaml_backends.t -> string

(* Resolves a library directory added by a library modifier
   (<srcdir>/otherlibs/<lib>, e.g. via [include unix], or
   <srcdir>/testsuite/lib via [include testing]) against the library root
   of the selected codegen variant, when a non-default variant is selected
   and the variant tree provides the library; returns the directory
   unchanged otherwise.  The variant is chosen per backend, as for
   [stdlib]. *)
val variant_library_directory :
  Environments.t -> backend:Ocaml_backends.t -> string -> string

(* For a native compilation that follows the suite variant
   ([suite_codegen_variant] with no selection by the test): the first
   library it names, through the [directories] variable or through -I in
   [flags] or [ocamlopt_flags], that is only built for the install's
   default variant ("compiler-libs", "the debugger" or "eval").  [None]
   for every other compilation. *)
val default_only_library :
  Environments.t -> Ocaml_backends.t -> string option

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
