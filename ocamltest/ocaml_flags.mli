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

(* The name of the variant that the compilers compile for when nothing
   selects one, and that the library root of every install holds: base. *)
val default_codegen_variant : string

(* The test's own selection, from the [codegen_variant] TSL variable
   ([None] when unset or empty).  The value is the name of a variant, or
   what the test needs of one: signed axes, +<axis> (the axis is on) and
   -<axis> (it is off), with or without blanks between them, as in "+fp",
   "+fp-sc" or "+fp -sc".  A value whose first character, blanks aside,
   is a sign is requirements; any other value is a name.  The selection is
   then the variant that the run uses anyway (the suite variant, else the
   install's default) when that one meets the requirements, and otherwise
   the available variant that meets them and differs from it on the fewest
   axes; when none is available, the variant that would have been
   closest, which [codegen_variant_available] then refuses.  A value that
   is neither a name nor requirements raises [Invalid_codegen_variant],
   here and in everything below that reads the selection. *)
val test_codegen_variant : Environments.t -> string option

(* A value of [codegen_variant] that is neither a variant name nor
   requirements.  The argument is the error to report for it. *)
exception Invalid_codegen_variant of string

(* The selection for the whole run: the operand of -codegen-variant, when
   it names a variant other than the install's default ([None] otherwise:
   asking for the default is the same as not asking). *)
val suite_codegen_variant : string option

(* The selected variant: the test's selection, else the run's. *)
val codegen_variant : Environments.t -> string option

(* The name of the codegen variant that a test block's native code is
   built for: the selected variant ([codegen_variant]), the configure-time
   default when nothing is selected.  Native compilations and links, the
   programs that compile in process (see [Ocaml_files.native_host]), the
   C code of native programs and the compilers that scripts start all
   follow it; bytecode and JSIR have no codegen variants. *)
val effective_codegen_variant : Environments.t -> string

(* [effective_codegen_variant] when it is not the install's default
   variant. *)
val non_default_codegen_variant : Environments.t -> string option

(* Whether [effective_codegen_variant] enables the given axis. *)
val effective_codegen_variant_mem :
  Environments.t -> Codegen_variant_name.token -> bool

(* Marks a task as work that depends on a non-default codegen variant.
   Under a suite variant with -codegen-variant-only, [Main.test_file] skips
   a test file whose task carries no such mark ([has_variant_work]): it
   would only repeat the default run. *)
val variant_work : Actions.task -> Actions.task
val has_variant_work : Actions.task -> bool

(* [variant_work_for env task] marks [task], which runs a command that is
   given the effective variant (a program built for it, its C flags, its
   OCAMLPARAM keys), when that variant is not the default one.  When the
   install does not have that variant, the result is a failure that says
   so instead: such a block needs the [variant_available] guard. *)
val variant_work_for : Environments.t -> Actions.task -> Actions.task

(* [variant_work_for] for a task that runs a compiler for [backend] with
   [codegen_variant_flags]: only a native compilation is given the
   variant. *)
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

(* The library tree of the named codegen variant: the install's library
   directory for the default variant, <stdlib>/variants/<name> for any
   other. *)
val codegen_variant_directory : string -> string

(* Whether the tested install can compile and link native code for the
   named codegen variant: it is the install's default variant, or the
   install has a library tree for it under <stdlib>/variants/. *)
val codegen_variant_available : string -> bool

(* The names for which [codegen_variant_available] holds: the default
   variant first, then the installed variant trees in sorted order. *)
val available_codegen_variants : unit -> string list

(* [-nostdlib -I <root>], where <root> is the library tree that a
   compilation for [backend] uses: the default one, or its
   variants/<name> subtree for a native compilation when a non-default
   codegen variant is selected (bytecode and JSIR compilations always use
   the default tree). *)
val stdlib : Environments.t -> backend:Ocaml_backends.t -> string

(* The native compiler flags selecting the selected codegen variant ("" when
   nothing is selected or the backend is not native). *)
val codegen_variant_flags : Environments.t -> Ocaml_backends.t -> string

(* Resolves the directories of the [directories] variable in the library
   tree of the codegen variant that a compilation for [backend] uses.  A
   test names the default variant's directories: <srcdir>/otherlibs/<lib>
   (e.g. via [include unix]), <srcdir>/otherlibs/eval, <srcdir>/testsuite/lib
   ([include testing]) and the directories of the compiler's own libraries
   ([include ocamlcommon], [include config]).  For a native compilation
   under a non-default variant, each of them becomes the directory of the
   variant's tree that holds the same library, when the tree has it;
   directories that end up equal are given once.  The list is returned
   unchanged otherwise. *)
val variant_library_directories :
  Environments.t -> backend:Ocaml_backends.t -> string list -> string list

(* The same for the directories given to -I in a string of flags (the
   value of [flags] or [ocamlopt_flags]), which is returned with those
   directories replaced. *)
val variant_include_flags :
  Environments.t -> backend:Ocaml_backends.t -> string -> string

(* What [stdlib] is bound to at the start of a test: a reference to the
   variable [codegen_variant_stdlib].  [with_variant_stdlib env] is [env]
   with that variable bound to the library tree of the effective variant;
   it is applied before every test and action of a test file, so [stdlib]
   follows the selection for as long as the test does not assign it. *)
val variant_stdlib_reference : string
val with_variant_stdlib : Environments.t -> Environments.t

(* Takes out of the environment of ocamltest, which its commands inherit,
   the axis keys that come before the '_' of OCAMLPARAM: they are what an
   ocamltest that started this one gave to its own command (see
   [command_environment]), and would override the variant of the tests of
   this one.  To call once, before any test runs. *)
val clear_inherited_axis_defaults : unit -> unit

(* The axis settings that come after the '_' of the OCAMLPARAM that
   ocamltest inherited, as they are written there ("frame-pointers=1").
   The compilers obey such a setting whatever ocamltest gives them, so it
   overrides the variant of every test: ocamltest does not run with one. *)
val inherited_axis_settings : unit -> string list

(* Records the backend of the compiler or of the toplevel that a build
   environment is set up for.  The C code that a test builds itself (the
   [cc] action, [cflags] and [mkexe] in a script) is built for the
   effective variant, except in a block set up for bytecode or JSIR: such
   programs have no codegen variant, and their C code is for the bytecode
   runtime, which is the default variant's. *)
val set_build_env_backend :
  Ocaml_backends.t -> Environments.t -> Environments.t

(* [variant_work_for] for a task that builds C code of the block, as the
   [cc] action does: only where that code is built for the variant. *)
val c_code_variant_work : Environments.t -> Actions.task -> Actions.task

(* The flags of the [cc] action: the configured ones, followed by the flags
   that state the effective variant for the C code of the block
   ([Codegen_variant_name.variant_cc_flags]: its axis macros and, for
   example, the sanitizer flags) when that code is built for a variant
   other than the default one. *)
val cflags : Environments.t -> string

(* [env] for a command that a test writes itself (a script, a hook, the
   program of [run], a compiler given its whole command line, ocamlmklib):
   under a non-default effective variant, OCAMLPARAM makes that variant
   the default of the compilers that the command starts, as it would be on
   an install configured with it, and [cflags] and [mkexe] are followed by
   the flags that state the variant for the C code of the block (tests
   also compile C files with [mkexe], and an AddressSanitizer variant
   needs the sanitizer at the link). *)
val command_environment : Environments.t -> Environments.t

(* [env] with [cflags] and [mkexe] followed by the flags that state the
   effective variant for the C code of the block, as in
   [command_environment]: the environment in which to read the flags that
   a test gives a native compiler, which may name the two variables (in
   -ccopt '${cflags}', or in a preprocessor given with -pp). *)
val with_variant_c_flags : Environments.t -> Environments.t

(* [run_for_variant run], where [run] is one of
   [Actions_helpers.run_program], [run_script], [run_config_script] and
   [run_hook name]: the command runs in [command_environment], and under
   a non-default effective variant it is marked as [variant_work]. *)
val run_for_variant :
  (command_env:(Environments.t -> Environments.t) -> Actions.code) ->
  Actions.code

val include_toplevel_directory : string

val c_includes : string

val target_flags : Ocaml_backends.t -> string

val runtime_flags :
  Environments.t -> Ocaml_backends.t -> bool -> string

val toplevel_default_flags : string

val ocamlobjinfo_default_flags : string
