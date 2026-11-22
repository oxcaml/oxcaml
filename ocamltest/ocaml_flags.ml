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

(* Composed from the copied utils/config.ml, which carries the configure
   default of every axis (a new axis adds its [Config] read here). *)
let compose_codegen_variant ~address_sanitizer =
  Codegen_variant_name.of_tokens
    ((if address_sanitizer
      then [Codegen_variant_name.Address_sanitizer]
      else [])
     @ (if Config.with_frame_pointers
        then [Codegen_variant_name.Frame_pointers]
        else [])
     @ (if Config.poll_insertion
        then [Codegen_variant_name.Poll_insertion]
        else [])
     @ (if Config.no_stack_checks
        then []
        else [Codegen_variant_name.Stack_checks]))

let default_codegen_variant =
  Codegen_variant_name.to_string
    (compose_codegen_variant ~address_sanitizer:Config.with_address_sanitizer)

(* A codegen variant (absolute name, e.g. "base" or "fp"; see
   utils/codegen_variant_name.ml, copied into ocamltest) is selected either
   by a test, with the [codegen_variant] TSL variable, or for the whole run,
   with the -codegen-variant option (the "suite variant").  The test's
   selection wins; an empty [codegen_variant] selects nothing, so the suite
   variant applies. *)
let test_codegen_variant env =
  match Environments.safe_lookup Ocaml_variables.codegen_variant env with
  | "" -> None
  | name -> Some name

(* Asking for the install's default variant is the same as not asking:
   [make test CODEGEN_VARIANT=base] on a base-default install is
   [make test]. *)
let suite_codegen_variant =
  match Options.codegen_variant with
  | "" -> None
  | name when String.equal name default_codegen_variant -> None
  | name -> Some name

(* The selected variant is what native compilation and linking target.  It
   swaps the standard-library include path to the variant's library root
   (<stdlib>/variants/<name>) when the named variant is not the install's
   configure-time default, and adds the corresponding per-axis compiler
   flags to native compiler invocations (see [codegen_variant_flags]). *)
let codegen_variant env =
  match test_codegen_variant env with
  | Some _ as selection -> selection
  | None -> suite_codegen_variant

(* The variant whose library root a compilation for [backend] uses.
   Bytecode has no codegen variants and bytecode compilers do not accept
   the flags.  Under a test's own selection they still use the variant
   root, whose interfaces and bytecode artifacts are copies of the default
   root's, so that one test block can mix backends.  The suite variant
   leaves them alone: bytecode steps then run exactly as in a run without
   it, which matters for the C stub archives of an AddressSanitizer
   variant root (instrumented, so a -custom bytecode link against them
   lacks the sanitizer runtime).  The JSIR targets have no codegen variants
   either, and the variant roots carry no JSIR libraries. *)
let library_codegen_variant env (backend : Ocaml_backends.t) =
  match backend with
  | Native -> codegen_variant env
  | Bytecode -> test_codegen_variant env
  | Jsir_js | Jsir_wasm -> None

(* Marks [task] as work that depends on a non-default codegen variant: a
   native compilation that is given such a variant's flags
   ([native_variant_work]), or what a guard lets through when it holds for
   the effective variant but not for the default one
   ([codegen_variant_predicate]).  Under a suite variant, a test file whose
   task carries no such mark would only repeat the default run, so
   [Main.test_file] skips it when the run is restricted to what depends on
   the variant (-codegen-variant-only).  The mark is part of the task, so
   it goes away with a branch that is dropped. *)
let variant_work task =
  Actions.seq [Actions.Task [Depends_on_codegen_variant]; task]

let has_variant_work (task : Actions.task) =
  match task with
  | Static _ -> false
  | Task items ->
    List.exists
      (fun (item : Actions.task_item) ->
        match item with
        | Depends_on_codegen_variant -> true
        | Predicate_pass _ | Chdir _ | Check_file_exists _
        | Check_file_absent _ | Copy_file _ | Setup_build_env _ | Run_cmd _
        | Compare_files _ -> false)
      items

(* The expect/toplevel tools (expect/expect.opt, ocaml/ocamlnat) compile
   and load phrases in-process, so what matters for them is the variant
   the HOST BINARY was built for.  Those binaries come from the main build
   context, whose variant is the configure default; this mirrors the
   toplevel_host_variant constants baked into the binaries themselves
   (toplevel/native/dune). *)
let toplevel_host_codegen_variant_name =
  compose_codegen_variant ~address_sanitizer:Config.with_address_sanitizer

let toplevel_host_codegen_variant =
  Codegen_variant_name.to_string toplevel_host_codegen_variant_name

(* The codegen variant a test block's native compilations effectively use:
   every axis flag defaults to the configure default (the default variant),
   and ocamltest only deviates from that when the [codegen_variant] TSL
   variable injects a variant's flags (see [codegen_variant_flags];
   per-test [flags +=] additions are not visible here). *)
let effective_codegen_variant env =
  match codegen_variant env with
  | None -> default_codegen_variant
  | Some name -> name

(* Per-axis read of the effective variant, for the axis predicates
   (frame_pointers, stack-checks, poll-insertion, address-sanitizer and
   their duals): each predicate's meaning is the axis's value in the
   variant the block actually compiles for, not the raw configure default.
   An unknown variant name has no axes ([false] for every token); the
   compile actions reject such names loudly ([codegen_variant_flags]). *)
let codegen_variant_name_mem name token =
  match Codegen_variant_name.of_string name with
  | Some variant -> Codegen_variant_name.mem token variant
  | None -> false

let effective_codegen_variant_mem env token =
  codegen_variant_name_mem (effective_codegen_variant env) token

(* A predicate on the effective variant: [holds] applied to its name.  When
   it holds for the effective variant but not for the default one, what it
   lets through is work that only this variant can do, whatever the backend
   of that work (see [variant_work]). *)
let codegen_variant_predicate holds pass_reason skip_reason log env =
  let satisfied = holds (effective_codegen_variant env) in
  let task, env =
    Actions_helpers.predicate satisfied pass_reason skip_reason log env
  in
  let task =
    if satisfied && not (holds default_codegen_variant)
    then variant_work task
    else task
  in
  task, env

let codegen_variant_axis_predicate token ~enabled =
  codegen_variant_predicate (fun name ->
    Bool.equal (codegen_variant_name_mem name token) enabled)

let codegen_variant_directory name =
  if String.equal name default_codegen_variant
  then Ocaml_directories.stdlib
  else
    Ocamltest_stdlib.Filename.make_path
      [Ocaml_directories.stdlib; "variants"; name]

(* Whether the tested install can compile and link native code for the named
   codegen variant: either the variant is the configure-time default (served
   by the standard-library root itself), or the install provides a library
   tree for it under <stdlib>/variants/.  Checked against the actual tree
   being tested rather than configure-time truth, so that predicates stay
   correct if the variant trees are absent (e.g. --disable-codegen-variants
   builds, or incomplete installs). *)
let codegen_variant_available name =
  String.equal name default_codegen_variant
  || (let dir = codegen_variant_directory name in
      Sys.file_exists dir && Sys.is_directory dir)

let available_codegen_variants () =
  let variants =
    Ocamltest_stdlib.Filename.make_path [Ocaml_directories.stdlib; "variants"]
  in
  let installed =
    if Sys.file_exists variants && Sys.is_directory variants
    then
      List.filter codegen_variant_available
        (List.sort String.compare (Array.to_list (Sys.readdir variants)))
    else []
  in
  default_codegen_variant :: installed

let stdlib env ~backend =
  let stdlib_path =
    match library_codegen_variant env backend with
    | None -> Ocaml_directories.stdlib
    | Some name -> codegen_variant_directory name
  in
  "-nostdlib -I " ^ stdlib_path

(* Like [stdlib], but for in-process expect/toplevel host binaries: a
   NATIVE host compiles its phrases for [toplevel_host_codegen_variant]
   (its baked-in flag defaults), so the standard library must come from
   that variant's root rather than the configure default's.  The only
   [codegen_variant] selection admitted for a native host is that same
   variant ([reject_in_process_codegen_variant] in ocaml_actions.ml), so
   the selection never changes the answer.  Bytecode phrase compilation is
   unaffected by codegen variants, so bytecode hosts keep [stdlib]'s
   resolution. *)
let toplevel_stdlib env ~(backend : Ocaml_backends.t) =
  match backend with
  | Bytecode | Jsir_js | Jsir_wasm -> stdlib env ~backend
  | Native ->
    "-nostdlib -I " ^ codegen_variant_directory toplevel_host_codegen_variant

let codegen_variant_flags env backend =
  match backend, codegen_variant env with
  | Ocaml_backends.(Bytecode | Jsir_js | Jsir_wasm), _ | _, None -> ""
  | Ocaml_backends.Native, Some name ->
    (match Codegen_variant_name.of_string name with
     | Some variant ->
       String.concat " " (Codegen_variant_name.ocamlopt_flags variant)
     | None ->
       Printf.ksprintf failwith
         "unknown codegen_variant %S (expected a variant name such as \
          \"base\" or \"fp\")" name)

(* [task] runs a compiler for [backend] with [codegen_variant_flags]. *)
let native_variant_work env (backend : Ocaml_backends.t) task =
  match backend, codegen_variant env with
  | Native, Some name when not (String.equal name default_codegen_variant) ->
    variant_work task
  | Native, (Some _ | None) | (Bytecode | Jsir_js | Jsir_wasm), _ -> task

(* Like [codegen_variant_flags], but for modules compiled out-of-process
   for loading into in-process expect/toplevel host binaries: a native
   host loads code built for [toplevel_host_codegen_variant] (its own
   baked-in flag defaults), so the modules must be compiled with that
   variant's flags whatever variant the test selects.
   Bytecode is unaffected by codegen variants, as are the JSIR targets
   (which have no toplevel).  Mirrors [toplevel_stdlib]. *)
let toplevel_codegen_variant_flags env (backend : Ocaml_backends.t) =
  match backend with
  | Bytecode | Jsir_js | Jsir_wasm -> codegen_variant_flags env backend
  | Native ->
    String.concat " "
      (Codegen_variant_name.ocamlopt_flags toplevel_host_codegen_variant_name)

(* Library modifiers (see ocaml_modifiers.ml, e.g. [include unix]) append
   default-tree library directories of the form <srcdir>/otherlibs/<lib>,
   and [include testing] and [include tool-ocaml-lib] append
   <srcdir>/testsuite/lib.  Under a non-default [codegen_variant] such a
   directory would mix the default tree's artifacts into a variant
   compilation, so resolve it against the variant's library root instead
   (the read-time analogue of the [stdlib] remap above) whenever the variant
   tree provides the library; directories without a variant counterpart,
   and every directory when the variant is unset or the default, are left
   unchanged.  The systhreads source directory is named "systhreads", but
   it installs (and gets variant trees) as "threads".  The testsuite's own
   libraries are built per variant under <srcdir>/testsuite/lib/variants
   (by [make install_for_test]). *)
let library_directory_for_variant name dir =
  if String.equal name default_codegen_variant then dir
  else begin
    let otherlibs =
      Ocamltest_stdlib.Filename.make_path
        [Ocaml_directories.srcdir; "otherlibs"]
      ^ Filename.dir_sep
    in
    let testsuite_lib =
      Ocamltest_stdlib.Filename.make_path
        [Ocaml_directories.srcdir; "testsuite"; "lib"]
    in
    let candidate =
      if String.equal dir testsuite_lib
      then
        Some
          (Ocamltest_stdlib.Filename.make_path
             [testsuite_lib; "variants"; name])
      else
        match Ocamltest_stdlib.String.remove_prefix ~prefix:otherlibs dir with
        | Some rest ->
          let rest =
            if String.equal rest "systhreads" then "threads" else rest
          in
          Some
            (Ocamltest_stdlib.Filename.make_path
               [codegen_variant_directory name; rest])
        | None -> None
    in
    match candidate with
    | Some candidate
      when Sys.file_exists candidate && Sys.is_directory candidate ->
      candidate
    | Some _ | None -> dir
  end

(* The compiler's own libraries, the debugger and eval are only built for
   the install's default variant (the variant trees deliberately leave
   them out), so a native compilation that names one of their directories
   cannot target another variant. *)
let default_only_libraries =
  List.map
    (fun (library, path) -> library, Ocaml_directories.srcdir_path path)
    (List.map
       (fun subdir -> "compiler-libs", [subdir])
       Ocaml_modifiers.compilerlibs_subdirs
     @ ["the debugger", ["debugger"]; "eval", ["otherlibs"; "eval"]])

let include_directories_of_flags flags =
  let rec directories = function
    | "-I" :: dir :: words -> dir :: directories words
    | _ :: words -> directories words
    | [] -> []
  in
  directories (Ocamltest_stdlib.String.words flags)

(* For a native compilation that follows the suite variant: the first
   library with no variant tree that it names, through the [directories]
   variable (which the library modifiers fill) or through -I in [flags] or
   [ocamlopt_flags].  A test that selects a variant itself is not asked:
   its author chose the combination. *)
let default_only_library env (backend : Ocaml_backends.t) =
  match backend, test_codegen_variant env, suite_codegen_variant with
  | Native, None, Some _ ->
    let lookup variable = Environments.safe_lookup variable env in
    let named =
      Ocamltest_stdlib.String.words (lookup Ocaml_variables.directories)
      @ include_directories_of_flags (lookup Ocaml_variables.flags)
      @ include_directories_of_flags (lookup Ocaml_variables.ocamlopt_flags)
    in
    let library_of dir =
      List.find_map
        (fun (library, default_only) ->
          if String.equal dir default_only
             || String.starts_with
                  ~prefix:(default_only ^ Filename.dir_sep) dir
          then Some library
          else None)
        default_only_libraries
    in
    List.find_map library_of named
  | Native, Some _, _ | Native, None, None
  | (Bytecode | Jsir_js | Jsir_wasm), _, _ -> None

let variant_library_directory env ~backend dir =
  match library_codegen_variant env backend with
  | None -> dir
  | Some name -> library_directory_for_variant name dir

(* Like [variant_library_directory], but for in-process expect/toplevel
   host binaries: a native host compiles its phrases for
   [toplevel_host_codegen_variant], so library-modifier directories must
   resolve against that variant's root too (the companion of the
   [toplevel_stdlib] remap above). *)
let toplevel_library_directory env ~(backend : Ocaml_backends.t) dir =
  match backend with
  | Bytecode | Jsir_js | Jsir_wasm ->
    variant_library_directory env ~backend dir
  | Native -> library_directory_for_variant toplevel_host_codegen_variant dir

let include_toplevel_directory =
  "-I " ^ Ocaml_directories.toplevel

let c_includes =
  let dir = Ocaml_directories.runtime in
  "-ccopt -I" ^ dir

let runtime_variant_flags () = match Ocaml_files.runtime_variant() with
  | Ocaml_files.Normal -> ""
  | Ocaml_files.Debug -> " -runtime-variant d"
  | Ocaml_files.Instrumented -> " -runtime-variant i"

let target_flags = function
  | Ocaml_backends.Native | Ocaml_backends.Bytecode -> ""
  | Ocaml_backends.Jsir_js -> "-target js_of_ocaml"
  | Ocaml_backends.Jsir_wasm -> "-target wasm_of_ocaml"

let runtime_flags env backend c_files =
  let runtime_library_flags = "-I " ^
    Ocaml_directories.runtime in
  let rt_flags = match backend with
    | Ocaml_backends.Native -> runtime_variant_flags ()
    | Ocaml_backends.Jsir_js | Ocaml_backends.Jsir_wasm -> ""
    | Ocaml_backends.Bytecode ->
      begin
        if c_files then begin (* custom mode *)
          "-custom " ^ (runtime_variant_flags ())
        end else begin (* non-custom mode *)
          let use_runtime =
            Environments.lookup_as_bool Ocaml_variables.use_runtime env
          in
          if use_runtime = Some false
          then ""
          else "-use-runtime " ^ Ocaml_files.ocamlrun
        end
      end in
  rt_flags ^ " " ^ runtime_library_flags

let toplevel_default_flags =
  "-noinit -no-version -noprompt \
   -alert -unsafe_multidomain \
   -alert -do_not_spawn_domains \
   -alert -unsafe_effects"

let ocamlobjinfo_default_flags = "-null-crc"
