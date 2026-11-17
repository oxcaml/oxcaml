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

(* The [codegen_variant] TSL variable selects the codegen variant (absolute
   name, e.g. "base" or "fp"; see utils/codegen_variant_name.ml, copied
   into ocamltest) that native compilation and linking target.  It swaps
   the standard-library include path to the variant's library root
   (<stdlib>/variants/<name>) when the named variant is not the install's
   configure-time default, and adds the corresponding per-axis compiler
   flags to native compiler invocations (see [codegen_variant_flags];
   bytecode compilers do not accept the flags and bytecode is unaffected by
   codegen variants, but they still use the variant root so that one test
   block can mix backends). *)
let codegen_variant env =
  match Environments.safe_lookup Ocaml_variables.codegen_variant env with
  | "" -> None
  | name -> Some name

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

let codegen_variant_directory name =
  if String.equal name default_codegen_variant
  then Ocaml_directories.stdlib
  else
    Ocamltest_stdlib.Filename.make_path
      [Ocaml_directories.stdlib; "variants"; name]

let stdlib env =
  let stdlib_path =
    match codegen_variant env with
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
  | Bytecode | Jsir_js | Jsir_wasm -> stdlib env
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
   default-tree library directories of the form <srcdir>/otherlibs/<lib>.
   Under a non-default [codegen_variant] such a directory would mix the
   default tree's artifacts into a variant compilation, so resolve it
   against the variant's library root instead (the read-time analogue of
   the [stdlib] remap above) whenever the variant tree provides the
   library; directories without a variant counterpart, and every directory
   when the variant is unset or the default, are left unchanged.  The
   systhreads source directory is named "systhreads", but it installs (and
   gets variant trees) as "threads". *)
let library_directory_for_variant name dir =
  if String.equal name default_codegen_variant then dir
  else begin
    let prefix =
      Ocamltest_stdlib.Filename.make_path
        [Ocaml_directories.srcdir; "otherlibs"]
      ^ Filename.dir_sep
    in
    if String.starts_with ~prefix dir
    then begin
      let rest =
        String.sub dir (String.length prefix)
          (String.length dir - String.length prefix)
      in
      let rest =
        if String.equal rest "systhreads" then "threads" else rest
      in
      let candidate =
        Ocamltest_stdlib.Filename.make_path
          [codegen_variant_directory name; rest]
      in
      if Sys.file_exists candidate && Sys.is_directory candidate
      then candidate
      else dir
    end
    else dir
  end

let variant_library_directory env dir =
  match codegen_variant env with
  | None -> dir
  | Some name -> library_directory_for_variant name dir

(* Like [variant_library_directory], but for in-process expect/toplevel
   host binaries: a native host compiles its phrases for
   [toplevel_host_codegen_variant], so library-modifier directories must
   resolve against that variant's root too (the companion of the
   [toplevel_stdlib] remap above). *)
let toplevel_library_directory env ~(backend : Ocaml_backends.t) dir =
  match backend with
  | Bytecode | Jsir_js | Jsir_wasm -> variant_library_directory env dir
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
