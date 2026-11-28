(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jacob Van Buren, Jane Street, New York                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Check compiler location, META paths/dependencies, native archive ownership,
   Dune availability, native toplevel/JIT/eval consumers, findlib and Dune
   consumers under each codegen variant and a Dune consumer that mixes the
   poll-insertion settings. --bundled adds Nix library
   inventories/consumers and checks that private dependencies stay wrapped.
   The wrapper supplies a disposable cwd, bootstrap tools and an environment
   without inherited OCaml configuration. *)

open Fl_metascanner

let fail fmt = Printf.ksprintf failwith fmt
let checking fmt =
  Printf.ksprintf (fun s -> Printf.printf "Checking %s\n%!" s) fmt
let write path text =
  Out_channel.with_open_text path (fun channel -> output_string channel text)
let read path = In_channel.with_open_text path In_channel.input_all
let lines text =
  String.split_on_char '\n' text
  |> List.map String.trim |> List.filter ((<>) "")
let words = Fl_split.in_words
let sorted = List.sort String.compare

let check_status program args status =
  let error reason =
    fail "Command %s\n  cwd: %s\n  %s"
      (Filename.quote_command program args) (Sys.getcwd ()) reason
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> error (Printf.sprintf "exited with code %d" code)
  | Unix.WSIGNALED signal -> error ("killed by " ^ Sys.signal_to_string signal)
  | Unix.WSTOPPED signal -> error ("stopped by " ^ Sys.signal_to_string signal)

let run program args =
  let pid = Unix.create_process program (Array.of_list (program :: args))
    Unix.stdin Unix.stdout Unix.stderr
  in
  check_status program args (snd (Unix.waitpid [] pid))

let capture program args =
  let channel = Unix.open_process_args_in program
    (Array.of_list (program :: args))
  in
  let text = In_channel.input_all channel in
  check_status program args (Unix.close_process_in channel);
  String.trim text

type context =
  { prefix : string; lists_dir : string; findlib : string; dune : string }

let installed t path = Filename.concat t.prefix path
let data t file = Filename.concat t.lists_dir file

(* Use ocamlfind rather than the in-process Findlib API: version 1.9.8 caches
   missing dependencies across predicate sets, with no public reset. *)
let findlib_query t args package =
  capture t.findlib ("query" :: args @ [package])

type kind = Library | Ppx_deriver | Ppx_rewriter

let library_kind_of_string package = function
  | "" | "normal" -> Library
  | "ppx_deriver" -> Ppx_deriver
  | "ppx_rewriter" -> Ppx_rewriter
  | value -> fail "%s: unknown library_kind %S" package value

let predicates = function
  | Library -> "native"
  | Ppx_deriver | Ppx_rewriter -> "native,ppx_driver"

let link_flags = function
  | "eval" -> ["-extension"; "runtime_metaprogramming"; "-uses-metaprogramming"]
  | _ -> []

let findlib_flags package =
  List.concat_map (fun flag -> ["-passopt"; flag]) (link_flags package)

let read_meta file =
  try In_channel.with_open_text file parse with
  | Failure message | Fl_metascanner.Error message -> fail "%s: %s" file message

let check_meta_files t =
  checking "META files";
  let stdlib = installed t "lib/ocaml" in
  let relative_to dir path =
    if Filename.is_relative path then Filename.concat dir path else path
  in
  let check_path owner path =
    if not (Sys.file_exists path) then fail "%s: missing %s" owner path;
    let resolved = Unix.realpath path in
    if not (String.starts_with ~prefix:(t.prefix ^ "/") resolved) then
      fail "%s: %s escapes %s" owner path t.prefix;
    resolved
  in
  let archives = Hashtbl.create 128 in
  let packages = Hashtbl.create 64 in
  let rec check_package meta parent name expr =
    checking "META package %s" name;
    (match Hashtbl.find_opt packages name with
     | None -> Hashtbl.add packages name meta
     | Some previous -> fail "Duplicate package %s in %s and %s"
                          name previous meta);
    let property key =
      try lookup key [] expr.pkg_defs with Not_found -> ""
    in
    let dir = match property "directory" with
      | "" -> parent
      | directory when directory.[0] = '+' || directory.[0] = '^' ->
          Filename.concat stdlib
            (String.sub directory 1 (String.length directory - 1))
      | directory -> relative_to parent directory
    in
    let (_ : string) = check_path name dir in
    List.iter (fun def ->
      match def.def_var with
      | "archive" | "plugin" ->
          List.iter (fun file ->
            let path = check_path name (relative_to dir file) in
            Hashtbl.replace archives path ()) (words def.def_value)
      | "exists_if" ->
          if not (List.exists
            (fun file -> Sys.file_exists (relative_to dir file))
            (words def.def_value))
          then fail "%s: exists_if %S fails in %s" name def.def_value dir
      | _ -> ()) expr.pkg_defs;
    let kind = library_kind_of_string name (property "library_kind") in
    let (_ : string) = findlib_query t
      ["-recursive"; "-predicates"; predicates kind] name
    in
    List.iter (fun (child, expr) ->
      check_package meta dir (name ^ "." ^ child) expr) expr.pkg_children
  in
  (* Findlib's package listing hides packages with a failing exists_if. *)
  List.iter (fun root ->
    Sys.readdir root |> Array.to_list |> sorted |> List.iter (fun package ->
      let parent = Filename.concat root package in
      let meta = Filename.concat parent "META" in
      if Sys.file_exists meta then
        check_package meta parent package (read_meta meta)))
    [stdlib; installed t "lib"];
  archives, Hashtbl.to_seq_keys packages |> List.of_seq |> sorted

let check_archive_ownership t archives =
  checking "native archive ownership";
  (* The compiler links stdlib.cmxa implicitly. *)
  let implicit_stdlib = installed t "lib/ocaml/stdlib.cmxa" in
  (* Codegen-variant library trees deliberately carry no META files
     (findlib and dune find the packages in the default root, and the
     compiler looks the paths they pass up in the selected variant's
     tree), so their archives are not META-referenced by design. *)
  let variants = installed t "lib/ocaml/variants" in
  let rec check_dir dir =
    Sys.readdir dir |> Array.to_list |> sorted |> List.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then
        (if path <> variants then check_dir path)
      else if List.exists (Filename.check_suffix path) [".cmxa"; ".cmxs"] &&
              path <> implicit_stdlib &&
              not (Hashtbl.mem archives (Unix.realpath path))
      then fail "Unreferenced installed archive: %s; add a META reference" path)
  in
  check_dir (installed t "lib")

(* Warning 58 as an error: a compilation that uses a unit whose .cmx file is
   not installed gets that warning, unless the unit's interface was compiled
   with -opaque, as all are when the compiler is built in the dev profile. *)
let no_missing_cmx = ["-w"; "@58"]

let check_native_smoke_programs t =
  let smoke package text =
    checking "native consumer of %s" package;
    write "main.ml" text;
    run t.findlib (["ocamlopt"; "-package"; package; "-linkpkg"] @
      no_missing_cmx @ findlib_flags package @ ["main.ml"; "-o"; "smoke.exe"]);
    run "./smoke.exe" []
  in
  smoke "compiler-libs.native-toplevel"
    "let () = Opttoploop.initialize_toplevel_env ()\n";
  smoke "ocaml-jit" "let () = Jit.init_top ()\n";
  (* -uses-metaprogramming would mask a missing eval-to-JIT dependency. *)
  checking "eval's ocaml-jit dependency";
  let dependencies = findlib_query t
    ["-recursive"; "-predicates"; "native"; "-format"; "%p"] "eval" |> lines
  in
  if not (List.mem "ocaml-jit" dependencies) then
    fail "eval: missing ocaml-jit dependency";
  smoke "eval" "let () = ()\n"

(* The installation serves each codegen variant besides its default one from
   a library tree of its own, lib/ocaml/variants/<name>, which mirrors the
   default tree: it has its own build of every native file, and every other
   file is a relative link to the default tree's file, so that it is stored
   once. JSIR artifacts and META files are in the default tree only, and each
   tree has its own runtime archives and caml/m.h. A program built for a
   variant that compiles code into itself gets that variant's build of the
   native toplevel, the JIT and eval. *)
let check_variant_trees t =
  let stdlib = installed t "lib/ocaml" in
  let variants = Filename.concat stdlib "variants" in
  let default_only file =
    file = "META" || String.ends_with ~suffix:"-js" file ||
    String.ends_with ~suffix:"-wasm" file
  in
  let own path =
    let file = Filename.basename path in
    path = "caml/m.h" ||
    List.exists (Filename.check_suffix file)
      [".cmx"; ".cmxa"; ".cmxs"; ".a"; ".o"] &&
    (* Only bytecode links read these. *)
    not (String.starts_with ~prefix:"libcamlrun" file) &&
    file <> "libthreads_stubs.a"
  in
  let rec check_dir root dir =
    Sys.readdir (Filename.concat stdlib dir) |> Array.to_list |> sorted
    |> List.iter (fun file ->
      let path = Filename.concat dir file in
      let default = Filename.concat stdlib path in
      let copy = Filename.concat root path in
      if Sys.is_directory default then
        (if default <> variants then check_dir root path)
      else if String.starts_with ~prefix:"libasmrun" file then ()
      else if default_only file then
        (if Sys.file_exists copy then
           fail "%s: only the default library tree has this file" copy)
      else match (Unix.lstat copy).st_kind with
        | exception Unix.Unix_error (ENOENT, _, _) -> fail "Missing %s" copy
        | S_REG when own path -> ()
        | S_LNK when not (own path) ->
            if not (Filename.is_relative (Unix.readlink copy)) ||
               Unix.realpath copy <> default
            then fail "%s is not a relative link to %s" copy default
        | _ ->
            fail "%s should be %s" copy
              (if own path then "a file of this variant's build"
               else "a link to " ^ default))
  in
  let axes =
    [ "asan", "-asan", "-no-asan";
      "fp", "-frame-pointers", "-no-frame-pointers";
      "poll", "-enable-poll-insertion", "-disable-poll-insertion";
      "sc", "-stack-checks", "-no-stack-checks" ]
  in
  let toplevel =
    [ "-I"; "+compiler-libs"; "-I"; "+unix"; "-I"; "+dynlink"; "unix.cmxa";
      "dynlink.cmxa"; "ocamlcommon.cmxa"; "ocamlfrontend.cmxa";
      "ocamloptcomp.cmxa"; "ocamlopttoplevel.cmxa"; "-linkall" ]
  in
  capture (installed t "bin/ocamlopt") ["-config-var"; "codegen_variants"]
  |> words
  |> List.iter (fun variant ->
    checking "library tree of codegen variant %s" variant;
    check_dir (Filename.concat variants variant) "";
    let tokens = String.split_on_char '-' variant in
    (* The headers of the tree are links to the default tree's, and the
       caml/m.h that they include has to be the tree's own. *)
    checking "C headers of codegen variant %s" variant;
    write "macros.c"
      "#include <caml/mlvalues.h>\n\
       #ifdef WITH_ADDRESS_SANITIZER\nasan\n#endif\n\
       #ifdef WITH_FRAME_POINTERS\nfp\n#endif\n\
       #ifdef STACK_CHECKS_ENABLED\nsc\n#endif\n";
    let cc =
      capture (installed t "bin/ocamlopt") ["-config-var"; "c_compiler"]
      |> words
    in
    let macros = ["asan"; "fp"; "sc"] in
    let defined =
      capture (List.hd cc)
        (List.tl cc @
         ["-E"; "-P"; "-I"; Filename.concat variants variant; "macros.c"])
      |> lines |> List.filter (fun line -> List.mem line macros)
    in
    if defined <> List.filter (fun token -> List.mem token macros) tokens then
      fail "The headers of codegen variant %s define the macros of [%s]"
        variant (String.concat " " defined);
    let flags =
      List.map (fun (token, enable, disable) ->
        if List.mem token tokens then enable else disable) axes
    in
    let smoke library arguments text =
      checking "native consumer of %s for codegen variant %s" library variant;
      write "main.ml" text;
      run (installed t "bin/ocamlopt")
        (flags @ no_missing_cmx @ arguments @ ["main.ml"; "-o"; "smoke.exe"]);
      capture "./smoke.exe" []
    in
    let host = smoke "the native toplevel" toplevel
      "let () =\n\
      \  Opttoploop.initialize_toplevel_env ();\n\
      \  print_string (Codegen_variant.to_string Opttoploop.host_variant)\n"
    in
    if host <> variant then
      fail "The native toplevel of codegen variant %s compiles for %s"
        variant host;
    let (_ : string) = smoke "ocaml-jit"
      (toplevel @ ["-I"; "+ocaml-jit"; "jit.cmxa"])
      "let () = Jit.init_top ()\n"
    in
    let (_ : string) = smoke "eval" (link_flags "eval" @ ["eval.cmxa"])
      "let () = ()\n"
    in
    ())

(* The compiler offers the codegen variants whose trees it finds in the
   installation, so a tree that the installation lost is not missed by what
   asks the compiler for its variants, as the checks here do. The list of
   the variants that were built is in the installed Makefile.config: the
   compiler must report exactly those. *)
let check_variant_list t =
  checking "list of codegen variants";
  let makefile = installed t "lib/ocaml/Makefile.config" in
  let prefix = "CODEGEN_VARIANTS=" in
  let built =
    match
      List.filter (String.starts_with ~prefix) (lines (read makefile))
    with
    | [line] ->
        let length = String.length prefix in
        sorted (words (String.sub line length (String.length line - length)))
    | _ -> fail "%s: expected one line %s" makefile prefix
  in
  let found =
    capture (installed t "bin/ocamlopt") ["-config-var"; "codegen_variants"]
    |> words |> sorted
  in
  if found <> built then
    fail "The installation was built with the codegen variants [%s], but \
          has library trees for [%s] in %s"
      (String.concat " " built) (String.concat " " found)
      (installed t "lib/ocaml/variants")

(* The symbols starting with [prefix] that a runtime archive or shared
   library defines, by their names in C. nm prints "address type name" for
   a symbol that the file defines and "U name" for one that it only refers
   to. Its listing of external symbols, -g, exists for every object format;
   an ELF shared library may have kept its dynamic symbols only, which -D
   lists, and a Mach-O one has no such table. Mach-O also puts an underscore
   in front of the names of C. *)
let defined_symbols ~macho ~prefix path =
  let listing =
    if Filename.check_suffix path ".so" && not macho then "-D" else "-g"
  in
  capture "nm" [listing; path] |> lines
  |> List.filter_map (fun line ->
    match words line with
    | [_address; kind; symbol] when kind <> "U" ->
        let symbol =
          if macho && String.starts_with ~prefix:"_" symbol
          then String.sub symbol 1 (String.length symbol - 1)
          else symbol
        in
        if String.starts_with ~prefix symbol then Some symbol else None
    | _ -> None)
  |> List.sort_uniq String.compare

(* Each library tree has the archives of the native runtime built for its
   variant and no others: a link that takes -lasmrun from a variant's tree,
   as a hand-written link of an -output-obj object may, must not find the
   runtime of another variant there. The names of the archives end in the
   variant's axes that the runtime depends on: in nothing for base, the
   variant of the library root. A name does not say what the file is: each
   runtime defines the guard symbol of the variant it was built for and no
   other (see runtime/codegen_variant_guard.c), which nm shows. *)
let check_runtime_archives t =
  let stdlib = installed t "lib/ocaml" in
  let config variable =
    capture (installed t "bin/ocamlopt") ["-config-var"; variable]
  in
  let macho = config "system" = "macosx" in
  let check tree axes =
    checking "runtime archives of %s" tree;
    let suffix = if axes = [] then "" else "_" ^ String.concat "_" axes in
    let expected =
      sorted
        [ "libasmrun" ^ suffix ^ ".a"; "libasmrund" ^ suffix ^ ".a";
          "libasmrun_pic" ^ suffix ^ ".a";
          "libasmrun_shared" ^ suffix ^ ".so" ]
    in
    let found =
      Sys.readdir tree |> Array.to_list
      |> List.filter (String.starts_with ~prefix:"libasmrun") |> sorted
    in
    if found <> expected then
      fail "%s has the native runtime archives [%s], expected [%s]"
        tree (String.concat " " found) (String.concat " " expected);
    let guards = "caml_runtime_codegen_variant_is_" in
    let guard =
      guards ^ (if axes = [] then "base" else String.concat "_" axes)
    in
    List.iter (fun file ->
      let path = Filename.concat tree file in
      let defined = defined_symbols ~macho ~prefix:guards path in
      if defined <> [guard] then
        fail "%s defines [%s]: the runtime of this tree's variant defines %s"
          path (String.concat " " defined) guard) expected
  in
  check stdlib [];
  config "codegen_variants" |> words |> List.iter (fun variant ->
    check (Filename.concat (Filename.concat stdlib "variants") variant)
      (String.split_on_char '-' variant
       |> List.filter (fun token -> List.mem token ["asan"; "fp"; "sc"])))

let read_list file =
  read file |> lines |> List.filter_map (fun line ->
    let name = String.split_on_char '#' line |> List.hd |> String.trim in
    if name = "" then None else Some name)

let inventory_names tool text =
  lines text |> List.map (fun line ->
    match words line with
    | name :: "(version:" :: (_ :: _)
      when String.ends_with ~suffix:")" line -> name
    | _ -> fail "%s: unrecognized library listing line %S" tool line)

let check_names tool files actual =
  let expected = List.concat_map read_list files |> sorted in
  let actual = sorted actual in
  let rec duplicates = function
    | a :: (b :: _ as rest) when a = b -> a :: duplicates rest
    | _ :: rest -> duplicates rest
    | [] -> []
  in
  let report label names = match List.sort_uniq String.compare names with
    | [] -> []
    | names -> [label ^ ": " ^ String.concat ", " names]
  in
  let problems =
    report "listed, not installed"
      (List.filter (fun name -> not (List.mem name actual)) expected) @
    report "installed, not listed"
      (List.filter (fun name -> not (List.mem name expected)) actual) @
    report "listed twice" (duplicates expected) @
    report "installed twice" (duplicates actual)
  in
  if problems <> [] then
    fail "%s inventory (%s):\n%s" tool (String.concat ", " files)
      (String.concat "\n" problems)

let check_findlib_consumer t name = function
  | Ppx_deriver -> () (* Dune supplies the driver needed to run derivers. *)
  | Ppx_rewriter ->
      checking "findlib preprocessor %s" name;
      run t.findlib ["ocamlopt"; "-package"; name; "-c"; "direct/main.ml"]
  | Library ->
      checking "findlib native consumer %s" name;
      run t.findlib (["ocamlopt"; "-package"; name; "-linkpkg"; "-linkall"] @
        findlib_flags name @ ["-o"; "direct/main.exe"; "direct/main.ml"])

(* A directory with an executable that consumes the package, main.exe, and
   with what ocamlobjinfo prints for the executable's unit as Dune compiled
   it, main.objinfo. *)
let create_dune_target ?(dir = ".") ?(source = "let () = ()\n") name kind =
  let dir = Filename.concat dir name in
  Unix.mkdir dir 0o700;
  write (Filename.concat dir "main.ml") source;
  let dependency = match kind with
    | Library -> Printf.sprintf "(libraries %s)" name
    | Ppx_deriver | Ppx_rewriter ->
        Printf.sprintf "(preprocess (pps %s))" name
  in
  write (Filename.concat dir "dune") (Printf.sprintf
    "(executable (name main) (modes exe)\n %s\n\
     (link_flags (:standard -linkall %s)))\n\
     (rule (with-stdout-to main.objinfo (run ocamlobjinfo %%{cmx:main})))\n"
    dependency (String.concat " " (link_flags name)));
  dir

(* The tokens of a codegen variant's name, with their OCAMLPARAM keys. *)
let codegen_axes =
  [ "asan", "asan"; "fp", "frame-pointers"; "poll", "poll-insertion";
    "sc", "stack-checks" ]

(* A consumer prints whether it is fully polled. Poll insertion is not in
   the fingerprint that the linker enforces, so only the linked program
   tells whether the libraries it was linked with have poll insertion, as
   its own units do: unless they all do, the runtime keeps the program to
   one domain and Domain.spawn fails. *)
let consumer_source =
  "[@@@alert \"-unsafe_multidomain-do_not_spawn_domains\"]\n\
   let () =\n\
  \  match Domain.spawn ignore with\n\
  \  | domain -> Domain.join domain; print_string \"fully polled\"\n\
  \  | exception Failure message -> print_string message\n"

(* Build systems select a codegen variant with OCAMLPARAM alone and go on
   naming the libraries by their paths in the default tree, the one that
   ocamlc -where reports; the compiler resolves those paths in the variant's
   tree. So under every variant that the installation serves, base (with
   an empty OCAMLPARAM) and each of the others, every package
   whose native archives that variant's tree provides has to link, through
   findlib and through Dune, into a program of that variant, which has to
   run. *)
let check_variant_consumers t packages =
  let stdlib = installed t "lib/ocaml" in
  let config variable =
    capture (installed t "bin/ocamlopt") ["-config-var"; variable]
  in
  let variants =
    ("base", [], stdlib, "") ::
    List.map (fun variant ->
      let tokens = String.split_on_char '-' variant in
      variant, tokens,
      Filename.concat (Filename.concat stdlib "variants") variant,
      String.concat "," ("_" :: List.map (fun (token, key) ->
        Printf.sprintf "%s=%d" key (Bool.to_int (List.mem token tokens)))
        codegen_axes))
      (config "codegen_variants" |> words)
  in
  Unix.mkdir "variants" 0o700;
  write "variants/main.ml" consumer_source;
  List.iter (fun (variant, tokens, root, ocamlparam) ->
    let enabled token = List.mem token tokens in
    Unix.putenv "OCAMLPARAM" ocamlparam;
    let provided package =
      let archives = findlib_query t
        ["-predicates"; "native"; "-format"; "%+A"] package |> words
      in
      archives <> [] && List.for_all (fun archive ->
        String.starts_with ~prefix:(stdlib ^ "/") archive &&
        let length = String.length stdlib + 1 in
        Sys.file_exists (Filename.concat root
          (String.sub archive length (String.length archive - length))))
        archives
    in
    let packages = List.filter provided packages in
    if packages = [] then fail "No package has native archives in %s" root;
    (* That the compilation was for the variant, whoever ran the compiler.
       The linker refuses a unit whose fingerprint is not that of the
       libraries, so the fingerprint of the unit is that of the program. It
       links units of either poll-insertion setting, so that of the unit
       says nothing about the program, nor the program's about the unit. *)
    let compiled_for_variant =
      [ Printf.sprintf "Codegen fingerprint: frame_pointers=%b, \
                        stack_checks=%b, address_sanitizer=%b"
          (enabled "fp") (enabled "sc") (enabled "asan");
        Printf.sprintf "Polled: %b" (enabled "poll") ]
    in
    let check_unit unit objinfo =
      let printed = lines objinfo in
      List.iter (fun line ->
        if not (List.mem line printed) then
          fail "%s is not compiled for codegen variant %s: ocamlobjinfo \
                does not print %S" unit variant line) compiled_for_variant
    in
    let check_program program =
      let output = capture program [] in
      if (output = "fully polled") <> enabled "poll" then
        fail "%s is not linked for codegen variant %s, whose programs are \
              %sfully polled: it printed %S" program variant
          (if enabled "poll" then "" else "not ") output
    in
    let dir = Filename.concat "variants" variant in
    Unix.mkdir dir 0o700;
    let targets = List.map (fun package ->
      checking "findlib native consumer %s for codegen variant %s"
        package variant;
      run t.findlib (["ocamlopt"; "-package"; package; "-linkpkg"; "-linkall"] @
        findlib_flags package @
        ["-o"; "variants/main.exe"; "variants/main.ml"]);
      check_unit "variants/main.cmx"
        (capture (installed t "bin/ocamlobjinfo") ["variants/main.cmx"]);
      check_program "variants/main.exe";
      create_dune_target ~dir ~source:consumer_source package Library) packages
    in
    checking "Dune native consumers for codegen variant %s" variant;
    run t.dune ("build" :: "--root" :: "." :: "--display=short" ::
      List.concat_map (fun target ->
        [target ^ "/main.exe"; target ^ "/main.objinfo"]) targets);
    List.iter (fun target ->
      let built = Filename.concat (Filename.concat "_build/default" target) in
      check_unit ("The unit that Dune compiled in " ^ target)
        (read (built "main.objinfo"));
      check_program (built "main.exe")) targets) variants;
  Unix.putenv "OCAMLPARAM" ""

(* Poll insertion is a property of each unit and not of the program, so a
   build may give its flags to some stanzas only: the units of both settings
   and the libraries of the installation link into one program, with either
   setting for the link, whatever library trees the installation has. *)
let check_poll_insertion_per_unit t =
  checking "Dune consumer with units of both poll-insertion settings";
  Unix.mkdir "poll" 0o700;
  let flags enabled =
    if enabled then "-enable-poll-insertion" else "-disable-poll-insertion"
  in
  let name enabled = if enabled then "polled" else "unpolled" in
  let stanzas enabled =
    Printf.sprintf
      "(library (name %s) (modules %s) (libraries unix)\n\
      \ (ocamlopt_flags (:standard %s)))\n\
       (executable (name linked_%s) (modules linked_%s) (modes exe)\n\
      \ (libraries polled unpolled)\n\
      \ (ocamlopt_flags (:standard %s)) (link_flags (%s)))\n"
      (name enabled) (name enabled) (flags enabled) (name enabled)
      (name enabled) (flags enabled) (flags enabled)
  in
  write "poll/dune" (stanzas true ^ stanzas false);
  write "poll/polled.ml" "let time () = Unix.gettimeofday ()\n";
  write "poll/unpolled.ml" "let pid () = Unix.getpid ()\n";
  let targets =
    List.map (fun enabled ->
      write (Printf.sprintf "poll/linked_%s.ml" (name enabled))
        "let () = assert (Polled.time () > 0. && Unpolled.pid () > 0)\n";
      Printf.sprintf "poll/linked_%s.exe" (name enabled))
      [true; false]
  in
  run t.dune ("build" :: "--root" :: "." :: "--display=short" :: targets);
  List.iter (fun target -> run (Filename.concat "_build/default" target) [])
    targets

let contains_double_underscore s =
  let rec go i =
    i + 1 < String.length s && ((s.[i] = '_' && s.[i + 1] = '_') || go (i + 1))
  in
  go 0

(* Libraries shipped only as private dependencies are wrapped, so that their
   units can't clash with a user's own copy of them. *)
let check_private_units t =
  checking "private dependency units";
  let patterns = read_list (data t "installed-private-units.txt") in
  let matches unit pattern =
    match String.ends_with ~suffix:"*" pattern with
    | true ->
        String.starts_with unit
          ~prefix:(String.sub pattern 0 (String.length pattern - 1))
    | false -> String.equal unit pattern
  in
  let stdlib = installed t "lib/ocaml" in
  let rec check_dir dir =
    Sys.readdir dir |> Array.to_list |> sorted |> List.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then (if path <> stdlib then check_dir path)
      else if Filename.check_suffix file ".cmi" then begin
        let unit = String.capitalize_ascii (Filename.chop_suffix file ".cmi") in
        if not (contains_double_underscore unit) &&
           List.exists (matches unit) patterns
        then fail "%s: top-level unit %s belongs to a private dependency; \
                   wrap its library" path unit
      end)
  in
  check_dir (installed t "lib")

let archive_less_packages =
  [ "compiler-libs"; (* Umbrella root. *)
    "stdlib"; (* Linked implicitly. *)
    "threads.posix"; (* Alias for threads. *)
    "compiler-libs.toplevel"; (* Bytecode only. *)
    (* Bytecode only, as they need compiler-libs.toplevel. *)
    "js_of_ocaml-toplevel";
    "js_of_ocaml-toplevel.common";
    "js_of_ocaml-toplevel.worker";
  ]

let check_bundled_libraries t =
  checking "bundled library inventories";
  let findlib_names = capture t.findlib ["list"] |> inventory_names "findlib" in
  let dune_names = capture t.dune ["installed-libraries"; "--root"; "."]
    |> inventory_names "Dune"
  in
  let common = data t "installed-bundled-libraries.txt" in
  let extras = data t "installed-findlib-only-libraries.txt" in
  check_names "findlib" [common; extras] findlib_names;
  check_names "Dune" [common] dune_names;
  let archive_less = archive_less_packages @ read_list extras in
  Unix.mkdir "direct" 0o700;
  write "direct/main.ml" "let () = ()\n";
  let targets = ref [] in
  List.iter (fun name ->
    checking "bundled package %s" name;
    let kind = findlib_query t ["-format"; "%(library_kind)"] name
      |> library_kind_of_string name
    in
    let consume = match kind with
      | Ppx_deriver | Ppx_rewriter -> true
      | Library ->
          let archive = findlib_query t
            ["-predicates"; "native"; "-format"; "%A"] name
          in
          if archive <> "" then true
          else if List.mem name archive_less then false
          else fail "%s: no native archive; fix META or document an exception \
                       in check_installed.ml's archive_less_packages" name
    in
    if consume then begin
      check_findlib_consumer t name kind;
      targets := (create_dune_target name kind ^ "/main.exe") :: !targets
    end) findlib_names;
  checking "Dune native consumers";
  run t.dune
    ("build" :: "--root" :: "." :: "--display=short" :: List.rev !targets)

let configure_environment t =
  let stdlib = installed t "lib/ocaml" in
  write "findlib.conf" (Printf.sprintf
    "path=%S\nstdlib=%S\nocamlc=%S\nocamlopt=%S\nldconf=\"ignore\"\n"
    (installed t "lib" ^ ":" ^ stdlib) stdlib
    (installed t "bin/ocamlc") (installed t "bin/ocamlopt"));
  let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  Unix.putenv "PATH" (installed t "bin" ^ ":" ^ path);
  Unix.putenv "OCAMLFIND_CONF" (Filename.concat (Sys.getcwd ()) "findlib.conf");
  Unix.putenv "TMPDIR" (Sys.getcwd ());
  Unix.putenv "DUNE_CACHE" "disabled";
  (* The checks do not run under the caller's settings: OCAMLPARAM can
     select another variant than the one that a check is about, and the d
     parameter of OCAMLRUNPARAM lifts the limit that consumer_source
     observes. Empty values, for want of Unix.unsetenv: the runtime only
     reads CAMLRUNPARAM when OCAMLRUNPARAM is unset. *)
  Unix.putenv "OCAMLPARAM" "";
  Unix.putenv "OCAMLRUNPARAM" ""

let main () =
  let lists_dir, findlib, dune, prefix, bundled =
    match Array.to_list Sys.argv with
    | [_; "--lists-dir"; dir; "--ocamlfind"; findlib; "--dune"; dune; prefix] ->
        dir, findlib, dune, prefix, false
    | [_; "--lists-dir"; dir; "--ocamlfind"; findlib; "--dune"; dune; prefix;
       "--bundled"] -> dir, findlib, dune, prefix, true
    | _ -> fail "Invalid checker invocation; use check-installed.sh"
  in
  let stdlib = Filename.concat prefix "lib/ocaml" in
  if not (Sys.file_exists stdlib && Sys.is_directory stdlib) then
    fail "No install at %s" prefix;
  let t = { prefix = Unix.realpath prefix; lists_dir; findlib; dune } in
  configure_environment t;
  checking "installed compiler location";
  let where = capture (installed t "bin/ocamlc") ["-where"] in
  if where <> installed t "lib/ocaml" then
    fail "Installed ocamlc uses %s, expected %s"
      where (installed t "lib/ocaml");
  write "dune-project" "(lang dune 3.0)\n";
  let archives, packages = check_meta_files t in
  check_archive_ownership t archives;
  check_variant_list t;
  (* Before the links of check_variant_trees: a link with the runtime of
     another variant fails without saying which file is wrong. *)
  check_runtime_archives t;
  check_variant_trees t;
  checking "Dune library availability";
  let unavailable =
    capture dune ["installed-libraries"; "--root"; "."; "--na"]
  in
  if unavailable <> "" then fail "Unavailable Dune libraries:\n%s" unavailable;
  check_native_smoke_programs t;
  check_variant_consumers t packages;
  check_poll_insertion_per_unit t;
  if bundled then begin
    check_bundled_libraries t;
    check_private_units t
  end

let () =
  try main () with
  | Failure message | Sys_error message -> prerr_endline message; exit 1
  | Unix.Unix_error (error, operation, argument) ->
      Printf.eprintf "%s(%S): %s\n"
        operation argument (Unix.error_message error);
      exit 1
