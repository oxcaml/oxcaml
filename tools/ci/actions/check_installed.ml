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

(* Check installed metadata. --bundled also checks the full Nix library set.
   The wrapper supplies a disposable cwd and bootstrap Findlib; installed
   compilers run only in subprocesses. *)

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

let check_status command = function
  | Unix.WEXITED 0 -> ()
  | status ->
      let reason = match status with
        | Unix.WEXITED code -> Printf.sprintf "exited %d" code
        | Unix.WSIGNALED signal -> "killed by " ^ Sys.signal_to_string signal
        | Unix.WSTOPPED signal -> "stopped by " ^ Sys.signal_to_string signal
      in
      fail "Command %s\n  cwd: %s\n  %s" command (Sys.getcwd ()) reason

let run program args =
  let command = Filename.quote_command program args in
  let channel = Unix.open_process_out ("exec " ^ command) in
  check_status command (Unix.close_process_out channel)

let capture program args =
  let command = Filename.quote_command program args in
  let channel = Unix.open_process_in ("exec " ^ command) in
  let text = In_channel.input_all channel in
  check_status command (Unix.close_process_in channel);
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
  archives

let check_archive_ownership t archives =
  checking "native archive ownership";
  (* The compiler links stdlib.cmxa implicitly. *)
  let implicit_stdlib = installed t "lib/ocaml/stdlib.cmxa" in
  let rec check_dir dir =
    Sys.readdir dir |> Array.to_list |> sorted |> List.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then check_dir path
      else if List.exists (Filename.check_suffix path) [".cmxa"; ".cmxs"] &&
              path <> implicit_stdlib &&
              not (Hashtbl.mem archives (Unix.realpath path))
      then fail "Unreferenced installed archive: %s" path)
  in
  check_dir (installed t "lib")

let check_native_smoke_programs t =
  let smoke package text =
    checking "native consumer of %s" package;
    write "main.ml" text;
    run t.findlib (["ocamlopt"; "-package"; package; "-linkpkg"] @
      findlib_flags package @ ["main.ml"; "-o"; "smoke.exe"]);
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

let create_dune_target name kind =
  Unix.mkdir name 0o700;
  write (Filename.concat name "main.ml") "let () = ()\n";
  let dependency = match kind with
    | Library -> Printf.sprintf "(libraries %s)" name
    | Ppx_deriver | Ppx_rewriter ->
        Printf.sprintf "(preprocess (pps %s))" name
  in
  write (Filename.concat name "dune") (Printf.sprintf
    "(executable (name main) (modes exe)\n %s\n\
     (link_flags (:standard -linkall %s)))\n"
    dependency (String.concat " " (link_flags name)));
  name ^ "/main.exe"

let archive_less_packages =
  [ "compiler-libs"; "ocaml-compiler-libs"; "ppxlib_ast" (* Umbrella roots. *)
  ; "stdlib" (* Linked implicitly. *)
  ; "threads.posix" (* Alias for threads. *)
  ; "compiler-libs.toplevel" (* Bytecode only. *)
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
          else if List.mem name archive_less_packages then false
          else fail "%s: no native archive" name
    in
    if consume then begin
      check_findlib_consumer t name kind;
      if List.mem name dune_names then
        targets := create_dune_target name kind :: !targets
    end) findlib_names;
  checking "Dune native consumers";
  run t.dune
    ("build" :: "--root" :: "." :: "--display=short" :: List.rev !targets)

let resolve_tool name =
  let path = String.split_on_char ':' (Sys.getenv "PATH") in
  match List.find_map (fun dir ->
    let file = Filename.concat dir name in
    try Unix.access file [Unix.X_OK];
        if Sys.is_directory file then None else Some (Unix.realpath file)
    with Unix.Unix_error _ -> None) path
  with
  | Some path -> path
  | None -> fail "Cannot find %s on PATH" name

let configure_environment t =
  let stdlib = installed t "lib/ocaml" in
  write "findlib.conf" (Printf.sprintf
    "path=%S\nstdlib=%S\nocamlc=%S\nocamlopt=%S\nldconf=\"ignore\"\n"
    (installed t "lib" ^ ":" ^ stdlib) stdlib
    (installed t "bin/ocamlc") (installed t "bin/ocamlopt"));
  Unix.putenv "PATH" (installed t "bin" ^ ":" ^ Sys.getenv "PATH");
  Unix.putenv "OCAMLFIND_CONF" (Filename.concat (Sys.getcwd ()) "findlib.conf");
  Unix.putenv "TMPDIR" (Sys.getcwd ());
  Unix.putenv "DUNE_CACHE" "disabled"

let main () =
  let lists_dir, prefix, bundled = match Array.to_list Sys.argv with
    | [_; "--lists-dir"; dir; prefix] -> dir, prefix, false
    | [_; "--lists-dir"; dir; prefix; "--bundled"] -> dir, prefix, true
    | _ -> fail "Usage: %s --lists-dir DIR PREFIX [--bundled]\n\
                 --bundled checks the complete Nix install's libraries."
             Sys.argv.(0)
  in
  let stdlib = Filename.concat prefix "lib/ocaml" in
  if not (Sys.file_exists stdlib && Sys.is_directory stdlib) then
    fail "No install at %s" prefix;
  let findlib = resolve_tool "ocamlfind" in
  let dune = resolve_tool "dune" in
  Printf.printf "Bootstrap ocamlfind: %s (%s)\n%!" findlib
    (capture findlib ["query"; "-format"; "%v"; "findlib"]);
  Printf.printf "Bootstrap dune: %s (%s)\n%!" dune
    (capture dune ["--root"; "."; "--version"]);
  let t = { prefix = Unix.realpath prefix; lists_dir; findlib; dune } in
  configure_environment t;
  checking "installed compiler location";
  let stdlib = capture (installed t "bin/ocamlc") ["-where"] in
  if stdlib <> installed t "lib/ocaml" then
    fail "Installed ocamlc uses %s, expected %s"
      stdlib (installed t "lib/ocaml");
  write "dune-project" "(lang dune 1.0)\n";
  let archives = check_meta_files t in
  check_archive_ownership t archives;
  checking "Dune library availability";
  let unavailable =
    capture dune ["installed-libraries"; "--root"; "."; "--na"]
  in
  if unavailable <> "" then fail "Unavailable Dune libraries:\n%s" unavailable;
  check_native_smoke_programs t;
  if bundled then check_bundled_libraries t

let () =
  try main () with
  | Failure message | Sys_error message -> prerr_endline message; exit 1
  | Unix.Unix_error (error, operation, argument) ->
      Printf.eprintf "%s(%S): %s\n"
        operation argument (Unix.error_message error);
      exit 1
