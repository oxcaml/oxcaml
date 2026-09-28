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

(* Check META paths, dependencies and native archive ownership, or inventories
   and findlib/Dune/JS/Wasm consumers. Built with bootstrap Findlib; the
   installed compiler runs only in subprocesses. *)

open Fl_metascanner

let fail fmt = Printf.ksprintf failwith fmt
let write path text =
  Out_channel.with_open_text path (fun channel -> output_string channel text)
let read path = In_channel.with_open_text path In_channel.input_all
let lines text = String.split_on_char '\n' text |> List.filter ((<>) "")
let words = Fl_split.in_words
let sorted = List.sort String.compare

let with_directory dir f =
  let previous = Sys.getcwd () in
  Unix.chdir dir;
  Fun.protect ~finally:(fun () -> Unix.chdir previous) f

type inventory = Core | Shipped
type check = Metadata | Libraries of inventory
type context = { prefix : string; source_root : string; env : string array }

let installed t path = Filename.concat t.prefix path
let source t path = Filename.concat t.source_root path
let data t file = source t ("tools/ci/actions/" ^ file)

let spawn ?(stdout = Unix.stdout) t program args =
  Unix.create_process_env program (Array.of_list (program :: args)) t.env
    Unix.stdin stdout Unix.stderr

let wait program pid =
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> fail "%s exited %d" program code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
      fail "%s stopped by signal %d" program signal

let run t program args = wait program (spawn t program args)

let capture t program args =
  let input, output = Unix.pipe ~cloexec:true () in
  let pid = spawn ~stdout:output t program args in
  Unix.close output;
  let channel = Unix.in_channel_of_descr input in
  let text = In_channel.input_all channel in
  close_in channel;
  wait program pid;
  String.trim text

(* Use ocamlfind rather than the in-process Findlib API: version 1.9.8 caches
   missing dependencies across predicate sets, with no public reset. *)
let query t args package = capture t "ocamlfind" ("query" :: args @ [package])

type kind = Library | Ppx_deriver | Ppx_rewriter

let kind = function
  | "ppx_deriver" -> Ppx_deriver
  | "ppx_rewriter" -> Ppx_rewriter
  | _ -> Library

let predicates = function
  | Library -> "native"
  | Ppx_deriver | Ppx_rewriter -> "native,ppx_driver"

let link_flags = function
  | "eval" -> ["-extension"; "runtime_metaprogramming"; "-uses-metaprogramming"]
  | _ -> []

let findlib_flags package =
  List.concat_map (fun flag -> ["-passopt"; flag]) (link_flags package)

let metadata t =
  let lib = installed t "lib" in
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
  let rec check_package parent name expr =
    let property key =
      try lookup key [] expr.pkg_defs with Not_found -> ""
    in
    let dir =
      match property "directory" with
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
    let (_ : string) = query t
      ["-recursive"; "-predicates"; predicates (kind (property "library_kind"))]
      name
    in
    Printf.printf "META checked: %s\n%!" name;
    List.iter (fun (child, expr) ->
      check_package dir (name ^ "." ^ child) expr) expr.pkg_children
  in
  (* Findlib's package listing hides packages with a failing exists_if. *)
  List.iter (fun root ->
    Sys.readdir root |> Array.to_list |> sorted |> List.iter (fun package ->
      let parent = Filename.concat root package in
      let meta = Filename.concat parent "META" in
      if Sys.file_exists meta then
        In_channel.with_open_text meta (fun channel ->
          check_package parent package (parse channel)))) [stdlib; lib];
  let allowed = read (data t "installed-unreferenced-archives.txt") |> lines
    |> List.filter_map (fun line ->
      let path = String.split_on_char '#' line |> List.hd |> String.trim in
      if path = "" then None else Some (Filename.concat lib path))
  in
  let rec check_dir dir =
    Sys.readdir dir |> Array.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then check_dir path
      else if List.exists (Filename.check_suffix path) [".cmxa"; ".cmxs"] &&
              not (Hashtbl.mem archives (Unix.realpath path) ||
                   List.mem path allowed)
      then fail "Unreferenced installed archive: %s" path)
  in
  check_dir lib;
  let smoke package text =
    Printf.printf "Checking native consumer of %s\n%!" package;
    write "main.ml" text;
    run t "ocamlfind" (["ocamlopt"; "-package"; package; "-linkpkg"] @
      findlib_flags package @ ["main.ml"; "-o"; "smoke.exe"]);
    run t "./smoke.exe" []
  in
  smoke "compiler-libs.native-toplevel"
    "let () = Opttoploop.initialize_toplevel_env ()\n";
  smoke "ocaml-jit" "let () = Jit.init_top ()\n";
  (* -uses-metaprogramming would mask a missing eval-to-JIT dependency. *)
  let dependencies = query t
    ["-recursive"; "-predicates"; "native"; "-format"; "%p"] "eval" |> lines
  in
  if not (List.mem "ocaml-jit" dependencies) then
    fail "eval: missing ocaml-jit dependency";
  smoke "eval" "let () = ()\n"

let inventory_names output =
  lines output |> List.filter_map (fun line ->
    match words line with
    | name :: "(version:" :: _ -> Some name
    | _ -> None) |> sorted

let check_names tool expected actual =
  let expected = sorted expected in
  if expected <> actual then begin
    List.iter (fun name ->
      if not (List.mem name actual) then Printf.eprintf "- %s\n" name) expected;
    List.iter (fun name ->
      if not (List.mem name expected) then Printf.eprintf "+ %s\n" name) actual;
    fail "Installed %s library names differ from the expected inventory" tool
  end

let check_findlib_consumer t name kind =
  let command = match kind with
    | Ppx_deriver -> None
    | Ppx_rewriter -> Some ("preprocessor", ["-c"; "main.ml"])
    | Library -> Some ("native",
        ["-linkpkg"; "-linkall"] @ findlib_flags name @
        ["-o"; "main.exe"; "main.ml"])
  in
  match command with
  | None -> ()
  | Some (label, args) ->
      let direct = Filename.concat "direct" name in
      Unix.mkdir direct 0o700;
      with_directory direct (fun () ->
        write "main.ml" "let () = ()\n";
        Printf.printf "Checking %s with findlib (%s)\n%!" name label;
        run t "ocamlfind" (["ocamlopt"; "-package"; name] @ args))

let dune_target name kind =
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

let libraries t inventory =
  let stdlib = capture t (installed t "bin/ocamlc") ["-where"] in
  if stdlib <> installed t "lib/ocaml" then
    fail "Installed ocamlc uses %s, expected %s"
      stdlib (installed t "lib/ocaml");
  write "dune-project" "(lang dune 3.23)\n(name installed_libraries_probe)\n";
  print_endline "Checking installed findlib and Dune inventories";
  let findlib_names = capture t "ocamlfind" ["list"] |> inventory_names in
  let dune_names =
    capture t "dune" ["installed-libraries"] |> inventory_names
  in
  let names file = read (data t file) |> lines in
  let shared = names "installed-core-libraries.txt" @
    match inventory with
    | Core -> []
    | Shipped -> names "installed-shipped-libraries.txt"
  in
  let findlib_only = match inventory with
    | Core -> []
    | Shipped -> names "installed-shipped-findlib-extras.txt"
  in
  check_names "findlib" (shared @ findlib_only) findlib_names;
  check_names "Dune" shared dune_names;
  Unix.mkdir "direct" 0o700;
  let targets = List.filter_map (fun name ->
    let kind = kind (query t ["-format"; "%(library_kind)"] name) in
    let archive =
      query t ["-predicates"; predicates kind; "-format"; "%A"] name
    in
    if archive = "" && kind = Library then None else begin
      check_findlib_consumer t name kind;
      if List.mem name dune_names then Some (dune_target name kind) else None
    end) findlib_names
  in
  let targets = match inventory with
    | Core -> targets
    | Shipped ->
        print_endline "Checking installed JS/Wasm compilers";
        (* prefix/bin is first on PATH; missing tools must not fall back. *)
        List.iter (fun tool ->
          let file = installed t ("bin/" ^ tool) in
          let executable =
            try Unix.access file [Unix.X_OK]; not (Sys.is_directory file)
            with Unix.Unix_error _ -> false
          in
          if not executable then fail "Missing installed compiler: %s" file)
          ["js_of_ocaml"; "wasm_of_ocaml"];
        Unix.mkdir "jsoo" 0o700;
        List.iter (fun file -> write (Filename.concat "jsoo" file)
          (read (source t ("external/ast-dependent-libs/smoke/" ^ file))))
          ["main.ml"; "dune"];
        targets @ ["jsoo/main.bc.js"; "jsoo/main.bc.wasm.js"]
  in
  print_endline "Checking installed libraries with Dune";
  run t "dune" ("build" :: "--display=short" :: targets)

let main () =
  let source_root, check, prefix = match Array.to_list Sys.argv with
    | [_; "--source-root"; root; "metadata"; prefix] -> root, Metadata, prefix
    | [_; "--source-root"; root; "libraries"; prefix; "core"] ->
        root, Libraries Core, prefix
    | [_; "--source-root"; root; "libraries"; prefix; "shipped"] ->
        root, Libraries Shipped, prefix
    | _ -> fail "Usage: %s --source-root ROOT \
                 metadata PREFIX | libraries PREFIX core|shipped" Sys.argv.(0)
  in
  let stdlib = Filename.concat prefix "lib/ocaml" in
  if not (Sys.file_exists stdlib && Sys.is_directory stdlib) then
    fail "No install at %s" prefix;
  let prefix = Unix.realpath prefix in
  let source_root = Unix.realpath source_root in
  let work = Filename.temp_dir "check-installed-" "" |> Unix.realpath in
  let removed = ["PATH"; "TMPDIR"; "OCAMLFIND_CONF"; "DUNE_CACHE"; "OCAMLLIB";
    "CAMLLIB"; "CAML_LD_LIBRARY_PATH"; "OCAMLPATH"; "OCAMLFIND_COMMANDS";
    "OCAMLFIND_TOOLCHAIN"; "OPAM_SWITCH_PREFIX"; "OPAMROOT"]
  in
  let inherited = Unix.environment () |> Array.to_list
    |> List.filter (fun entry ->
      not (List.exists (fun key -> String.starts_with ~prefix:(key ^ "=") entry)
        removed))
  in
  let env = Array.of_list
    (["PATH=" ^ Filename.concat prefix "bin" ^ ":" ^ Sys.getenv "PATH";
      "OCAMLFIND_CONF=" ^ Filename.concat work "findlib.conf";
      "TMPDIR=" ^ work; "DUNE_CACHE=disabled"] @
     (* Redirect compilers in staged trees that still embed the final prefix.
        The libraries check instead verifies the built-in -where. *)
     (match check with
      | Metadata -> ["OCAMLLIB=" ^ Filename.concat prefix "lib/ocaml"]
      | Libraries _ -> []) @ inherited)
  in
  let t = { prefix; source_root; env } in
  Fun.protect ~finally:(fun () -> run t "rm" ["-rf"; work]) (fun () ->
    with_directory work (fun () ->
      write "findlib.conf" (Printf.sprintf
        "path=%S\nstdlib=%S\nocamlc=%S\nocamlopt=%S\nldconf=\"ignore\"\n"
        (installed t "lib" ^ ":" ^ installed t "lib/ocaml")
        (installed t "lib/ocaml") (installed t "bin/ocamlc")
        (installed t "bin/ocamlopt"));
      match check with
      | Metadata -> metadata t
      | Libraries inventory -> libraries t inventory))

let () =
  try main () with Failure message -> prerr_endline message; exit 1
