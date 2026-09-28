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

(* Check META paths, dependencies and unreferenced native archives under PREFIX.
   Usage: check_metadata.exe PREFIX ALLOWLIST, with isolated OCAMLFIND_CONF. *)

open Fl_metascanner

let fail fmt = Printf.ksprintf (fun msg -> prerr_endline msg; exit 1) fmt

let is_native_archive path =
  List.exists (Filename.check_suffix path) [".cmxa"; ".cmxs"]

let relative_to dir path =
  if Filename.is_relative path then Filename.concat dir path else path

let () =
  let prefix = Unix.realpath Sys.argv.(1) in
  let lib = Filename.concat prefix "lib" in
  let stdlib = Filename.concat lib "ocaml" in
  let check_path owner path =
    if not (Sys.file_exists path) then fail "%s: missing %s" owner path;
    let resolved = Unix.realpath path in
    if not (String.starts_with ~prefix:(prefix ^ "/") resolved) then
      fail "%s: %s escapes %s" owner path prefix;
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
    List.iter
      (fun def ->
        let words = Fl_split.in_words def.def_value in
        match def.def_var with
        | "archive" | "plugin" ->
            List.iter (fun file ->
              let path = check_path name (relative_to dir file) in
              Hashtbl.replace archives path ()) words
        | "exists_if" ->
            if not (List.exists
              (fun file -> Sys.file_exists (relative_to dir file)) words)
            then fail "%s: exists_if %S fails in %s" name def.def_value dir
        | _ -> ())
      expr.pkg_defs;
    let predicates =
      match property "library_kind" with
      | "ppx_deriver" | "ppx_rewriter" -> ["ppx_driver"]
      | _ -> []
    in
    (* Findlib 1.9.8 caches missing dependencies across predicate sets.
       Use a fresh process per query; there is no public cache reset. *)
    let command = Filename.quote_command "ocamlfind"
      ["query"; "-recursive"; "-predicates";
       String.concat "," ("native" :: predicates); name]
    in
    if Sys.command (command ^ " > /dev/null") <> 0 then
      fail "%s: native dependency resolution failed" name;
    Printf.printf "META checked: %s\n%!" name;
    List.iter (fun (child, expr) ->
      check_package dir (name ^ "." ^ child) expr) expr.pkg_children
  in
  (* Findlib's package listing hides packages with a failing exists_if. *)
  List.iter (fun root ->
    Sys.readdir root |> Array.to_list |> List.sort String.compare
    |> List.iter (fun package ->
      let parent = Filename.concat root package in
      let meta = Filename.concat parent "META" in
      if Sys.file_exists meta then
        In_channel.with_open_text meta (fun channel ->
          check_package parent package (parse channel))))
    [stdlib; lib];
  let allowed =
    In_channel.with_open_text Sys.argv.(2)
      (fun channel ->
        In_channel.input_lines channel
        |> List.filter_map (fun line ->
          let path = String.split_on_char '#' line |> List.hd |> String.trim in
          if path = "" then None else Some (Filename.concat lib path)))
  in
  let rec check_dir dir =
    Sys.readdir dir |> Array.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then check_dir path
      else if is_native_archive path &&
              not (Hashtbl.mem archives (Unix.realpath path) ||
                   List.mem path allowed)
      then fail "Unreferenced installed archive: %s" path)
  in
  check_dir lib
