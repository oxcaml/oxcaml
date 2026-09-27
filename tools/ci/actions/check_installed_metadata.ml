open Fl_metascanner

let require_file package path =
  if not (Sys.file_exists path) then
    failwith (Printf.sprintf "%s: missing %s" package path)

let is_library_archive path =
  List.exists (Filename.check_suffix path) [".cma"; ".cmxa"; ".cmxs"]

let package_directory ~stdlib ~parent expr =
  match Fl_metascanner.lookup "directory" [] expr.pkg_defs with
  | exception Not_found -> parent
  | "" -> parent
  | directory when String.length directory > 0 &&
      (directory.[0] = '+' || directory.[0] = '^') ->
      Filename.concat stdlib
        (String.sub directory 1 (String.length directory - 1))
  | directory when Filename.is_relative directory ->
      Filename.concat parent directory
  | directory -> directory

let normalize_path path =
  String.split_on_char '/' path
  |> List.fold_left
       (fun components -> function
         | "" | "." -> components
         | ".." -> (match components with [] -> [] | _ :: parent -> parent)
         | component -> component :: components)
       []
  |> List.rev |> String.concat "/" |> ( ^ ) "/"

let resolve_archive_path ~prefix ~owner path =
  let resolved = normalize_path path in
  if resolved <> prefix &&
     not (String.starts_with ~prefix:(prefix ^ "/") resolved)
  then failwith (Printf.sprintf "%s: archive path %S escapes %s"
    owner path prefix);
  resolved

(* Dune installs unit-less .cmxa files without a companion .a. *)
let native_archive_has_units ~objinfo path =
  let channel =
    Unix.open_process_args_in objinfo [|objinfo; path|]
  in
  let output = In_channel.input_all channel in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 ->
      String.split_on_char '\n' output
      |> List.exists (String.starts_with ~prefix:"Name:")
  | _ -> failwith (Printf.sprintf "ocamlobjinfo failed: %s" path)

let rec check_package ~prefix ~stdlib ~objinfo ~archives ~packages ~parent name expr =
  let dir = package_directory ~stdlib ~parent expr in
  let kind =
    try Fl_metascanner.lookup "library_kind" [] expr.pkg_defs
    with Not_found -> ""
  in
  List.iter
    (fun def ->
      let words = Fl_split.in_words def.def_value in
      match def.def_var with
      | "archive" | "plugin" ->
          List.iter
            (fun file ->
              let path =
                let path =
                  if Filename.is_relative file then Filename.concat dir file
                  else file
                in
                resolve_archive_path ~prefix ~owner:name path
              in
              require_file name path;
              Hashtbl.replace archives path ();
              if Filename.check_suffix path ".cmxa" then
                let companion = Filename.chop_extension path ^ ".a" in
                if not (Sys.file_exists companion) &&
                   native_archive_has_units ~objinfo path
                then require_file name companion)
            words
      | "exists_if" ->
          if not (List.exists
            (fun file -> Sys.file_exists (Filename.concat dir file)) words)
          then failwith (Printf.sprintf "%s: exists_if %S fails in %s"
            name def.def_value dir)
      | "requires" ->
          List.iter
            (fun dependency ->
              let legacy_ppx_deriver_dependency =
                kind = "ppx_deriver"
                && dependency = "ppx_deriving"
                && def.def_flav = `Appendix
                && def.def_preds =
                   [`NegPred "custom_ppx"; `NegPred "ppx_driver"]
              in
              if not legacy_ppx_deriver_dependency then
                let (_ : string) =
                  Findlib.package_directory dependency
                in
                ())
            words
      | _ -> ())
    expr.pkg_defs;
  packages := (name, kind) :: !packages;
  Printf.printf "META checked: %s\n%!" name;
  List.iter
    (fun (child, expr) ->
      check_package ~prefix ~stdlib ~objinfo ~archives ~packages ~parent:dir
        (name ^ "." ^ child) expr)
    expr.pkg_children

let check_no_byte package =
  List.iter
    (fun property ->
      let value =
        try Findlib.package_property ["byte"] package property
        with Not_found -> ""
      in
      if value <> "" then
        failwith (Printf.sprintf "%s: unexpected %s(byte) = %S"
          package property value))
    ["archive"; "plugin"]

type dune_sexp = Atom of string | List of dune_sexp list

let rec read_dune_sexp input =
  match Scanf.bscanf input " %0c" Fun.id with
  | '(' -> Scanf.bscanf input "(" (); List (read_dune_list input)
  | '"' -> Atom (Scanf.bscanf input "%S" Fun.id)
  | ')' -> failwith "unexpected ')' in dune-package"
  | _ -> Atom (Scanf.bscanf input "%[^() \t\n\r\"]" Fun.id)

and read_dune_list input =
  if Scanf.bscanf input " %0c" Fun.id = ')' then
    (Scanf.bscanf input ")" (); [])
  else
    let item = read_dune_sexp input in
    item :: read_dune_list input

let rec read_dune_sexps input =
  Scanf.bscanf input " " ();
  if Scanf.Scanning.end_of_input input then []
  else
    let sexp = read_dune_sexp input in
    sexp :: read_dune_sexps input

let check_dune_package_paths ~prefix ~archives file =
  let dir = Filename.dirname file in
  let check_atom path =
    let resolved =
      resolve_archive_path ~prefix ~owner:file
        (if Filename.is_relative path then Filename.concat dir path
         else path)
    in
    if is_library_archive path then Hashtbl.replace archives resolved ()
  in
  let rec check_paths = function
    | Atom path -> check_atom path
    | List sexps -> List.iter check_paths sexps
  in
  let check_library = function
    | List (Atom "library" :: fields) ->
        List.iter
          (function
            | List (Atom ("archives" | "plugins" | "native_archives"
                         | "foreign_archives" | "foreign_dll_files"
                         | "jsoo_runtime" | "wasmoo_runtime") :: paths) ->
                List.iter check_paths paths
            | _ -> ())
          fields
    | _ -> ()
  in
  In_channel.with_open_text file (fun channel ->
    Scanf.Scanning.from_channel channel
    |> read_dune_sexps
    |> List.iter check_library)

let check_unreferenced_archives ~lib ~archives allowlist =
  let allowed =
    In_channel.with_open_text allowlist (fun channel ->
      In_channel.input_lines channel
      |> List.map (fun line ->
        Scanf.sscanf line "%s # %[^\n]" (fun path _reason ->
          Filename.concat lib path)))
  in
  let allowed_paths = Hashtbl.create 4 in
  List.iter (fun path ->
    require_file "allowlisted archive" path;
    if Hashtbl.mem archives path then
      failwith (Printf.sprintf "Archive is now owned by metadata: %s" path);
    Hashtbl.replace allowed_paths path ()) allowed;
  let rec check_dir dir =
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.iter (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then check_dir path
      else if is_library_archive path &&
              not (Hashtbl.mem archives path || Hashtbl.mem allowed_paths path)
      then failwith (Printf.sprintf "Unreferenced installed archive: %s" path))
  in
  check_dir lib

let () =
  let stdlib = Sys.argv.(1) in
  let lib = Filename.dirname stdlib in
  let prefix = Filename.dirname lib in
  let objinfo = Filename.concat prefix "bin/ocamlobjinfo" in
  Findlib.init_manually ~stdlib ~search_path:[lib; stdlib]
    ~install_dir:stdlib ~meta_dir:"" ();
  (* Findlib's package listing hides packages with a failing exists_if. *)
  let archives = Hashtbl.create 128 in
  let packages = ref [] in
  List.iter
    (fun root ->
      Sys.readdir root |> Array.to_list |> List.sort String.compare
      |> List.iter (fun package ->
        let parent = Filename.concat root package in
        let meta = Filename.concat parent "META" in
        if Sys.file_exists meta then
          In_channel.with_open_text meta (fun channel ->
            check_package ~prefix ~stdlib ~objinfo ~archives ~packages
              ~parent package
              (Fl_metascanner.parse channel));
        let dune_package = Filename.concat parent "dune-package" in
        if Sys.file_exists dune_package then
          check_dune_package_paths ~prefix ~archives dune_package))
    [stdlib; lib];
  check_unreferenced_archives ~lib ~archives Sys.argv.(3);
  Out_channel.with_open_text Sys.argv.(2) (fun channel ->
    List.iter
      (fun (name, kind) -> Printf.fprintf channel "%s\t%s\n" name kind)
      (List.rev !packages));
  List.iter check_no_byte ["ocaml-jit"; "eval"];
  (* -uses-metaprogramming would mask a missing dependency in the link test. *)
  let dependencies = Findlib.package_deep_ancestors ["native"] ["eval"] in
  if not (List.mem "ocaml-jit" dependencies) then
    failwith "eval: missing ocaml-jit dependency"
