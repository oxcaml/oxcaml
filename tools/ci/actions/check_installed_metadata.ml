open Fl_metascanner

let require_file package path =
  if not (Sys.file_exists path) then
    failwith (Printf.sprintf "%s: missing %s" package path)

let rec check_package name expr =
  let dir = Findlib.package_directory name in
  List.iter
    (fun def ->
      let words = Fl_split.in_words def.def_value in
      match def.def_var with
      | "archive" | "plugin" ->
          List.iter
            (fun file ->
              let path = Filename.concat dir file in
              require_file name path;
              if Filename.check_suffix path ".cmxa" then
                require_file name (Filename.chop_extension path ^ ".a"))
            words
      | "requires" ->
          List.iter
            (fun dependency ->
              let (_ : string) = Findlib.package_directory dependency in ())
            words
      | _ -> ())
    expr.pkg_defs;
  List.iter
    (fun mode ->
      let (_ : string list) =
        Findlib.package_deep_ancestors [mode] [name]
      in ())
    ["byte"; "native"];
  Printf.printf "META checked: %s\n%!" name;
  List.iter
    (fun (child, expr) -> check_package (name ^ "." ^ child) expr)
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

let () =
  let stdlib = Sys.argv.(1) in
  Findlib.init_manually ~stdlib ~search_path:[stdlib]
    ~install_dir:stdlib ~meta_dir:"" ();
  (* Findlib's package listing hides packages with a failing exists_if. *)
  Sys.readdir stdlib |> Array.to_list |> List.sort String.compare
  |> List.iter (fun package ->
    let meta = Filename.concat (Filename.concat stdlib package) "META" in
    if Sys.file_exists meta then
      In_channel.with_open_text meta (fun channel ->
        check_package package (Fl_metascanner.parse channel)));
  List.iter check_no_byte ["ocaml-jit"; "eval"];
  (* -uses-metaprogramming would mask a missing dependency in the link test. *)
  let dependencies = Findlib.package_deep_ancestors ["native"] ["eval"] in
  if not (List.mem "ocaml-jit" dependencies) then
    failwith "eval: missing ocaml-jit dependency"
