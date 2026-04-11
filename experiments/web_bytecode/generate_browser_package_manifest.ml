let package_roots = [ "stdlib_stable"; "base"; "core" ]

let linked_packages =
  [ "findlib"
  ; "findlib.top"
  ; "compiler-libs"
  ; "compiler-libs.common"
  ; "compiler-libs.bytecomp"
  ; "compiler-libs.toplevel"
  ; "js_of_ocaml"
  ; "js_of_ocaml-toplevel"
  ]

let package_with_parents pkg =
  let parts = String.split_on_char '.' pkg in
  let rec loop prefix acc = function
    | [] -> List.rev acc
    | part :: rest ->
        let prefix =
          match prefix with
          | "" -> part
          | _ -> prefix ^ "." ^ part
        in
        loop prefix (prefix :: acc) rest
  in
  loop "" [] parts

let package_sort_key pkg =
  match List.find_index (( = ) pkg) linked_packages with
  | Some index -> 0, index
  | None ->
      (match List.find_index (( = ) pkg) package_roots with
       | Some index -> 1, index
       | None -> 2, max_int)

let usage () =
  prerr_endline
    "usage: generate_browser_package_manifest INSTALL_LIB_ROOT PACKAGE_LIB_ROOT \
     MODULE_OUTPUT MAP_OUTPUT RUNTIME_OUTPUT";
  exit 2

let uniq xs =
  let rec loop seen = function
    | [] -> List.rev seen
    | x :: rest when List.mem x seen -> loop seen rest
    | x :: rest -> loop (x :: seen) rest
  in
  loop [] xs

let uniq_by key xs =
  let rec loop seen = function
    | [] -> List.rev seen
    | x :: rest ->
        let keyed = key x in
        if List.exists (fun seen_x -> key seen_x = keyed) seen
        then loop seen rest
        else loop (x :: seen) rest
  in
  loop [] xs

let ensure_prefix ~prefix path =
  let prefix_with_sep =
    if String.length prefix > 0 && prefix.[String.length prefix - 1] = '/'
    then prefix
    else prefix ^ "/"
  in
  String.length path >= String.length prefix_with_sep
  && String.sub path 0 (String.length prefix_with_sep) = prefix_with_sep

let relative_to ~root path =
  let root =
    if String.length root > 0 && root.[String.length root - 1] = '/'
    then String.sub root 0 (String.length root - 1)
    else root
  in
  let prefix = root ^ "/" in
  if String.length path < String.length prefix
     || String.sub path 0 (String.length prefix) <> prefix
  then invalid_arg ("path is not under root: " ^ path)
  else String.sub path (String.length prefix) (String.length path - String.length prefix)

let browser_dir_for ~install_lib_root ~package_lib_root dir =
  if dir = install_lib_root || ensure_prefix ~prefix:install_lib_root dir
  then "/static/packages/install/" ^ relative_to ~root:install_lib_root dir
  else if dir = package_lib_root || ensure_prefix ~prefix:package_lib_root dir
  then "/static/packages/opam/" ^ relative_to ~root:package_lib_root dir
  else invalid_arg ("unsupported package directory root: " ^ dir)

let staged_files_for_package_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun entry ->
         Filename.check_suffix entry ".cmi"
         || Filename.check_suffix entry ".cma"
         || Filename.check_suffix entry ".cmi"
         || Filename.check_suffix entry ".cmo"
         || Filename.check_suffix entry ".js"
         || Filename.check_suffix entry ".jsa"
         || entry = "META")
  |> List.sort String.compare

let write_ml_list oc indent values =
  output_string oc "[\n";
  List.iter
    (fun value -> Printf.fprintf oc "%s  %S;\n" indent value)
    values;
  Printf.fprintf oc "%s]\n" indent

let () =
  if Array.length Sys.argv <> 6 then usage ();
  let install_lib_root = Sys.argv.(1) in
  let package_lib_root = Sys.argv.(2) in
  let module_output = Sys.argv.(3) in
  let map_output = Sys.argv.(4) in
  let runtime_output = Sys.argv.(5) in
  Findlib.init_manually
    ~stdlib:install_lib_root
    ~ldconf:""
    ~install_dir:package_lib_root
    ~meta_dir:""
    ~search_path:[ install_lib_root; package_lib_root ]
    ();
  let preload_closure = Findlib.package_deep_ancestors [ "byte" ] package_roots |> uniq in
  let staged_closure =
    Findlib.package_deep_ancestors [ "byte" ] (linked_packages @ package_roots)
    |> List.concat_map package_with_parents
    |> uniq
    |> List.sort (fun left right -> compare (package_sort_key left) (package_sort_key right))
  in
  let package_dirs =
    List.map
      (fun pkg ->
        let dir = Findlib.package_directory pkg in
        pkg, dir, browser_dir_for ~install_lib_root ~package_lib_root dir)
      staged_closure
  in
  let staged_package_dirs = uniq_by (fun (_, _, browser_dir) -> browser_dir) package_dirs in
  let runtime_files =
    staged_closure
    |> List.filter_map (fun pkg ->
      let dir = Findlib.package_directory pkg in
      let runtimes =
        try Findlib.package_property [ "byte" ] pkg "jsoo_runtime" with
        | Not_found -> ""
      in
      if runtimes = ""
      then None
      else
        Some
          (runtimes
          |> Fl_split.in_words
          |> List.map (Findlib.resolve_path ~base:dir)))
    |> List.flatten
    |> uniq
  in
  let include_dirs =
    List.filter_map
      (fun (pkg, _, browser_dir) ->
        if List.mem pkg preload_closure then Some browser_dir else None)
      package_dirs
    |> uniq
  in
  let roots = [ "/static/packages/install"; "/static/packages/opam" ] in
  let oc = open_out module_output in
  output_string oc "let browser_cmis_dir = \"/static/cmis\"\n\n";
  output_string oc "let browser_package_roots = ";
  write_ml_list oc "" roots;
  output_string oc "\nlet browser_package_include_dirs = ";
  write_ml_list oc "" include_dirs;
  output_string oc "\nlet browser_preload_packages = ";
  write_ml_list oc "" preload_closure;
  output_string oc "\nlet browser_dont_load_packages = ";
  write_ml_list oc "" linked_packages;
  close_out oc;
  let map_oc = open_out map_output in
  List.iter
    (fun (_, dir, browser_dir) ->
      List.iter
        (fun entry ->
          let src = Filename.concat dir entry in
          let dst = browser_dir ^ "/" ^ entry in
          Printf.fprintf map_oc "%s:%s\n" src dst)
        (staged_files_for_package_dir dir))
    staged_package_dirs;
  close_out map_oc;
  let runtime_oc = open_out runtime_output in
  List.iter (Printf.fprintf runtime_oc "%s\n") runtime_files;
  close_out runtime_oc
