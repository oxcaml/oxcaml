let package_roots = [ "stdlib_stable"; "base"; "core"; "parallel" ]

let uniq xs =
  let rec loop seen = function
    | [] -> List.rev seen
    | x :: rest when List.mem x seen -> loop seen rest
    | x :: rest -> loop (x :: seen) rest
  in
  loop [] xs

let initialized = ref false
let include_dirs = ref []
let dll_dirs = ref []

let existing_dir path =
  try Sys.is_directory path
  with Sys_error _ -> false

let ensure_available package_name =
  try ignore (Findlib.package_directory package_name)
  with Not_found ->
    failwith
      (Printf.sprintf
         "Missing required package %S in the current findlib configuration"
         package_name)

let ensure_initialized () =
  if not !initialized
  then (
    Findlib.init ();
    List.iter ensure_available package_roots;
    let closure =
      Findlib.package_deep_ancestors [ "byte" ] package_roots |> uniq
    in
    let dirs = List.map Findlib.package_directory closure |> uniq in
    let stdlib_dir = Findlib.ocaml_stdlib () in
    let stub_candidates =
      List.map
        (fun dir -> Filename.concat (Filename.dirname dir) "stublibs")
        dirs
      @ [ Filename.concat stdlib_dir "stublibs";
          Filename.concat (Filename.dirname stdlib_dir) "stublibs" ]
    in
    let stubs = List.filter existing_dir (uniq stub_candidates) in
    include_dirs := dirs;
    dll_dirs := stubs;
    Web_bytecode_common.set_native_include_dirs dirs;
    initialized := true)

let with_quiet_topfind f =
  let previous = !(Topfind.log) in
  Topfind.log := ignore;
  Fun.protect f ~finally:(fun () -> Topfind.log := previous)

let init_toplevel_packages () =
  ensure_initialized ();
  Dll.add_path !dll_dirs;
  List.iter Topdirs.dir_directory !include_dirs;
  Topfind.reset ();
  Topfind.add_predicates [ "byte"; "toploop" ];
  Topfind.don't_load [ "findlib" ];
  with_quiet_topfind (fun () -> Topfind.load_deeply package_roots)
