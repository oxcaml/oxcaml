(* TEST
  subdirectories = "manifests";
  include ocamlcommon;
  expect;
*)


let run_test ?(include_manifests = []) ?(hidden_include_manifests = []) ?(files_to_check = []) () =
  let disable_interactive f =
    let old_interactive = !Sys.interactive in
    Fun.protect ~finally:(fun () -> Sys.interactive := old_interactive)
      (fun () ->
        Sys.interactive := false;
        f ())
  in
  let cwd = Sys.getcwd () in
  Clflags.include_manifests := include_manifests;
  Clflags.hidden_include_manifests := hidden_include_manifests;
  let manifest_files_root = Filename.concat (Sys.getcwd ()) "manifests" in
  Load_path.For_testing.set_manifest_files_root (Some manifest_files_root);
  Load_path.init ~auto_include:Load_path.no_auto_include ~visible:[] ~hidden:[];
  List.iter (fun basename ->
    let path_with_visibility = try
      let path, visibility = disable_interactive (fun () -> Load_path.find_normalized_with_visibility basename) in
      let path = if String.starts_with ~prefix:cwd path then (
        "$PWD" ^
          (String.sub path (String.length cwd) (String.length path - String.length cwd)))
      else path in
      let visibility = match visibility with
        | Load_path.Visible -> "visible"
        | Hidden -> "hidden"
      in
      Format.sprintf "%s (%s)" path visibility
    with Not_found -> "Not_found"
    in
    Format.printf "%s -> %s@." basename path_with_visibility) files_to_check
;;

[%%expect{|
val run_test :
  ?include_manifests:string list ->
  ?hidden_include_manifests:string list ->
  ?files_to_check:string list -> unit -> unit = <fun>
|}]

let () =
  run_test ~include_manifests:["00/00-manifest.txt"] ~files_to_check:["foo.cmi"; "bar.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> $PWD/manifests/00/foo (visible)
bar.cmi -> $PWD/manifests/01/bar (visible)
baz.cmi -> Not_found
|}]

let () =
  run_test ~include_manifests:["01/01-manifest.txt"] ~files_to_check:["foo.cmi"; "bar.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> Not_found
bar.cmi -> $PWD/manifests/01/bar (visible)
baz.cmi -> Not_found
|}]

let () =
  run_test ~include_manifests:["02/02-manifest.txt"] ~files_to_check:["foo.cmi"; "bar.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> Not_found
bar.cmi -> $PWD/manifests/01/bar (visible)
baz.cmi -> Not_found
|}]

let () =
  run_test ~include_manifests:[
    "00/00-manifest.txt";
    "01/01-manifest.txt";
    "02/02-manifest.txt";
    "03/03-manifest.txt";
  ] ~files_to_check:["foo.cmi"; "bar.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> $PWD/manifests/00/foo (visible)
bar.cmi -> $PWD/manifests/01/bar (visible)
baz.cmi -> $PWD/manifests/04-baz (visible)
|}]

let () =
  run_test ~include_manifests:[
    "00/00-manifest.txt";
  ] ~hidden_include_manifests:[
    "03/03-manifest.txt"
  ] ~files_to_check:["foo.cmi"; "bar.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> $PWD/manifests/00/foo (visible)
bar.cmi -> $PWD/manifests/01/bar (visible)
baz.cmi -> $PWD/manifests/04-baz (hidden)
|}]
