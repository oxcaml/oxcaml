(* TEST
  subdirectories = "manifests";
  include ocamlcommon;
  expect;
*)


let run_test ?(include_manifests = []) ?(hidden_include_manifests = []) ?(hidden_include_files = []) ?(files_to_check = []) () =
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
  Clflags.hidden_include_files := hidden_include_files;
  let manifest_files_root = Filename.concat (Sys.getcwd ()) "manifests" in
  Load_path.For_testing.set_manifest_files_root (Some manifest_files_root);
  Load_path.init ~auto_include:Load_path.no_auto_include ~visible:[] ~hidden:[];
  List.iter (fun basename ->
    let path_with_visibility = try
      let path, include_flags = disable_interactive (fun () -> Load_path.find_normalized_with_include_flags basename) in
      let path = if String.starts_with ~prefix:cwd path then (
        "$PWD" ^
          (String.sub path (String.length cwd) (String.length path - String.length cwd)))
      else path in
      let visibility = match include_flags with
        | { Load_path.visibility = Visible; cmx_guaranteed = true } ->
          "visible, cmx_guaranteed"
        | { visibility = Visible; cmx_guaranteed = false } ->
          "visible"
        | { visibility = Hidden; cmx_guaranteed = true } ->
          "hidden, cmx_guaranteed"
        | { visibility = Hidden; cmx_guaranteed = false } ->
          "hidden"
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
  ?hidden_include_files:Clflags.hidden_include_file list ->
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

(* Test 'x' entries in manifests: qux.cmi is cmx_guaranteed,
   quux.cmi is not *)
let () =
  run_test ~include_manifests:[
    "04/04-manifest.txt";
  ] ~files_to_check:["qux.cmi"; "quux.cmi"] ()

[%%expect{|
qux.cmi -> $PWD/manifests/04/qux (visible, cmx_guaranteed)
quux.cmi -> $PWD/manifests/04/quux (visible)
|}]

(* You can also write 'x' entries in hidden includes manifests. *)
let () =
  run_test ~hidden_include_manifests:[
    "04/04-manifest.txt";
  ] ~files_to_check:["qux.cmi"; "qux.cmx"; "quux.cmi"] ()

[%%expect{|
qux.cmi -> $PWD/manifests/04/qux (hidden, cmx_guaranteed)
qux.cmx -> $PWD/manifests/04/qux_cmx (hidden)
quux.cmi -> $PWD/manifests/04/quux (hidden)
|}]

(* Writing 'x' on a cmx file (rather than its associated cmi) is not expected,
   but the parser does permit it. *)
let () =
  run_test ~include_manifests:[
    "04/04-manifest.txt";
  ] ~files_to_check:["unusual.cmx"] ()

[%%expect{|
unusual.cmx -> $PWD/manifests/04/unusual_cmx (visible, cmx_guaranteed)
|}]

(* Marking a manifest file as cmx_guaranteed is a parse error. *)
let () =
  run_test ~include_manifests:[
    "05/05-manifest.txt";
  ] ~files_to_check:[] ()

[%%expect{|
Exception:
Load_path.Parse_error
 "Cannot parse manifest file line: manifest cmx_guaranteed_manifest 00/bad.txt x".
|}]

(* Parsing of -H-file arguments *)
let () =
  List.iter (fun entry ->
    match Clflags.parse_hidden_include_file ~cmx_guaranteed:false entry with
    | Ok { basename; actual_path; _ } ->
      Format.printf "%S -> %S %S@." entry basename actual_path
    | Error msg -> Format.printf "%S -> Error: %s@." entry msg)
  [ "foo.cmi:00/foo";
    "foo.cmi:00/foo:extra";
    "foo.cmi 00/foo";
    "foo.cmi" ]

[%%expect{|
"foo.cmi:00/foo" -> "foo.cmi" "00/foo"
"foo.cmi:00/foo:extra" -> Error: Expected '<basename>:<actual_path>', got: foo.cmi:00/foo:extra
"foo.cmi 00/foo" -> Error: Expected '<basename>:<actual_path>', got: foo.cmi 00/foo
"foo.cmi" -> Error: Expected '<basename>:<actual_path>', got: foo.cmi
|}]

(* -H-file and -Hx-file *)
let () =
  run_test ~hidden_include_files:[
    { Clflags.basename = "foo.cmi"; actual_path = "00/foo"; cmx_guaranteed = false };
    { Clflags.basename = "qux.cmi"; actual_path = "04/qux"; cmx_guaranteed = true };
  ] ~files_to_check:["foo.cmi"; "qux.cmi"; "baz.cmi"] ()

[%%expect{|
foo.cmi -> $PWD/manifests/00/foo (hidden)
qux.cmi -> $PWD/manifests/04/qux (hidden, cmx_guaranteed)
baz.cmi -> Not_found
|}]

(* -H-file takes precedence over -H-manifest *)
let () =
  run_test ~hidden_include_manifests:[
    "03/03-manifest.txt";
  ] ~hidden_include_files:[
    { Clflags.basename = "baz.cmi"; actual_path = "04/quux"; cmx_guaranteed = false };
  ] ~files_to_check:["baz.cmi"] ()

[%%expect{|
baz.cmi -> $PWD/manifests/04/quux (hidden)
|}]

(* The first -H-file flag takes precedence (the list is in reverse command-line
   order) *)
let () =
  run_test ~hidden_include_files:[
    { Clflags.basename = "baz.cmi"; actual_path = "04/quux"; cmx_guaranteed = false };
    { Clflags.basename = "baz.cmi"; actual_path = "04/qux"; cmx_guaranteed = false };
  ] ~files_to_check:["baz.cmi"] ()

[%%expect{|
baz.cmi -> $PWD/manifests/04/qux (hidden)
|}]
