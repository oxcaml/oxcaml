open! Js_of_ocaml_compiler.Stdlib
module Package = Jsoo_static_partition.Package
open Cmdliner

let run split_dir entry output_dir =
  let report = Package.Split_report.read_exn split_dir in
  if not (String.equal report.backend "javascript")
  then
    failwith (Printf.sprintf "Expected a javascript split, not a %s one" report.backend);
  List.iter
    [ "strategy", report.strategy, "late"
    ; "profile", string_of_int report.profile, "3"
    ; "effects", report.effects, "disabled"
    ]
    ~f:(fun (name, actual, expected) ->
      if not (String.equal actual expected)
      then
        failwith
          (Printf.sprintf
             "Packaging requires a split with %s %s, not %s"
             name
             expected
             actual));
  let file = Package.Split_report.file report in
  Package.generated_exn
    ~backend:`Javascript
    ~report
    ~entry
    ~output_dir
    ~runtime_configuration:(fun _ -> `Null)
    ~build:(fun directory ->
      Package.write_file
        (Filename.concat directory "loader.js")
        (Package.read_file (file "runtime.js") ^ "\n" ^ Package.read_file (file "main.js"));
      List.iter report.roots ~f:(fun (root : Package.Split_report.root) ->
          match Package.asset_basenames ~backend:`Javascript root with
          | [ name ] ->
              Package.write_file
                (Filename.concat directory name)
                (Package.script_module (Package.read_file (file root.file)))
          | _ -> failwith "Unexpected root files"))

let info =
  Info.make
    ~name:"package"
    ~doc:"Package the output of js_of_ocaml split"
    ~description:
      "js_of_ocaml-package packages the output directory of $(b,js_of_ocaml split \
       --strategy late) for deployment: an entry script $(i,ENTRY), which loads the \
       eager code, and the files it loads under $(i,DIR)/$(i,IDENTITY)/, where \
       $(i,IDENTITY) is a digest of their contents. The entry installs \
       $(i,globalThis.__jsoo_imports_v2), whose $(i,load(KEY)) loads root $(i,KEY), at \
       most once, and returns a promise of its OCaml module."

let command =
  let split_dir =
    let doc = "The output directory of $(b,js_of_ocaml split)." in
    Arg.(required & pos 0 (some dir) None & info [] ~docv:"SPLIT_DIR" ~doc)
  in
  let entry =
    let doc = "Write the entry script to $(docv), next to the output directory." in
    Arg.(required & opt (some string) None & info [ "entry" ] ~docv:"ENTRY" ~doc)
  in
  let output_dir =
    let doc = "Write the files to deploy to directory $(docv), which must be empty." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"DIR" ~doc)
  in
  Cmd.v info Term.(const run $ split_dir $ entry $ output_dir)
