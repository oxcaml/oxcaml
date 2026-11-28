(* TEST
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 set OCAMLLIB = "${test_build_directory}/root";
 run;
 check-program-output;
*)

(* The files that a manifest (-I-manifest, -H-manifest) or an -H-file or
   -Hx-file option names are looked up like the files of a directory given
   to -I, -H or -Hx: one inside the library root is found in the
   tree of the selected variant when that tree has it, and what the entry
   says about the .cmx file of the unit holds for the file found there.  A
   variant that has no tree of its own is served by the tree of the variant
   with the other poll-insertion setting, when the root has that one.  The
   root is one that this program creates and that OCAMLLIB names. *)

let root = Config.standard_library

let manifest_files_root = Filename.dirname root

let base = Codegen_variant.base

let with_tree = { base with stack_checks = true }

let without_tree = { base with frame_pointers = true }

let in_tree variant file =
  Filename.concat
    (Filename.concat (Filename.basename root) "variants")
    (Filename.concat (Codegen_variant.to_string variant) file)

let in_library_root file = Filename.concat (Filename.basename root) file

let write file lines =
  let rec make_directory dir =
    if not (Sys.file_exists dir)
    then begin
      make_directory (Filename.dirname dir);
      Sys.mkdir dir 0o777
    end
  in
  let file = Filename.concat manifest_files_root file in
  make_directory (Filename.dirname file);
  Out_channel.with_open_text file (fun channel ->
    List.iter (fun line -> output_string channel (line ^ "\n")) lines)

(* Each file with the kind of entry that names it; all but the last two
   are in both trees. *)
let files =
  [ "visible.cmi", "an entry of a manifest";
    "nested.cmi", "an entry of a manifest named by a manifest";
    "hidden.cmi", "an entry of a hidden manifest";
    "hidden_x.cmi", "an entry with its .cmx of a hidden manifest";
    "single.cmi", "an -H-file option";
    "single_x.cmi", "an -Hx-file option";
    "in_h.cmi", "a file of an -H directory";
    "in_hx.cmi", "a file of an -Hx directory";
    "default_only.cmi", "a file that only the root has";
    "outside.cmi", "a file outside the root" ]

let show path =
  let prefix = Filename.concat manifest_files_root "" in
  let path =
    if String.starts_with ~prefix path
    then
      let length = String.length prefix in
      String.sub path length (String.length path - length)
    else path
  in
  String.concat "/"
    (List.map
       (fun component ->
         if String.equal component (Codegen_variant.to_string with_tree)
         then "<selected>"
         else component)
       (String.split_on_char '/' path))

(* The load path is set up as the driver does it, from the options. *)
let lookups description variant =
  Codegen_variant.apply variant;
  Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
  Printf.printf "%s:\n" description;
  List.iter
    (fun (file, entry) ->
      let path, ({ visibility; cmx_guaranteed } : Load_path.include_flags) =
        Load_path.find_normalized_with_include_flags file
      in
      Printf.printf "  %s: %s (%s%s)\n" entry (show path)
        (match visibility with Visible -> "visible" | Hidden -> "hidden")
        (if cmx_guaranteed then ", with its .cmx" else ""))
    files

let () =
  List.iter
    (fun file ->
      write (in_library_root ("lib/" ^ file)) [];
      write (in_tree with_tree ("lib/" ^ file)) [])
    [ "visible.cmi"; "nested.cmi"; "hidden.cmi"; "hidden_x.cmi"; "single.cmi";
      "single_x.cmi" ];
  List.iter
    (fun file ->
      write (in_library_root file) [];
      write (in_tree with_tree file) [])
    ["dir_h/in_h.cmi"; "dir_hx/in_hx.cmi"];
  write (in_library_root "lib/default_only.cmi") [];
  write "elsewhere/outside.cmi" [];
  write "visible.manifest"
    [ "file visible.cmi " ^ in_library_root "lib/visible.cmi";
      "file default_only.cmi " ^ in_library_root "lib/default_only.cmi";
      "file outside.cmi elsewhere/outside.cmi";
      "manifest nested nested.manifest" ];
  write "nested.manifest"
    ["file nested.cmi " ^ in_library_root "lib/nested.cmi" ^ " x"];
  write "hidden.manifest"
    [ "file hidden.cmi " ^ in_library_root "lib/hidden.cmi";
      "file hidden_x.cmi " ^ in_library_root "lib/hidden_x.cmi" ^ " x" ];
  Load_path.For_testing.set_manifest_files_root (Some manifest_files_root);
  Clflags.include_manifests := ["visible.manifest"];
  Clflags.hidden_include_manifests := ["hidden.manifest"];
  Clflags.hidden_include_files
    := [ { basename = "single.cmi";
           actual_path = in_library_root "lib/single.cmi";
           cmx_guaranteed = false
         };
         { basename = "single_x.cmi";
           actual_path = in_library_root "lib/single_x.cmi";
           cmx_guaranteed = true
         } ];
  Clflags.no_std_include := true;
  Clflags.hidden_include_dirs
    := [ { path = Filename.concat root "dir_hx"; cmx_guaranteed = true };
         { path = Filename.concat root "dir_h"; cmx_guaranteed = false } ];
  lookups "The base variant" base;
  lookups "A variant without a tree" without_tree;
  lookups "A variant with a tree" with_tree;
  lookups "The same variant, with the other poll-insertion setting"
    { with_tree with poll_insertion = not with_tree.poll_insertion };
  (* The files that the invocation wrote itself are read where it wrote
     them, whichever option names them. *)
  List.iter
    (fun unit ->
      Codegen_variant.record_product
        (Filename.concat root ("lib/" ^ unit ^ ".cmi")))
    ["visible"; "hidden_x"; "single_x"];
  lookups "A variant with a tree, when the invocation wrote three interfaces"
    with_tree
