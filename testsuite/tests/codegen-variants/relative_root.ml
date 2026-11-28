(* TEST
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 {
   set OCAMLLIB = "${test_build_directory}/absolute/lib";
   run;
   check-program-output;
 }{
   set OCAMLLIB = "relative/lib";
   run;
   check-program-output;
 }{
   set OCAMLLIB = "./relative/lib";
   run;
   check-program-output;
 }{
   set OCAMLLIB = ".";
   run;
   check-program-output;
 }
*)

(* How an input of a link is found ([Compmisc.find_input]) when the library
   root, the one that OCAMLLIB names and that this program creates, is
   named in each of four ways, the last of them the current directory.
   The answers are the same for all four, with the current directory on
   the load path and without it.

   A name is looked for as it is written, as [Load_path.find] does it, and
   the file that is found is then taken from the tree of the selected
   variant when it is inside the library root.  So a bare name and a name
   with a directory are found in the variant's tree whether the search
   meets them there or, when the root is the current directory, in the
   root first; no name is searched for under a tree that the
   search goes through already.  A file that the invocation wrote is that
   file, not searched for and not mapped, however it is named: that file
   only, not another one named after the same source, and not the file
   that a path with ".." comes to if the directory before it is no
   symbolic link.  And a path is recognized as inside the root whether or
   not it is spelled as the root is. *)

let root = Config.standard_library

let base = Codegen_variant.base

let selected = { base with stack_checks = true }

let in_root file = Filename.concat root file

let in_tree file =
  Filename.concat
    (Filename.concat
       (Filename.concat root "variants")
       (Codegen_variant.to_string selected))
    file

(* A path as a file is known by: absolute, without "." components. *)
let absolute path =
  List.filter
    (fun component ->
      not (String.equal component "" || String.equal component "."))
    (String.split_on_char '/'
       (if Filename.is_relative path
        then Filename.concat (Sys.getcwd ()) path
        else path))

let same_file path other =
  List.equal String.equal (absolute path) (absolute other)

(* A spelling of [path] that [Load_path.find] does not search for. *)
let as_file path =
  if Filename.is_implicit path
  then Filename.concat Filename.current_dir_name path
  else path

(* A spelling of [path] that differs from the root's: absolute when the
   root is relative, and with a "." component when it is absolute. *)
let spelled_otherwise file =
  if Filename.is_relative root
  then Filename.concat (Sys.getcwd ()) (in_root file)
  else Filename.concat (Filename.concat root ".") file

let touch path =
  let rec make_directory dir =
    if not (Sys.file_exists dir)
    then begin
      make_directory (Filename.dirname dir);
      Sys.mkdir dir 0o777
    end
  in
  make_directory (Filename.dirname path);
  close_out (open_out path)

let describe ~file found =
  if same_file found (in_tree file)
  then "the file in the variant's tree"
  else if same_file found (in_root file)
  then "the file in the root"
  else "another file"

let lookups ~no_cwd =
  Clflags.no_cwd := no_cwd;
  Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
  let find description ~file name =
    Printf.printf "  %s: %s\n" description
      (match Compmisc.find_input name with
       | found -> describe ~file found
       | exception Not_found -> "not found")
  in
  find "a bare name" ~file:"unit.cmx" "unit.cmx";
  find "a name with a directory" ~file:"sub/unit.cmx" "sub/unit.cmx";
  find "a file" ~file:"sub/unit.cmx" (as_file (in_root "sub/unit.cmx"));
  find "a file, spelled otherwise than the root" ~file:"sub/unit.cmx"
    (spelled_otherwise "sub/unit.cmx");
  find "a file that the invocation wrote" ~file:"sub/own.cmx"
    (as_file (in_root "sub/own.cmx"));
  find "the same, spelled otherwise" ~file:"sub/own.cmx"
    (spelled_otherwise "sub/own.cmx");
  find "another file named after the same source" ~file:"sub/own.cmxa"
    (as_file (in_root "sub/own.cmxa"));
  find "a file that a written path with \"..\" may lead to"
    ~file:"sub/kept.cmx"
    (as_file (in_root "sub/kept.cmx"))

let () =
  List.iter
    (fun file ->
      touch (in_root file);
      touch (in_tree file))
    ["unit.cmx"; "sub/unit.cmx"; "sub/own.cmx"; "sub/own.cmxa"; "sub/kept.cmx"];
  Codegen_variant.apply selected;
  Codegen_variant.record_product (in_root "sub/own.cmx");
  Codegen_variant.record_product (in_root "sub/link/../kept.cmx");
  print_endline "With the current directory on the load path:";
  lookups ~no_cwd:false;
  print_endline "Without it:";
  lookups ~no_cwd:true;
  Printf.printf "A directory, spelled otherwise than the root: %s\n"
    (describe ~file:"sub"
       (Codegen_variant.in_selected_root (spelled_otherwise "sub")))
