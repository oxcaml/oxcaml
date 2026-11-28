(* TEST
 include ocamlcommon;
 include ocamloptcomp;
 set OCAMLLIB = "${test_build_directory}/lib";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* [Asmlink.cached_generic_functions_archive] is the archive of cached
   generic functions that a link uses, and the function that the linker
   itself calls to find it: a build system that asks it which file to
   depend on is told the file that is linked. This program asks it on a
   library root of its own (OCAMLLIB names it, so it is
   [Config.standard_library]), with and without
   -cached-generic-functions-path. A path given with that option is
   redirected like any other path inside the root: the archive of the root,
   named explicitly, is the one of the selected variant's tree where there
   is one. *)

let root = Config.standard_library

let base = Codegen_variant.base

let with_tree = { base with stack_checks = true }

let without_tree = { base with frame_pointers = true }

let archive = "cached-generic-functions" ^ Config.ext_lib

let tree variant = root ^ "/variants/" ^ Codegen_variant.to_string variant

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

(* Paths are printed relative to the root, with the variants' names
   replaced by their roles. *)
let show path =
  let path =
    if String.starts_with ~prefix:root path
    then
      let length = String.length root in
      "<root>" ^ String.sub path length (String.length path - length)
    else path
  in
  let role component =
    if String.equal component (Codegen_variant.to_string with_tree)
    then "<with a tree>"
    else if String.equal component (Codegen_variant.to_string without_tree)
    then "<without a tree>"
    else component
  in
  String.concat "/" (List.map role (String.split_on_char '/' path))

let query description named =
  Oxcaml_flags.cached_generic_functions_path := named;
  Printf.printf "  %s: %s\n" description
    (show (Asmlink.cached_generic_functions_archive ()))

let queries description variant =
  Codegen_variant.apply variant;
  Printf.printf "%s:\n" description;
  query "by default" None;
  query "the root's, by name" (Some (root ^ "/" ^ archive));
  query "another, by name" (Some ("elsewhere/" ^ archive))

let () =
  touch (root ^ "/" ^ archive);
  touch (tree with_tree ^ "/" ^ archive);
  queries "The base variant" base;
  queries "A variant with a tree" with_tree;
  queries "A variant without a tree" without_tree
