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
 }
*)

(* [Codegen_variant.in_selected_root], the function behind the resolution of
   -I directories and link inputs named by a path inside the default library
   root, on a library root that this program creates: OCAMLLIB names it, so
   it is [Config.standard_library].  The variants are named relative to the
   configure default, so that the output is the same on every
   configuration. *)

let root = Config.standard_library

let default = Codegen_variant.default

(* The variant with a tree in the root, one that has a tree but is never
   selected, and one that no tree serves. *)
let with_tree = { default with stack_checks = not default.stack_checks }

let other = { default with frame_pointers = not default.frame_pointers }

let without_tree =
  { with_tree with frame_pointers = not default.frame_pointers }

(* A variant without a tree is served by the tree of the variant with the
   other poll-insertion setting, if there is one. *)
let other_poll_insertion (variant : Codegen_variant.t) =
  { variant with poll_insertion = not variant.poll_insertion }

let variant_tree variant = "variants/" ^ Codegen_variant.to_string variant

let in_root rest = root ^ "/" ^ rest

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

let paths =
  [ root;
    root ^ "/";
    in_root "unix";
    in_root "unix/";
    in_root "unix/unix.cmxa";
    in_root "./unix//unix.cmxa";
    in_root "unix/../unix/unix.cmxa";
    root ^ "/../" ^ Filename.basename root ^ "/unix";
    in_root "only_in_default";
    in_root "only_in_default/a.cmxa";
    in_root "unix/missing.cmxa";
    in_root "a_file_in_the_tree";
    in_root "variants";
    in_root (variant_tree with_tree ^ "/unix");
    in_root (variant_tree other ^ "/unix");
    root ^ "_sibling/unix";
    "elsewhere/unix";
    "unix.cmxa";
    "" ]

(* Paths are printed relative to the root, with the variants' names
   replaced by their roles. *)
let show path =
  let replace_prefix ~prefix ~by s =
    if String.starts_with ~prefix s
    then
      let length = String.length prefix in
      Some (by ^ String.sub s length (String.length s - length))
    else None
  in
  let path = Option.value (replace_prefix ~prefix:root ~by:"<root>" path)
      ~default:path
  in
  let role component =
    if String.equal component (Codegen_variant.to_string with_tree)
    then "<selected>"
    else if String.equal component (Codegen_variant.to_string other)
    then "<other>"
    else component
  in
  String.concat "/" (List.map role (String.split_on_char '/' path))

let resolved path =
  let resolved = Codegen_variant.in_selected_root path in
  if String.equal resolved path then None else Some resolved

let none_changes description =
  let changed = List.filter_map resolved paths in
  Printf.printf "%s: %d of %d paths change\n" description
    (List.length changed) (List.length paths)

let () =
  List.iter touch
    [ in_root "unix/unix.cmxa";
      in_root "only_in_default/a.cmxa";
      in_root (variant_tree with_tree ^ "/unix/unix.cmxa");
      (* A directory of the root where the variant's tree has a regular
         file: only a directory stands for a directory. *)
      in_root "a_file_in_the_tree/a.cmxa";
      in_root (variant_tree with_tree ^ "/a_file_in_the_tree");
      (* Never where a variant's tree is, in an installation; here to show
         that a path below variants/ is not looked up again. *)
      in_root
        (variant_tree with_tree ^ "/" ^ variant_tree other
         ^ "/unix/unix.cmxa");
      in_root (variant_tree other ^ "/unix/unix.cmxa");
      root ^ "_sibling/unix/unix.cmxa";
      root ^ "_sibling/" ^ variant_tree with_tree ^ "/unix/unix.cmxa" ];
  Codegen_variant.apply default;
  none_changes "The default variant";
  Codegen_variant.apply without_tree;
  none_changes "A variant without a tree";
  Codegen_variant.apply (other_poll_insertion without_tree);
  none_changes "The same variant, with the other poll-insertion setting";
  Codegen_variant.apply (other_poll_insertion default);
  none_changes "The default variant, with the other poll-insertion setting";
  Codegen_variant.apply with_tree;
  print_endline "A variant with a tree:";
  List.iter
    (fun path ->
      match resolved path with
      | None -> Printf.printf "  %S\n" (show path)
      | Some resolved ->
        Printf.printf "  %S -> %S\n" (show path) (show resolved))
    paths;
  let in_tree = List.map resolved paths in
  Codegen_variant.apply (other_poll_insertion with_tree);
  Printf.printf
    "The same variant, with the other poll-insertion setting: the same \
     paths: %b\n"
    (List.equal (Option.equal String.equal) in_tree (List.map resolved paths));
  Codegen_variant.apply with_tree;
  (* The linker compares paths as strings to tell whether two of its inputs
     are the same file, so a resolved path has to be spelled like the one a
     search of the variant's tree gives. *)
  Printf.printf "Spelled like a file found in the variant's root: %b\n"
    (String.equal
       (Codegen_variant.in_selected_root (in_root "./unix//unix.cmxa"))
       (Filename.concat
          (Codegen_variant.effective_standard_library ())
          "unix/unix.cmxa"));
  Clflags.set_target Js_of_ocaml;
  none_changes "The same variant, compiling to JavaScript"
