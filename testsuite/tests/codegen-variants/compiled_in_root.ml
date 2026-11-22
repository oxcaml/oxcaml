(* TEST
 script = "sh ${test_source_directory}/compiled_in_root.sh \
   ${ocamlsrcdir}/stdlib ${codegen_variant}";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.opt-build-env;
   script;
   set OCAMLLIB = "${test_build_directory}/root";
   all_modules = "${test_build_directory}/root/compiled_in_root.ml";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
   flags = "-a";
   program = "compiled_in_root.cmxa";
   ocamlopt.opt;
   flags = "-linkall";
   all_modules = "compiled_in_root.cmxa";
   program = "${test_build_directory}/from_archive.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.opt-build-env;
   script;
   set OCAMLLIB = "${test_build_directory}/root";
   all_modules = "${test_build_directory}/root/compiled_in_root.ml";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
   flags = "-a";
   program = "compiled_in_root.cmxa";
   ocamlopt.opt;
   flags = "-linkall";
   all_modules = "compiled_in_root.cmxa";
   program = "${test_build_directory}/from_archive.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }
*)

(* The object that ocamlopt writes for a source is the one that the same
   invocation then links or archives, also when the source is inside the
   default library root (here a root that compiled_in_root.sh makes and
   OCAMLLIB names) and the tree of the selected variant has a file of the
   same name: a path inside the root is looked up in that tree when it
   names a file of the installation, not when the compiler has just
   written the file.  This source is compiled and linked where it is in
   the root, and then compiled into an archive that a second program is
   linked from. *)

let () = print_endline "Compiled by this invocation"
