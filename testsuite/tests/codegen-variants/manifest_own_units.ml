(* TEST
 readonly_files = "manifest_own_units_helper.ml";
 set MANIFEST_FILES_ROOT = "/";
 flags = "-I-manifest ${test_build_directory}/own.manifest";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/manifest_own_units.sh \
     ${ocamlsrcdir}/stdlib base ${test_build_directory}";
   script;
   set OCAMLLIB = "${test_build_directory}/root";
   all_modules = "${test_build_directory}/root/./manifest_own_units_helper.ml \
     manifest_own_units.ml";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/manifest_own_units.sh \
     ${ocamlsrcdir}/stdlib fp ${test_build_directory}";
   script;
   set OCAMLLIB = "${test_build_directory}/root";
   all_modules = "${test_build_directory}/root/./manifest_own_units_helper.ml \
     manifest_own_units.ml";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }
*)

(* Two units compiled and linked by one invocation, the first of them where
   its source is, inside the default library root (a root that
   manifest_own_units.sh makes and OCAMLLIB names).  A manifest names the
   interface and the .cmx file of the first unit there, as it would name
   those of a library of the installation, and the tree of the selected
   variant has files of the same names.  The second unit has to be compiled
   against what the invocation has just written, and the program linked
   from it: a file inside the root is looked up in the variant's tree when
   it is a file of the installation, not when it belongs to a unit that the
   invocation compiles itself.  The source is given with a "." in its path,
   which the manifest does not have: it is the file that counts, not how
   its name is written. *)

let () = Printf.printf "%d\n" (Manifest_own_units_helper.answer () + 1)
