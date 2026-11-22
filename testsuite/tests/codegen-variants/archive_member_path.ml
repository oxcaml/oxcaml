(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 flags = "-a ${ocamlsrcdir}/stdlib/std_exit.cmx";
 all_modules = "";
 program = "${test_build_directory}/archive.cmxa";
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh base";
   script;
   check-program-output;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh fp";
   script;
   check-program-output;
 }{
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh sc";
   script;
   check-program-output;
 }{
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh poll";
   script;
   check-program-output;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh fp-sc";
   script;
   check-program-output;
 }{
   codegen_variant = "fp-poll";
   variant_available;
   compiler_directory_suffix = ".fp-poll";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh fp-poll";
   script;
   check-program-output;
 }{
   codegen_variant = "poll-sc";
   variant_available;
   compiler_directory_suffix = ".poll-sc";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh poll-sc";
   script;
   check-program-output;
 }{
   codegen_variant = "fp-poll-sc";
   variant_available;
   compiler_directory_suffix = ".fp-poll-sc";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh fp-poll-sc";
   script;
   check-program-output;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh asan-fp";
   script;
   check-program-output;
 }{
   codegen_variant = "asan-fp-poll";
   variant_available;
   compiler_directory_suffix = ".asan-fp-poll";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   script = "sh ${test_source_directory}/archive_member_path.sh asan-fp-poll";
   script;
   check-program-output;
 }
*)

(* A unit given to ocamlopt -a by a path inside the default library root
   (here the root OCAMLLIB names) is taken from the tree of the variant
   being compiled, like an input of a link: the default tree's std_exit.cmx
   would be refused under every variant with a fingerprint of its own. It
   would be accepted under a variant that differs from the default one in
   poll insertion alone, so the script looks at what the archive got: the
   unit of the variant's tree, with its fingerprint and with polls or
   without. *)
