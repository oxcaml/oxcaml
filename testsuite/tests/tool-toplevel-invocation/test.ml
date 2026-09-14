(* TEST
 readonly_files = "first_arg_fail.txt last_arg_fail.txt";
 setup-ocaml-build-env;
 {
   set test_case = "";
   ocaml_exit_status = "2";
   split [
   | test_case = "first_arg_fail";
   | test_case = "indirect_first_arg_fail";
   | test_case = "indirect_last_arg_fail";
   | test_case = "last_arg_fail";
   | test_case = "working_arg";
     ocaml_exit_status = "0";
   ]
   flags = "-args ${test_source_directory}/${test_case}.txt";
   compiler_reference = "${test_source_directory}/${test_case}.txt.reference";
   compiler_output = "${test_build_directory}/${test_case}.output";
   ocaml;
   check-ocaml-output;
 }{
   flags = "${test_source_directory}/print_args.ml foo bar";
   compiler_reference = "${test_source_directory}/print_args.reference";
   compiler_output = "${test_build_directory}/print_args.output";
   ocaml;
   check-ocaml-output;
 }
*)

printf "Test succeeds\n";;
