(* TEST
 setup-simple-build-env;
 set child = "${test_source_directory}/promote-missing-reference-child.tsl";
 output = "${test_build_directory}/promote-missing-reference.result";
 script = "sh ${test_source_directory}/promote-missing-reference.sh";
 script;
 check-program-output;
*)

(* This file is a driver, not a program: the work happens in
   promote-missing-reference.sh, which runs a nested ocamltest with -promote on a
   test whose reference file does not exist yet, and reports whether promotion
   created it.

   Promoting a brand-new test's output used to be impossible: check_file returns
   Unexpected_output when the reference is absent, and that branch never consulted
   the promote variable, so the reference had to be written by hand. *)
