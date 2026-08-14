(* TEST
 setup-simple-build-env;
 set child = "${test_source_directory}/skip-bytecode-compilers-child.tsl";
 output = "${test_build_directory}/skip-bytecode-compilers.result";
 script = "sh ${test_source_directory}/skip-bytecode-compilers.sh";
 script;
 check-program-output;
*)

(* A driver, not a program: skip-bytecode-compilers.sh runs a nested ocamltest on
   a child test that uses the ocamlc.byte and ocamlopt.byte actions, with
   OCAMLTEST_SKIP_BYTECODE_COMPILERS set, and reports how those actions were
   classified.

   The dev harness (`make dev-test`) points ocamlc.byte at the boot main.bc, which
   carries the host runtime's magic and so cannot be run by the in-tree ocamlrun.
   Those tests then present as ordinary failures, and every session re-does the
   forensics from a magic-number error buried in a log. With the variable set they
   are skipped instead, and the skip names the compiler that went untested. *)
