(* TEST
 script = "sh ${test_source_directory}/test-lists.sh ${ocamlsrcdir}/testsuite";
 stdout = "test-lists.output";
 stderr = "test-lists.output";
 script;
*)

(* The testsuite's Makefile refuses, before any test runs, a list of tests
   that is not made of directories of tests in the form tests/<directory>,
   each named once and none inside another, and it does not take a run of
   no test for a success.  It reports a run in which ocamltest failed to
   list the tests, instead of running what is left and calling that a
   success.  The list that it makes of the directories of tests leaves out
   a directory without tests, and is one that it accepts.  The log of the
   run of a list is the log of a whole run, with and without GNU parallel.
   The variables given on its command line do not reach the makes that
   tests start as variables of their own command lines, and a test that
   starts the makefile itself does not write into the log of the run that
   it is part of.  The test-variants target of the tree refuses to test no
   variant, its passes are phony targets, and it takes a pass without a
   summary for a failure. *)
