(* TEST
 script = "sh ${test_source_directory}/summarize.sh ${ocamlsrcdir}/testsuite/summarize.awk";
 output = "summarize.output";
 stdout = "${output}";
 stderr = "${output}";
 script;
 reference = "${test_source_directory}/summarize.reference";
 check-program-output;
*)

(* The summary of a run fails, and counts the file as an unexpected error,
   when ocamltest itself failed on a test file after it had reported that a
   test of that file passed.  It fails, and counts the file as failed, when
   the log reports a failure of a test file and then a success of the same
   file.  It fails when the run ran no test. *)
