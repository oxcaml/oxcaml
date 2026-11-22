(* TEST
 script = "sh ${test_source_directory}/variant-only.sh ${ocamlsrcdir}/ocamltest/ocamltest ${ocamlsrcdir} ${test_source_directory}";
 stdout = "variant-only.output";
 stderr = "variant-only.output";
 script;
*)

(* A run for a codegen variant that is restricted to what depends on the
   variant (-codegen-variant-only) keeps the test files whose only such
   work is a command that the test writes itself and that is given the
   variant: a native compiler with its command line, ocamlmklib, the
   program of the run action.  It skips a file that only compiles
   bytecode. *)
