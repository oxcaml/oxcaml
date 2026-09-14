(* TEST
 set arm = "unset";
 {
   split [
   | arm = "a";
   | arm = "b";
   | arm = "c";
   ]
   script = "sh ${test_source_directory}/split-accum.sh";
   script;
 }
 {
   script = "sh ${test_source_directory}/split-check.sh";
   script;
 }
*)

(* This file tests the "split" syntax of ocamltest. The first block should run once per
   alternative, each run appending the value of the variable "arm" to a file in the test
   build directory; the second block checks that all three alternatives actually ran, in
   order. *)
