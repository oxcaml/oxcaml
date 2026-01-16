(* TEST
   (*
     These are the same test steps as [expect;] (see `ocaml_tests.ml`), but
     the reference file is changed to `ignore_feedback.reference` so we can
     test cases where [expect;] changes the input program.
   *)
   setup-simple-build-env;
   run-expect;
   reference = "${test_source_directory}/ignore_feedback.reference";
   check-program-output;
*)

(* Feedback is printed *)
type t = A | B of int
let x = B 5
[%%expect{|
type t = A | B of int
val x : t = B 5
|}]

(* Feedback is not printed *)
type t = A | B of int
let x = B 5
[%%expect.ignore_feedback]

(* Warnings are not ignored *)
let x = let x = 5 in 5
[%%expect.ignore_feedback]

(* Trailing output is still captured *)
type t = A | B of int
