(* TEST
  setup-simple-build-env;
  run-expect;
  reference = "${test_source_directory}/principal_trailing_code.reference";
  check-program-output;
*)

(* Trailing code with different -principal and -no-principal outputs *)
type t1 = A
type t2 = A
let x = [(A : t1); A]
