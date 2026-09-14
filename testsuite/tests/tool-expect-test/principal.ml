(* TEST
  setup-simple-build-env;
  run-expect;
  reference = "${test_source_directory}/principal.reference";
  check-program-output;
*)

type t1 = A
type t2 = A
[%%expect{|
type t1 = A
type t2 = A
|}]

(* -principal output doesn't match expectation *)
let x = [(A : t1); A]
[%%expect{|
val x : t1 list = [A; A]
|}]

(* -no-principal output doesn't match expectation *)
let x = [(A : t1); A]
[%%expect{|
Line 1, characters 19-20:
1 | let x = [(A : t1); A]
                       ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val x : t1 list = [A; A]
|}]

(* -principal and -no-principal have the same output *)
let x = [(A : t1); (A : t1)]
[%%expect{|
val x : t1 list = [A; A]
|}, Principal{|
val x : t1 list = [A; A]
|}]
