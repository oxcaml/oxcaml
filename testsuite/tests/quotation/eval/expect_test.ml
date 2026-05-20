(* TEST
  readonly_files = "expect_test_dep.ml";
  flags = "-extension runtime_metaprogramming";
  setup-ocamlopt.opt-build-env;
  module = "expect_test_dep.ml";
  ocamlopt.opt;
  include eval;
  expect.opt;
*)

#syntax quotations on

(** Basics **)

Eval.eval <[ 42 ]>
[%%expect{|
- : int = 42
|}];;

Eval.eval <[ 2 + 2 ]>
[%%expect{|
- : int = 4
|}];;

(* Use of [Stdlib] *)
Eval.eval <[ List.append [1; 2] (List.tl [2; 3]) ]>
[%%expect{|
- : int list = [1; 2; 3]
|}];;

(** Importing libraries **)

#directory "ocamlopt.opt";;
#load "expect_test_dep.cmx";;
[%%expect{|
|}];;

(* We see the implementation *)
Eval.eval <[
  Expect_test_dep.to_string (Expect_test_dep.of_int 42 : Expect_test_dep.t)
]>
[%%expect{|
- : string = "420"
|}];;

(* We see the interface (at least if it's specified in the [.ml] file) *)
Eval.eval <[
  (Expect_test_dep.of_int 42 : int)
]>
[%%expect{|
File "caml_startup", line 2, characters 3-28:
Error: This expression has type "Expect_test_dep.t"
       but an expression was expected of type "int"
|}];;
