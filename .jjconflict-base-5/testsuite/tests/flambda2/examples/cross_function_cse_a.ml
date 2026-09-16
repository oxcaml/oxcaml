(* TEST
 compile_only = "true";
 setup-ocamlopt.byte-build-env;
 flags = "-Oclassic";
 ocamlopt.byte with dump-raw;
 check-fexpr-dump;
*)

(* Comes with cross_function_cse_b.ml.

  We test that two allocations in different functions which are inlined are correctly CSE'd
  together. Cross_function_cse_a is compiled in classic mode to test if resimplifying
  after the inlining has been done in classic mode works for CSE as well. 
*)

let some x = Some x
[@@inline]

let f x =
  let a = Some x in
  let b = some x in
  a, b

let g x =
  let a = some x in
  let b = Some x in
  a, b
