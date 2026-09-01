(* TEST
 compile_only = "true";
 readonly_files = "cross_function_cse_a.ml";
 setup-ocamlopt.byte-build-env;
 {
   module = "cross_function_cse_a.ml";
   flags = "-Oclassic";
   ocamlopt.byte with dump-raw;
 }{
   module = "cross_function_cse_b.ml";
   flags = "-O3";
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 }
*)
(* CR ncourant: check fexpr dump for cross_function_cse_a.ml *)

(* Comes with cross_function_cse_a.ml.

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

let f_classic x = (Cross_function_cse_a.f [@inlined]) x
let g_classic x = (Cross_function_cse_a.g [@inlined]) x

(* TEST
   module = "cross_function_cse_a.ml";
   flags = "-Oclassic";
   ocamlopt.opt with dump-raw;
   check-fexpr-dump;
   module = "cross_function_cse_b.ml";
   flags = "-O3";
   ocamlopt.opt with dump-raw, dump-simplify;
   check-fexpr-dump;
*)