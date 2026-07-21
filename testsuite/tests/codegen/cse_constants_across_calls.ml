(* TEST
 flags += " -O3";
 flags += " -cfg-cse-constants-across-calls";
 only-default-codegen;
 expect.opt;
*)

(* Tests for CSE of expensive constants across calls
   (-cfg-cse-constants-across-calls). On arm64 a symbol materialization is
   an adrp/ldr pair through the GOT; reusing the value computed before a
   call replaces each later pair with a single stack reload. The expected
   assembly blocks below are only checked on arm64 hosts; promote them
   there with [make promote-one TEST=codegen/cse_constants_across_calls.ml]. *)

let counter = ref 0

let[@inline never] callee x = x + 1

(* [counter]'s address should be materialized once, before the call, and
   reused after it: a single adrp/ldr pair for the whole function. *)
let across_one_call () =
  counter := !counter + 1;
  ignore (callee 0 : int);
  counter := !counter + 2
[%%expect_asm ARM64 {|
|}]

(* The reuse should extend across several calls in sequence. *)
let across_two_calls () =
  counter := !counter + 1;
  ignore (callee 0 : int);
  counter := !counter + 2;
  ignore (callee 0 : int);
  counter := !counter + 3
[%%expect_asm ARM64 {|
|}]
