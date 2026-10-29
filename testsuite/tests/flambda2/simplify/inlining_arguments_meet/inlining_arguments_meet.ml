(* TEST
 flambda2;
 readonly_files = "lib.ml";
 setup-ocamlopt.byte-build-env;
 ocamlopt_flags = "-flambda2-inline-small-function-size 1 -flambda2-inline-large-function-size 10 -flambda2-inline-threshold 1";
 module = "lib.ml";
 ocamlopt.byte;
 {
   ocamlopt_flags = "-flambda2-inline-small-function-size 20 -flambda2-inline-large-function-size 20 -flambda2-inline-threshold 20 -X flambda2-combine-inlining-arguments=env";
   module = "inlining_arguments_meet.ml";
   ocamlopt.byte with dump-simplify;
   fexpr_reference_suffix = "without-meet.reference";
   check-fexpr-dump;
 }{
   ocamlopt_flags = "-flambda2-inline-small-function-size 20 -flambda2-inline-large-function-size 20 -flambda2-inline-threshold 20";
   module = "inlining_arguments_meet.ml";
   ocamlopt.byte with dump-simplify;
   fexpr_reference_suffix = "with-meet.reference";
   check-fexpr-dump;
 }
*)

(* Test for the [-X flambda2-disable-inlining-arguments-meet=1] option.
   [lib.ml] is compiled with much weaker inlining arguments than this unit;
   this unit is compiled twice with the same options, except for the [-X]
   option: without it (the control, checked against
   [*.with-meet.reference]) and with it (checked against
   [*.without-meet.reference]).

   In [h], the call to [Lib.g] is inlined in both cases, but the call to
   [Lib.f_with_code_size_between_1_and_10] inside it is only inlined when
   the meet is disabled: with the meet, the inline threshold from [Lib]
   applies inside the inlined body of [g] and prevents inlining.

   The specialised code for the inner function of [two_arg] created by
   inlining it into [p] has size 17. With the meet, [Lib]'s
   large_function_size (10) makes it be classified as
   function_body_too_large, so the call in [q] remains an apply. With the
   meet disabled, its size is less than the current small_function_size
   (20), making it be classified as small_function and inlined in [q]. *)

let h x = Lib.g x

let p = Lib.two_arg 42

let q x = p x
