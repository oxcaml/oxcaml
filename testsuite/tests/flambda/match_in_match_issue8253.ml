(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O2 -flambda2-match-in-match";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Regression test for issue 8253: specializing the continuation of the
   second [if] on the switch on [w] used to fail when replaying history. *)

let f (w : bool) (y : bool) (x : int) =
  let b = (w && y) = (x = 0) in
  if w then b else not b
