(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let[@inline never] foo k o _ = 0

let bar oc z = foo (fun x -> x) oc z

let f z = bar 0 z
