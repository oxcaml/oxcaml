(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The same program as result_types_closure.ml with the default setting, under
   which result types are only computed for functors: [add3 4] stays an
   indirect call to an unknown function. *)

let[@inline never] make_adder n = fun[@inline always] x -> x + n

let f () =
  let add3 = make_adder 3 in
  add3 4
