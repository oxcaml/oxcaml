(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The same program as result_types_function_value_slots.ml without
   -flambda2-function-result-types-through-value-slots: the type of [helper],
   only reachable through the value slot of the returned closure, is dropped
   from the result type of [make], and the inlined call to it loads [n] from
   the environment at run time instead of returning the constant 8. *)

let[@inline never] make n =
  let helper = fun[@inline always] x -> x + n in
  fun[@inline always] y -> helper y * 2

let h () = (make 1) 3
