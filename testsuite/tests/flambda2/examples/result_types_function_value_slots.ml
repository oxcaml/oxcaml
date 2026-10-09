(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-closures -flambda2-function-result-types-through-value-slots";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* [make] returns a closure that captures [helper], a closure built in its
   body. [helper] is only reachable through the value slot of the returned
   closure: by default its type is dropped from the result type of [make], so
   the inlined call to it loads [n] from the environment at run time. With
   -flambda2-function-result-types-through-value-slots the type is kept, [n]
   is known to be 1 and [h] returns the constant 8. Compare
   result_types_function_value_slots_default.ml. *)

let[@inline never] make n =
  let helper = fun[@inline always] x -> x + n in
  fun[@inline always] y -> helper y * 2

let h () = (make 1) 3
