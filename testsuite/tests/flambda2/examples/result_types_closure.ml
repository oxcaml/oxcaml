(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-static-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* [make_adder] is not inlined, so without a result type the closure it returns
   is unknown at the call site and [add3 4] is an indirect call. With result
   types computed for functions returning closures whose environment only
   refers to their parameters, the call site knows the code of the returned
   closure and the value of its environment: [add3 4] becomes a direct call,
   which is inlined, and [f] returns the constant 7. Compare
   result_types_closure_default.ml. *)

let[@inline never] make_adder n = fun[@inline always] x -> x + n

let f () =
  let add3 = make_adder 3 in
  add3 4
