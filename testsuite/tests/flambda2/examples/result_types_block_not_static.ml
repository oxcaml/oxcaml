(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-static-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Under the static-closures setting a block returned by a function only gets
   a result type if its non-closure fields are the function's parameters,
   symbols or constants. Here the second field is a reference allocated in the
   body, so no result type is computed and the call through the closure stays
   indirect; compare result_types_block_of_closures.ml. *)

let[@inline never] make_ops n = (fun[@inline always] x -> x + n), ref n

let g () =
  let add, r = make_ops 2 in
  add 1 + !r
