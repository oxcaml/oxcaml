(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-static-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* [make_ops] returns a pair of closures; result types are computed for
   functions returning immutable blocks whose fields are such closures, so the
   calls through the pair's fields become direct calls and [g] reduces to a
   constant. *)

let[@inline never] make_ops n =
  (fun[@inline always] x -> x + n), (fun[@inline always] x -> x * n)

let g () =
  let add, mul = make_ops 2 in
  add 1 + mul 5
