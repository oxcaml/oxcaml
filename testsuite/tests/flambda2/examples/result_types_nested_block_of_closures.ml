(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-static-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Blocks are looked into two levels deep: the pair returned by [make_ops]
   holds a closure and another pair holding a closure and a parameter, all
   statically allocatable, so result types are computed and [g] reduces to a
   constant. *)

let[@inline never] make_ops n =
  (fun[@inline always] x -> x + n), ((fun[@inline always] x -> x * n), n)

let g () =
  let add, (mul, m) = make_ops 2 in
  add 1 + mul 5 + m
