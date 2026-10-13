(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-static-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The closure returned by [g] captures [x], which [g] itself captures from
   [f]. Being available at [g]'s entry, like a parameter, [x] does not prevent
   the result of [g] from being recognised as a statically allocatable closure
   (a criterion only accepting parameters, symbols and constants would reject
   it), so result types are computed for [g] as well as for [f]. In [h], the
   call to [g] thus becomes a direct call and the closure it returns is known,
   so the call to [k] is inlined. The value of [x] itself does not reach [h],
   since result types do not relate a function's own closure to the call site:
   the inlined body projects [x] from the returned closure. *)

let[@inline never] f x =
  0, fun[@inline never] y -> 1, fun[@inline always] z -> x + y + z

let h () =
  let _, g = f 1 in
  let _, k = g 2 in
  k 3
