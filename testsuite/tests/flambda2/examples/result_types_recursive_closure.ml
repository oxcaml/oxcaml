(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The closure returned by [count] captures [count] itself, whose type inside
   the body carries the depth variable of the function under [succ]. That
   variable must be projected out of the result type like the others,
   otherwise it escapes into the type stored for [count]. *)

let[@inline never] rec count n =
  fun[@inline always] () -> if n = 0 then 0 else 1 + count (n - 1) ()

let f () = count 3 ()
