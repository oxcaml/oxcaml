(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-simplify;
 check-fexpr-dump;
*)

(* The #(int * unit#) type needs to be correctly considered, as it unarizes to a single kind,
   even though it is an unboxed product. *)

let f (x : #(int * unit#)) =
  let #(a, _) = x in a

let[@local never] g (f : #(int * unit#) -> int) x = f x

let h x = g f x
