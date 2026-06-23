(* TEST
 include stdlib_stable;
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* This checks that loads from immutable arrays are eligible for CSE: the two
   [Iarray.unsafe_get] calls below read the same element, so the second load
   should be replaced by the result of the first. *)

open Stdlib_stable

let f (arr : int Iarray.t) =
  let x = Iarray.unsafe_get arr 0 in
  let y = Iarray.unsafe_get arr 0 in
  x = y
