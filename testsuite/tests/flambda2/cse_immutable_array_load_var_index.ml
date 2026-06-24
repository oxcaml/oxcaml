(* TEST
 include stdlib_stable;
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* As [cse_immutable_array_load.ml], but with a non-constant index: the two
   [Iarray.unsafe_get] calls read the same element [arr.(i)], so CSE should
   replace the second load by the result of the first. *)

open Stdlib_stable

let f (arr : int Iarray.t) i =
  let x = Iarray.unsafe_get arr i in
  let y = Iarray.unsafe_get arr i in
  x = y
