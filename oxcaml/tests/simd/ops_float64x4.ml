open Utils

type t = float64x4

(* Creation / Destruction *)

external low_of : float -> t
  = "caml_vec256_unreachable" "caml_float64x4_low_of_float"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> float
  = "caml_vec256_unreachable" "caml_float64x4_low_to_float"
  [@@noalloc] [@@unboxed] [@@builtin]

(* Helper functions for testing - using the infrastructure from
   utils256_cast.ml *)
external float64x4_low_int64 : t -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let () =
  let v1 = low_of 1. in
  let v2 = low_of 2. in
  let i1 = float64x4_low_int64 v1 in
  let i2 = float64x4_low_int64 v2 in
  eq i1 i2 0x3ff0000000000000L 0x4000000000000000L;
  let f1 = low_to v1 in
  let f2 = low_to v2 in
  eqf f1 f2 1. 2.
