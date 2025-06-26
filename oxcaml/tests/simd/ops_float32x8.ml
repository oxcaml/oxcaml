open Utils

type t = float32x8

(* Creation / Destruction *)

external low_of : float32 -> t
  = "caml_vec256_unreachable" "caml_float32x8_low_of_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> float32
  = "caml_vec256_unreachable" "caml_float32x8_low_to_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

external float32x8_low_int64 : t -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let () =
  let v1 = low_of 1.s in
  let v2 = low_of 2.s in
  let i1 = Int64.logand (float32x8_low_int64 v1) 0xffffffffL in
  let i2 = Int64.logand (float32x8_low_int64 v2) 0xffffffffL in
  eq i1 i2 0x3f800000L 0x40000000L;
  let f1 = low_to v1 in
  let f2 = low_to v2 in
  eqf32 f1 f2 1.s 2.s
