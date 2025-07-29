open Utils

type t = int64x4

(* Creation / Destruction *)

external low_of : int64 -> t
  = "caml_vec256_unreachable" "caml_int64x4_low_of_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int64
  = "caml_vec256_unreachable" "caml_int64x4_low_to_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

external int64x4_low_int64 : t -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let () =
  let v1 = low_of 1L in
  let v2 = low_of 2L in
  let i1 = int64x4_low_int64 v1 in
  let i2 = int64x4_low_int64 v2 in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eq i1 i2 1L 2L
