open Utils

type t = int32x8

(* Creation / Destruction *)

external low_of : int32 -> t
  = "caml_vec256_unreachable" "caml_int32x8_low_of_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int32
  = "caml_vec256_unreachable" "caml_int32x8_low_to_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

external int32x8_low_int64 : t -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let () =
  let v1 = low_of 1l in
  let v2 = low_of 2l in
  let i1 = int32x8_low_int64 v1 |> Int64.logand 0xffffffffL in
  let i2 = int32x8_low_int64 v2 |> Int64.logand 0xffffffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eql i1 i2 1l 2l
