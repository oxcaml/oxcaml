open Utils

type t = int16x16

(* Creation / Destruction *)

external low_of : (int[@untagged]) -> (t[@unboxed])
  = "caml_vec256_unreachable" "caml_int16x16_low_of_int"
  [@@noalloc] [@@builtin]

external low_to : (t[@unboxed]) -> (int[@untagged])
  = "caml_vec256_unreachable" "caml_int16x16_low_to_int"
  [@@noalloc] [@@builtin]

external int16x16_low_int64 : t -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let () =
  let v1 = low_of 1 in
  let v2 = low_of 2 in
  let i1 = int16x16_low_int64 v1 |> Int64.logand 0xffffL in
  let i2 = int16x16_low_int64 v2 |> Int64.logand 0xffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eqi i1 i2 1 2
