open Stdlib

(* An out-of-range immediate for an AVX512 intrinsic is a compile-time error. *)

external cmp_ps_mask :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_cmp_ps_mask"
[@@noalloc] [@@builtin]

external low_of : int64 -> int64x8 = "" "caml_int64x8_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external reint : 'a -> 'b = "%identity"

let _ =
  let v : float32x16 = reint (low_of 1L) in
  cmp_ps_mask 99 v v
