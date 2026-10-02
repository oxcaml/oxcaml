open Stdlib

(* An invalid embedded-rounding immediate is a compile-time error. *)

external add_round_ps :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_add_round_ps"
[@@noalloc] [@@builtin]

external low_of : int64 -> int64x8 = "" "caml_int64x8_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external reint : 'a -> 'b = "%identity"

let _ =
  let v : float32x16 = reint (low_of 1L) in
  add_round_ps 5 v v
