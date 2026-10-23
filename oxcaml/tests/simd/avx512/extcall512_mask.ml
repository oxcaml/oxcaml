open Stdlib
open! Stdlib_stable

(* Unboxed products containing masks ([mask#]) passed to and returned from C
   stubs. A mask crosses into C as a 64-bit integer in a GPR, so a product
   containing one follows the convention of a C struct with a [__mmask64]
   member; on the OCaml side the component must still end up in a mask
   register. *)

external box_int64 : int64_u -> int64 = "%box_int64"
external box_mask : mask# -> mask = "%box_mask"
external unbox_mask : mask -> mask# = "%unbox_mask"

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

let eqi l r = if l <> r then Printf.printf "%016Lx <> %016Lx\n" l r

let mask bits = unbox_mask (mask_of_int64 bits)
let bits (m : mask#) = int64_of_mask (box_mask m)

(* Returns: the mask in either position, via [caml_c_call] and directly. *)

external ret_mask_i64 : int64_u -> int64_u -> #(mask# * int64_u)
  = "" "extcall512_ret_mask_i64"

external ret_mask_i64_noalloc : int64_u -> int64_u -> #(mask# * int64_u)
  = "" "extcall512_ret_mask_i64"
[@@noalloc]

external ret_i64_mask : int64_u -> int64_u -> #(int64_u * mask#)
  = "" "extcall512_ret_i64_mask"

external ret_i64_mask_noalloc : int64_u -> int64_u -> #(int64_u * mask#)
  = "" "extcall512_ret_i64_mask"
[@@noalloc]

let () =
  let #(m, i) = ret_mask_i64 #0x5aL #77L in
  eqi (bits m) 0x5aL;
  eqi (box_int64 i) 77L

let () =
  let #(m, i) = ret_mask_i64_noalloc #0xa5L #78L in
  eqi (bits m) 0xa5L;
  eqi (box_int64 i) 78L

let () =
  let #(i, m) = ret_i64_mask #79L #0x0fL in
  eqi (box_int64 i) 79L;
  eqi (bits m) 0x0fL

let () =
  let #(i, m) = ret_i64_mask_noalloc #80L #0xf0L in
  eqi (box_int64 i) 80L;
  eqi (bits m) 0xf0L

(* Arguments: the mask in either position. *)

external sum_mask_i64 : #(mask# * int64_u) -> int64_u
  = "" "extcall512_sum_mask_i64"

external sum_i64_mask : #(int64_u * mask#) -> int64_u
  = "" "extcall512_sum_i64_mask"
[@@noalloc]

let () =
  (* 0x5a + 2 * 10 *)
  eqi (box_int64 (sum_mask_i64 #(mask 0x5aL, #10L))) 110L;
  (* 10 + 2 * 0x5a *)
  eqi (box_int64 (sum_i64_mask #(#10L, mask 0x5aL))) 190L

(* Spilled products, with a register left for a phantom. *)

external i64x5_mask_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> #(mask# * int64_u)
  -> int64_u
  = "" "extcall512_i64x5_mask_i64"

external i64x4_mask_pair :
  int64_u -> int64_u -> int64_u -> int64_u -> mask# -> #(int64_u * int64_u)
  -> int64_u
  = "" "extcall512_i64x4_mask_pair"
[@@noalloc]

let () =
  (* 1 + 2 * 2 + 4 * 3 + 8 * 4 + 16 * 5 + 32 * 0x5a + 64 * 7 *)
  eqi
    (box_int64 (i64x5_mask_i64 #1L #2L #3L #4L #5L #(mask 0x5aL, #7L)))
    3457L;
  (* 1 + 2 * 2 + 4 * 3 + 8 * 4 + 16 * 0x5a + 32 * 6 + 64 * 7 *)
  eqi
    (box_int64 (i64x4_mask_pair #1L #2L #3L #4L (mask 0x5aL) #(#6L, #7L)))
    2129L
