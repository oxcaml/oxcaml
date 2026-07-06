open Stdlib
open Stdlib_stable

[@@@ocaml.warning "-unused-module"]

(* Tests for the 512-bit vector intrinsics in [cmm_builtins]: static (scalar)
   casts, reinterpret casts, and constants. *)

(* Raw 64-bit word extraction, reused for every 512-bit vector type by
   reinterpreting through [@@unboxed]. *)
external w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external cast : int64x8 -> int64x8 = "caml_vec512_unreachable" "caml_vec512_cast"
[@@noalloc] [@@unboxed] [@@builtin]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d e f g h =
  eq (w0 v) a;
  eq (w1 v) b;
  eq (w2 v) c;
  eq (w3 v) d;
  eq (w4 v) e;
  eq (w5 v) f;
  eq (w6 v) g;
  eq (w7 v) h

(* [cast] reinterprets the bits of a 512-bit vector, so the words must be left
   unchanged. It lets us read the raw words of the other vector types below. *)
let words v = check (cast v) (w0 v) (w1 v) (w2 v) (w3 v) (w4 v) (w5 v) (w6 v)
    (w7 v)

(* Int64x8: static cast and constants *)
module Int64x8 = struct
  external low_of : int64 -> int64x8 = "caml_vec512_unreachable"
    "caml_int64x8_low_of_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : int64x8 -> int64 = "caml_vec512_unreachable"
    "caml_int64x8_low_to_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int64 -> int64x8 = "caml_vec512_unreachable"
    "caml_int64x8_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const8 :
    int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
    -> int64x8 = "caml_vec512_unreachable" "caml_int64x8_const8"
  [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] check1 i =
    let v = const1 i in
    check v i i i i i i i i;
    eq (low_to v) i;
    eq (w0 (low_of i)) i

  let () =
    check1 0L;
    check1 1L;
    check1 (-1L);
    check1 0xffffL;
    check1 Int64.min_int;
    check1 Int64.max_int;
    let v = const8 1L 2L 3L 4L 5L 6L 7L 8L in
    check v 1L 2L 3L 4L 5L 6L 7L 8L;
    eq (low_to v) 1L;
    words v
end

(* Float64x8: static cast and constants *)
module Float64x8 = struct
  external low_of : float -> int64x8 = "caml_vec512_unreachable"
    "caml_float64x8_low_of_float"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : int64x8 -> float = "caml_vec512_unreachable"
    "caml_float64x8_low_to_float"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : float -> int64x8 = "caml_vec512_unreachable"
    "caml_float64x8_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const8 :
    float -> float -> float -> float -> float -> float -> float -> float
    -> int64x8 = "caml_vec512_unreachable" "caml_float64x8_const8"
  [@@noalloc] [@@unboxed] [@@builtin]

  let bits = Int64.bits_of_float

  let () =
    let v = const1 1. in
    let i = bits 1. in
    check v i i i i i i i i;
    eq (Int64.bits_of_float (low_to v)) i;
    eq (w0 (low_of 2.)) (bits 2.);
    let v = const8 1. 2. 3. 4. 5. 6. 7. 8. in
    check v (bits 1.) (bits 2.) (bits 3.) (bits 4.) (bits 5.) (bits 6.) (bits 7.)
      (bits 8.);
    eq (Int64.bits_of_float (low_to v)) (bits 1.);
    words v
end

(* Int32x16: static cast and constants *)
module Int32x16 = struct
  external low_of : int32 -> int64x8 = "caml_vec512_unreachable"
    "caml_int32x16_low_of_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : int64x8 -> int32 = "caml_vec512_unreachable"
    "caml_int32x16_low_to_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int32 -> int64x8 = "caml_vec512_unreachable"
    "caml_int32x16_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

  let i32 i = Int64.(logand (of_int32 i) 0xffffffffL)

  let pack lo hi = Int64.(logor (shift_left (i32 hi) 32) (i32 lo))

  let[@inline always] check1 i =
    let p = pack i i in
    let v = const1 i in
    check v p p p p p p p p;
    eq (Int64.of_int32 (low_to v)) (Int64.of_int32 i)

  let () =
    check1 0l;
    check1 1l;
    check1 (-1l);
    check1 0xffl;
    check1 Int32.min_int;
    check1 Int32.max_int;
    eq (Int64.logand (w0 (low_of 7l)) 0xffffffffL) (i32 7l)
end

(* Int16x32: static cast and constants *)
module Int16x32 = struct
  external low_to : (int64x8[@unboxed]) -> int16# = "caml_vec512_unreachable"
    "caml_int16x32_low_to_int16"
  [@@noalloc] [@@builtin]

  external const1 : int16# -> (int64x8[@unboxed]) = "caml_vec512_unreachable"
    "caml_int16x32_const1"
  [@@noalloc] [@@builtin]

  let i16 i = Int64.(logand (of_int i) 0xffffL)

  let[@inline always] check1 i =
    let p = i16 i in
    let p = Int64.(logor (shift_left p 16) p) in
    let p = Int64.(logor (shift_left p 32) p) in
    let v = const1 (Int16_u.of_int i) in
    check v p p p p p p p p;
    eq (Int64.of_int (Int16_u.to_int (low_to v) land 0xffff)) (i16 i)

  let () =
    check1 0;
    check1 1;
    check1 0xffff;
    check1 0x8000
end

(* Int8x64: static cast and constants *)
module Int8x64 = struct
  external low_to : (int64x8[@unboxed]) -> int8# = "caml_vec512_unreachable"
    "caml_int8x64_low_to_int8"
  [@@noalloc] [@@builtin]

  external const1 : int8# -> (int64x8[@unboxed]) = "caml_vec512_unreachable"
    "caml_int8x64_const1"
  [@@noalloc] [@@builtin]

  let i8 i = Int64.(logand (of_int i) 0xffL)

  let[@inline always] check1 i =
    let p = i8 i in
    let p = Int64.(logor (shift_left p 8) p) in
    let p = Int64.(logor (shift_left p 16) p) in
    let p = Int64.(logor (shift_left p 32) p) in
    let v = const1 (Int8_u.of_int i) in
    check v p p p p p p p p;
    eq (Int64.of_int (Int8_u.to_int (low_to v) land 0xff)) (i8 i)

  let () =
    check1 0;
    check1 1;
    check1 0xff;
    check1 0x80
end

(* Float32x16: static cast and constants *)
module Float32x16 = struct
  external low_to : int64x8 -> float32 = "caml_vec512_unreachable"
    "caml_float32x16_low_to_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : float32 -> int64x8 = "caml_vec512_unreachable"
    "caml_float32x16_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let v = const1 1.s in
    let i = 0x3f8000003f800000L in
    check v i i i i i i i i;
    eq (Int64.logand (Int64.of_int32 (Float32.to_bits (low_to v)))
          0xffffffffL) 0x3f800000L
end

(* Reinterpret casts between vector widths (low bits preserved) *)
module Reinterpret = struct
  external i64x8_of_i64s :
    int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
    -> int64x8 = "" "vec512_of_int64s"
  [@@noalloc] [@@unboxed]

  external low_of_vec128 : int64x2 -> int64x8 = "caml_vec512_unreachable"
    "caml_vec512_low_of_vec128"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_to_vec128 : int64x8 -> int64x2 = "caml_vec512_unreachable"
    "caml_vec512_low_to_vec128"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_of_vec256 : int64x4 -> int64x8 = "caml_vec512_unreachable"
    "caml_vec512_low_of_vec256"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_to_vec256 : int64x8 -> int64x4 = "caml_vec512_unreachable"
    "caml_vec512_low_to_vec256"
  [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let v = i64x8_of_i64s 1L 2L 3L 4L 5L 6L 7L 8L in
    (* cast is the identity reinterpret *)
    check (cast v) 1L 2L 3L 4L 5L 6L 7L 8L;
    (* round-trip through the low 128 bits: low 2 lanes preserved *)
    let v' = low_of_vec128 (low_to_vec128 v) in
    eq (w0 v') 1L;
    eq (w1 v') 2L;
    (* round-trip through the low 256 bits: low 4 lanes preserved *)
    let v'' = low_of_vec256 (low_to_vec256 v) in
    eq (w0 v'') 1L;
    eq (w1 v'') 2L;
    eq (w2 v'') 3L;
    eq (w3 v'') 4L
end
