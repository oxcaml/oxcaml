let[@inline never] [@local never] f_start () = ()
let _ = f_start ()

(* External functions for creating SIMD vectors *)
external int64x2_of_int64s : int64# -> int64# -> int64x2#
  = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int32x4_of_int32s : int32# -> int32# -> int32# -> int32# -> int32x4#
  = "" "vec128_of_int32s" [@@noalloc] [@@unboxed]
external float64x2_of_floats : float# -> float# -> float64x2#
  = "" "vec128_of_doubles" [@@noalloc] [@@unboxed]
external float32x4_of_floats
  : float32# -> float32# -> float32# -> float32# -> float32x4#
  = "" "vec128_of_floats" [@@noalloc] [@@unboxed]

external int64x2_of_int64s_boxed : int64 -> int64 -> int64x2
  = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int32x4_of_int32s_boxed
  : int32 -> int32 -> int32 -> int32 -> int32x4
  = "caml_vec128_unreachable" "vec128_of_int32s" [@@noalloc] [@@unboxed]
external float64x2_of_floats_boxed : float -> float -> float64x2
  = "caml_vec128_unreachable" "vec128_of_doubles" [@@noalloc] [@@unboxed]
external float32x4_of_floats_boxed
  : float32 -> float32 -> float32 -> float32 -> float32x4
  = "caml_vec128_unreachable" "vec128_of_floats" [@@noalloc] [@@unboxed]

external int8x16_of_int64x2_unboxed : int64x2# -> int8x16#
  = "caml_vec128_unreachable" "vec128_cast" [@@noalloc] [@@unboxed]
external int16x8_of_int64x2_unboxed : int64x2# -> int16x8#
  = "caml_vec128_unreachable" "vec128_cast" [@@noalloc] [@@unboxed]
external int8x16_of_int64x2_boxed : int64x2 -> int8x16
  = "caml_vec128_unreachable" "vec128_cast" [@@noalloc] [@@unboxed]
external int16x8_of_int64x2_boxed : int64x2 -> int16x8
  = "caml_vec128_unreachable" "vec128_cast" [@@noalloc] [@@unboxed]

(* 128-bit SIMD vectors - unboxed *)
let[@inline never] [@local never] f_int64x2_unboxed (x: int64x2#) = x
let _ = f_int64x2_unboxed (int64x2_of_int64s #1L #2L)
let _ = f_int64x2_unboxed (int64x2_of_int64s #0L #0L)
let _ = f_int64x2_unboxed (int64x2_of_int64s (-#100L) #9223372036854775807L)

let[@inline never] [@local never] f_int32x4_unboxed (x: int32x4#) = x
let _ = f_int32x4_unboxed (int32x4_of_int32s #1l #2l #3l #4l)
let _ = f_int32x4_unboxed (int32x4_of_int32s #0l #0l #0l #0l)
let _ = f_int32x4_unboxed (int32x4_of_int32s (-#10l) #20l (-#30l) #2147483647l)

let[@inline never] [@local never] f_float64x2_unboxed (x: float64x2#) = x
let _ = f_float64x2_unboxed (float64x2_of_floats #1.0 #2.1)
let _ = f_float64x2_unboxed (float64x2_of_floats #0.0 #0.0)
let _ = f_float64x2_unboxed (float64x2_of_floats (-#3.14) #1e10)

let[@inline never] [@local never] f_float32x4_unboxed (x: float32x4#) = x
let _ = f_float32x4_unboxed (float32x4_of_floats #1.1s #2.0s #3.0s #4.1s)
let _ = f_float32x4_unboxed (float32x4_of_floats #0.0s #0.0s #0.0s #0.0s)
let _ = f_float32x4_unboxed
    (float32x4_of_floats (-#2.5s) #10.0s (-#100.0s) #1e5s)

(* Cast to different element types *)
let[@inline never] [@local never] f_int8x16_unboxed (x: int8x16#) = x
let _ = f_int8x16_unboxed
    (int8x16_of_int64x2_unboxed
       (int64x2_of_int64s #0x0123456789abcdefL #0xfedcba9876543210L))
(* CR sspies: int8 vector elements display inconsistently as mix of hex
   escapes and ASCII characters *)
let _ = f_int8x16_unboxed
    (int8x16_of_int64x2_unboxed (int64x2_of_int64s #0L #0L))

let[@inline never] [@local never] f_int16x8_unboxed (x: int16x8#) = x
let _ = f_int16x8_unboxed
    (int16x8_of_int64x2_unboxed
       (int64x2_of_int64s #0x0123456789abcdefL #0xfedcba9876543210L))
let _ = f_int16x8_unboxed
    (int16x8_of_int64x2_unboxed (int64x2_of_int64s #0L #0L))

(* 128-bit SIMD vectors - boxed *)
let[@inline never] [@local never] f_int64x2_boxed (x: int64x2) = x
let _ = f_int64x2_boxed (int64x2_of_int64s_boxed 1L 2L)
let _ = f_int64x2_boxed (int64x2_of_int64s_boxed 0L 0L)
let _ = f_int64x2_boxed (int64x2_of_int64s_boxed (-100L) 9223372036854775807L)

let[@inline never] [@local never] f_int32x4_boxed (x: int32x4) = x
let _ = f_int32x4_boxed (int32x4_of_int32s_boxed 1l 2l 3l 4l)
let _ = f_int32x4_boxed (int32x4_of_int32s_boxed 0l 0l 0l 0l)
let _ = f_int32x4_boxed (int32x4_of_int32s_boxed (-10l) 20l (-30l) 2147483647l)

let[@inline never] [@local never] f_float64x2_boxed (x: float64x2) = x
let _ = f_float64x2_boxed (float64x2_of_floats_boxed 1.0 2.1)
let _ = f_float64x2_boxed (float64x2_of_floats_boxed 0.0 0.0)
let _ = f_float64x2_boxed (float64x2_of_floats_boxed (-3.14) 1e10)

let[@inline never] [@local never] f_float32x4_boxed (x: float32x4) = x
let _ = f_float32x4_boxed (float32x4_of_floats_boxed 1.1s 2.0s 3.0s 4.1s)
let _ = f_float32x4_boxed (float32x4_of_floats_boxed 0.0s 0.0s 0.0s 0.0s)
let _ = f_float32x4_boxed
    (float32x4_of_floats_boxed (-2.5s) 10.0s (-100.0s) 1e5s)

(* Cast to different element types - boxed *)
let[@inline never] [@local never] f_int8x16_boxed (x: int8x16) = x
let _ = f_int8x16_boxed
    (int8x16_of_int64x2_boxed
       (int64x2_of_int64s_boxed 0x0123456789abcdefL 0xfedcba9876543210L))
let _ = f_int8x16_boxed
    (int8x16_of_int64x2_boxed (int64x2_of_int64s_boxed 0L 0L))

let[@inline never] [@local never] f_int16x8_boxed (x: int16x8) = x
let _ = f_int16x8_boxed
    (int16x8_of_int64x2_boxed
       (int64x2_of_int64s_boxed 0x0123456789abcdefL 0xfedcba9876543210L))
let _ = f_int16x8_boxed
    (int16x8_of_int64x2_boxed (int64x2_of_int64s_boxed 0L 0L))

(* Polymorphic functions with SIMD types *)
let[@inline never] [@local never] f_poly_simd_unboxed (x: 'a) = x
let _ = f_poly_simd_unboxed (int64x2_of_int64s #1L #2L)
let _ = f_poly_simd_unboxed (int32x4_of_int32s #1l #2l #3l #4l)
(* CR sspies: SIMD vectors in polymorphic context display as
   incomprehensible large integers *)
let _ = f_poly_simd_unboxed (float64x2_of_floats #1.0 #2.1)
let _ = f_poly_simd_unboxed (float32x4_of_floats #1.1s #2.0s #3.0s #4.1s)

let[@inline never] [@local never] f_poly_simd_boxed (x: 'a) = x
let _ = f_poly_simd_boxed (int64x2_of_int64s_boxed 1L 2L)
let _ = f_poly_simd_boxed (int32x4_of_int32s_boxed 1l 2l 3l 4l)
(* CR sspies: boxed SIMD vectors in polymorphic context show as opaque
   abstract objects *)
let _ = f_poly_simd_boxed (float64x2_of_floats_boxed 1.0 2.1)
let _ = f_poly_simd_boxed (float32x4_of_floats_boxed 1.1s 2.0s 3.0s 4.1s)

(* Layout-constrained polymorphic functions *)
let[@inline never] [@local never] f_poly_vec128 (type a : vec128) (x: a) = x
let _ = f_poly_vec128 (int64x2_of_int64s #1L #2L)
let _ = f_poly_vec128 (int32x4_of_int32s #1l #2l #3l #4l)
let _ = f_poly_vec128 (float64x2_of_floats #1.0 #2.1)
let _ = f_poly_vec128 (float32x4_of_floats #1.1s #2.0s #3.0s #4.1s)

(* ========== 256-bit SIMD Tests ========== *)

(* External functions for creating 256-bit SIMD vectors *)

external int64x4_of_int64s_boxed
  : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int32x8_of_int32s_boxed
  : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32
    -> int32x8
  = "" "vec256_of_int32s" [@@noalloc] [@@unboxed]
external float64x4_of_doubles_boxed
  : float -> float -> float -> float -> float64x4
  = "" "vec256_of_doubles" [@@noalloc] [@@unboxed]
external float32x8_of_floats_boxed
  : float32 -> float32 -> float32 -> float32 -> float32 -> float32 -> float32
    -> float32 -> float32x8
  = "" "vec256_of_floats" [@@noalloc] [@@unboxed]

external int8x32_of_int64x4_boxed : int64x4 -> int8x32
  = "" "vec256_cast" [@@noalloc] [@@unboxed]
external int16x16_of_int64x4_boxed : int64x4 -> int16x16
  = "" "vec256_cast" [@@noalloc] [@@unboxed]


(* CR sspies: 256-bit SIMD unboxed vectors are currently not tested. *)

(* 256-bit SIMD vectors - boxed *)
let[@inline never] [@local never] f_int64x4_boxed (x: int64x4) = x
let _ = f_int64x4_boxed (int64x4_of_int64s_boxed 1L 2L 3L 4L)
let _ = f_int64x4_boxed (int64x4_of_int64s_boxed 0L 0L 0L 0L)
let _ = f_int64x4_boxed
    (int64x4_of_int64s_boxed (-100L) 9223372036854775807L (-1000L) 1000L)

let[@inline never] [@local never] f_int32x8_boxed (x: int32x8) = x
let _ = f_int32x8_boxed (int32x8_of_int32s_boxed 1l 2l 3l 4l 5l 6l 7l 8l)
let _ = f_int32x8_boxed (int32x8_of_int32s_boxed 0l 0l 0l 0l 0l 0l 0l 0l)
let _ = f_int32x8_boxed
    (int32x8_of_int32s_boxed (-10l) 20l (-30l) 40l (-50l) 60l (-70l)
       2147483647l)

let[@inline never] [@local never] f_float64x4_boxed (x: float64x4) = x
let _ = f_float64x4_boxed (float64x4_of_doubles_boxed 1.0 2.1 3.2 4.3)
let _ = f_float64x4_boxed (float64x4_of_doubles_boxed 0.0 0.0 0.0 0.0)
let _ = f_float64x4_boxed
    (float64x4_of_doubles_boxed (-3.14) 1e10 (-2.718) 6.28)

let[@inline never] [@local never] f_float32x8_boxed (x: float32x8) = x
let _ = f_float32x8_boxed
    (float32x8_of_floats_boxed 1.1s 2.0s 3.0s 4.1s 5.2s 6.3s 7.4s 8.5s)
let _ = f_float32x8_boxed
    (float32x8_of_floats_boxed 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s)
let _ = f_float32x8_boxed
    (float32x8_of_floats_boxed (-2.5s) 10.0s (-100.0s) 1e5s (-1.5s) 2.5s
       (-3.5s) 4.5s)

(* Cast to different element types - boxed *)
let[@inline never] [@local never] f_int8x32_boxed (x: int8x32) = x
let _ = f_int8x32_boxed
    (int8x32_of_int64x4_boxed
       (int64x4_of_int64s_boxed 0x0123456789abcdefL 0xfedcba9876543210L
          0x1122334455667788L 0x8877665544332211L))
let _ = f_int8x32_boxed
    (int8x32_of_int64x4_boxed (int64x4_of_int64s_boxed 0L 0L 0L 0L))

let[@inline never] [@local never] f_int16x16_boxed (x: int16x16) = x
let _ = f_int16x16_boxed
    (int16x16_of_int64x4_boxed
       (int64x4_of_int64s_boxed 0x0123456789abcdefL 0xfedcba9876543210L
          0x1122334455667788L 0x8877665544332211L))
let _ = f_int16x16_boxed
    (int16x16_of_int64x4_boxed (int64x4_of_int64s_boxed 0L 0L 0L 0L))

(* Polymorphic functions with 256-bit SIMD types *)

let[@inline never] [@local never] f_poly_simd256_boxed (x: 'a) = x
let _ = f_poly_simd256_boxed (int64x4_of_int64s_boxed 1L 2L 3L 4L)
let _ = f_poly_simd256_boxed (int32x8_of_int32s_boxed 1l 2l 3l 4l 5l 6l 7l 8l)
let _ = f_poly_simd256_boxed (float64x4_of_doubles_boxed 1.0 2.1 3.2 4.3)
let _ = f_poly_simd256_boxed
    (float32x8_of_floats_boxed 1.1s 2.0s 3.0s 4.1s 5.2s 6.3s 7.4s 8.5s)

