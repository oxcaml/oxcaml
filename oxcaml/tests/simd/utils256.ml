[@@@ocaml.warning "-unused-module"]

open Utils

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int32x8
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int16x16_of_int64s : int64 -> int64 -> int64 -> int64 -> int16x16
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int8x32_of_int64s : int64 -> int64 -> int64 -> int64 -> int8x32
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external float32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> float32x8
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external float64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> float64x4
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

let eq a b c d e f g h =
  if a <> e || b <> f || c <> g || d <> h
  then Printf.printf "%Ld,%Ld,%Ld,%Ld <> %Ld,%Ld,%Ld,%Ld\n" a b c d e f g h

let int64x4_to_quadruple v =
  ( int64x4_first_int64 v,
    int64x4_second_int64 v,
    int64x4_third_int64 v,
    int64x4_fourth_int64 v )

module Vector256_casts = struct
  external int64x4_of_int32x8 : int32x8 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x4_of_int16x16 : int16x16 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x4_of_int8x32 : int8x32 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x4_of_float32x8 : float32x8 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x4_of_float64x4 : float64x4 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int32x8_of_int64s 1L 2L 3L 4L in
    let _1 = int16x16_of_int64s 5L 6L 7L 8L in
    let _2 = int8x32_of_int64s 9L 10L 11L 12L in
    let _3 = float32x8_of_int64s 13L 14L 15L 16L in
    let _4 = float64x4_of_int64s 17L 18L 19L 20L in
    let _0 = int64x4_of_int32x8 (Sys.opaque_identity _0) in
    let _1 = int64x4_of_int16x16 (Sys.opaque_identity _1) in
    let _2 = int64x4_of_int8x32 (Sys.opaque_identity _2) in
    let _3 = int64x4_of_float32x8 (Sys.opaque_identity _3) in
    let _4 = int64x4_of_float64x4 (Sys.opaque_identity _4) in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external int32x8_of_int64x4 : int64x4 -> int32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x8_of_int16x16 : int16x16 -> int32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x8_of_int8x32 : int8x32 -> int32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x8_of_float32x8 : float32x8 -> int32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x8_of_float64x4 : float64x4 -> int32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int64x4_of_int64s 1L 2L 3L 4L in
    let _1 = int16x16_of_int64s 5L 6L 7L 8L in
    let _2 = int8x32_of_int64s 9L 10L 11L 12L in
    let _3 = float32x8_of_int64s 13L 14L 15L 16L in
    let _4 = float64x4_of_int64s 17L 18L 19L 20L in
    let _0 = int32x8_of_int64x4 (Sys.opaque_identity _0) in
    let _1 = int32x8_of_int16x16 (Sys.opaque_identity _1) in
    let _2 = int32x8_of_int8x32 (Sys.opaque_identity _2) in
    let _3 = int32x8_of_float32x8 (Sys.opaque_identity _3) in
    let _4 = int32x8_of_float64x4 (Sys.opaque_identity _4) in
    let _0 = int64x4_of_int32x8 _0 in
    let _1 = int64x4_of_int32x8 _1 in
    let _2 = int64x4_of_int32x8 _2 in
    let _3 = int64x4_of_int32x8 _3 in
    let _4 = int64x4_of_int32x8 _4 in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external int16x16_of_int64x4 : int64x4 -> int16x16
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x16_of_int32x8 : int32x8 -> int16x16
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x16_of_int8x32 : int8x32 -> int16x16
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x16_of_float32x8 : float32x8 -> int16x16
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x16_of_float64x4 : float64x4 -> int16x16
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int64x4_of_int64s 1L 2L 3L 4L in
    let _1 = int32x8_of_int64s 5L 6L 7L 8L in
    let _2 = int8x32_of_int64s 9L 10L 11L 12L in
    let _3 = float32x8_of_int64s 13L 14L 15L 16L in
    let _4 = float64x4_of_int64s 17L 18L 19L 20L in
    let _0 = int16x16_of_int64x4 (Sys.opaque_identity _0) in
    let _1 = int16x16_of_int32x8 (Sys.opaque_identity _1) in
    let _2 = int16x16_of_int8x32 (Sys.opaque_identity _2) in
    let _3 = int16x16_of_float32x8 (Sys.opaque_identity _3) in
    let _4 = int16x16_of_float64x4 (Sys.opaque_identity _4) in
    let _0 = int64x4_of_int16x16 _0 in
    let _1 = int64x4_of_int16x16 _1 in
    let _2 = int64x4_of_int16x16 _2 in
    let _3 = int64x4_of_int16x16 _3 in
    let _4 = int64x4_of_int16x16 _4 in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external int8x32_of_int64x4 : int64x4 -> int8x32
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x32_of_int32x8 : int32x8 -> int8x32
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x32_of_int16x16 : int16x16 -> int8x32
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x32_of_float32x8 : float32x8 -> int8x32
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x32_of_float64x4 : float64x4 -> int8x32
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int64x4_of_int64s 1L 2L 3L 4L in
    let _1 = int32x8_of_int64s 5L 6L 7L 8L in
    let _2 = int16x16_of_int64s 9L 10L 11L 12L in
    let _3 = float32x8_of_int64s 13L 14L 15L 16L in
    let _4 = float64x4_of_int64s 17L 18L 19L 20L in
    let _0 = int8x32_of_int64x4 (Sys.opaque_identity _0) in
    let _1 = int8x32_of_int32x8 (Sys.opaque_identity _1) in
    let _2 = int8x32_of_int16x16 (Sys.opaque_identity _2) in
    let _3 = int8x32_of_float32x8 (Sys.opaque_identity _3) in
    let _4 = int8x32_of_float64x4 (Sys.opaque_identity _4) in
    let _0 = int64x4_of_int8x32 _0 in
    let _1 = int64x4_of_int8x32 _1 in
    let _2 = int64x4_of_int8x32 _2 in
    let _3 = int64x4_of_int8x32 _3 in
    let _4 = int64x4_of_int8x32 _4 in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external float32x8_of_int64x4 : int64x4 -> float32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x8_of_int32x8 : int32x8 -> float32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x8_of_int16x16 : int16x16 -> float32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x8_of_int8x32 : int8x32 -> float32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x8_of_float64x4 : float64x4 -> float32x8
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int64x4_of_int64s 1L 2L 3L 4L in
    let _1 = int32x8_of_int64s 5L 6L 7L 8L in
    let _2 = int16x16_of_int64s 9L 10L 11L 12L in
    let _3 = int8x32_of_int64s 13L 14L 15L 16L in
    let _4 = float64x4_of_int64s 17L 18L 19L 20L in
    let _0 = float32x8_of_int64x4 (Sys.opaque_identity _0) in
    let _1 = float32x8_of_int32x8 (Sys.opaque_identity _1) in
    let _2 = float32x8_of_int16x16 (Sys.opaque_identity _2) in
    let _3 = float32x8_of_int8x32 (Sys.opaque_identity _3) in
    let _4 = float32x8_of_float64x4 (Sys.opaque_identity _4) in
    let _0 = int64x4_of_float32x8 _0 in
    let _1 = int64x4_of_float32x8 _1 in
    let _2 = int64x4_of_float32x8 _2 in
    let _3 = int64x4_of_float32x8 _3 in
    let _4 = int64x4_of_float32x8 _4 in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external float64x4_of_int64x4 : int64x4 -> float64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x4_of_int32x8 : int32x8 -> float64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x4_of_int16x16 : int16x16 -> float64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x4_of_int8x32 : int8x32 -> float64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x4_of_float32x8 : float32x8 -> float64x4
    = "caml_vec256_unreachable" "caml_vec256_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _0 = int64x4_of_int64s 1L 2L 3L 4L in
    let _1 = int32x8_of_int64s 5L 6L 7L 8L in
    let _2 = int16x16_of_int64s 9L 10L 11L 12L in
    let _3 = int8x32_of_int64s 13L 14L 15L 16L in
    let _4 = float32x8_of_int64s 17L 18L 19L 20L in
    let _0 = float64x4_of_int64x4 (Sys.opaque_identity _0) in
    let _1 = float64x4_of_int32x8 (Sys.opaque_identity _1) in
    let _2 = float64x4_of_int16x16 (Sys.opaque_identity _2) in
    let _3 = float64x4_of_int8x32 (Sys.opaque_identity _3) in
    let _4 = float64x4_of_float32x8 (Sys.opaque_identity _4) in
    let _0 = int64x4_of_float64x4 _0 in
    let _1 = int64x4_of_float64x4 _1 in
    let _2 = int64x4_of_float64x4 _2 in
    let _3 = int64x4_of_float64x4 _3 in
    let _4 = int64x4_of_float64x4 _4 in
    let a, b, c, d = int64x4_to_quadruple _0 in
    eq a b c d 1L 2L 3L 4L;
    let a, b, c, d = int64x4_to_quadruple _1 in
    eq a b c d 5L 6L 7L 8L;
    let a, b, c, d = int64x4_to_quadruple _2 in
    eq a b c d 9L 10L 11L 12L;
    let a, b, c, d = int64x4_to_quadruple _3 in
    eq a b c d 13L 14L 15L 16L;
    let a, b, c, d = int64x4_to_quadruple _4 in
    eq a b c d 17L 18L 19L 20L

  external int64x4_of_int64x2 : int64x2 -> int64x4
    = "caml_vec256_unreachable" "caml_vec256_low_of_vec128"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x2_of_int64x4 : int64x4 -> int64x2
    = "caml_vec256_unreachable" "caml_vec256_low_to_vec128"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let _12 = int64x2_of_int64s 1L 2L in
    let up = int64x4_of_int64x2 (Sys.opaque_identity _12) in
    let down = int64x2_of_int64x4 (Sys.opaque_identity up) in
    let _d, _c, b, a = int64x4_to_quadruple up in
    eq a b 0L 0L 1L 2L 0L 0L;
    let a, b = int64x2_low_int64 down, int64x2_high_int64 down in
    eq a b 0L 0L 1L 2L 0L 0L
end
