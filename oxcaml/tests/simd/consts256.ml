open Stdlib
open Utils

[@@@ocaml.warning "-unused-module"]

external int64x4_word0_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int64x4_word1_int64 : int64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int64x4_word2_int64 : int64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int64x4_word3_int64 : int64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int32x8_word0_int64 : int32x8 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int32x8_word1_int64 : int32x8 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int32x8_word2_int64 : int32x8 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int32x8_word3_int64 : int32x8 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int16x16_word0_int64 : int16x16 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int16x16_word1_int64 : int16x16 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int16x16_word2_int64 : int16x16 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int16x16_word3_int64 : int16x16 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int8x32_word0_int64 : int8x32 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int8x32_word1_int64 : int8x32 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int8x32_word2_int64 : int8x32 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int8x32_word3_int64 : int8x32 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external float32x8_word0_int64 : float32x8 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external float32x8_word1_int64 : float32x8 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external float32x8_word2_int64 : float32x8 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external float32x8_word3_int64 : float32x8 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external float64x4_word0_int64 : float64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external float64x4_word1_int64 : float64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external float64x4_word2_int64 : float64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external float64x4_word3_int64 : float64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

module Float32x8 = struct
  type t = float32x8

  external low_to : t -> float32 = "" "caml_float32x8_low_to_float32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : float32 -> t = "" "caml_float32x8_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const8 :
    float32 ->
    float32 ->
    float32 ->
    float32 ->
    float32 ->
    float32 ->
    float32 ->
    float32 ->
    t = "" "caml_float32x8_const8"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let v1 = const1 1.s in
    let v2 = const1 2.s in
    let w0_1 = float32x8_word0_int64 v1 in
    let w1_1 = float32x8_word1_int64 v1 in
    let w2_1 = float32x8_word2_int64 v1 in
    let w3_1 = float32x8_word3_int64 v1 in
    let w0_2 = float32x8_word0_int64 v2 in
    let w1_2 = float32x8_word1_int64 v2 in
    let w2_2 = float32x8_word2_int64 v2 in
    let w3_2 = float32x8_word3_int64 v2 in
    eq4 w0_1 w1_1 w2_1 w3_1 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L;
    eq4 w0_2 w1_2 w2_2 w3_2 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L;
    let f0 = low_to v1 in
    let f1 = low_to v2 in
    eqf32 f0 f1 1.s 2.s

  let () =
    let v1 = const8 1.s 2.s 3.s 4.s 5.s 6.s 7.s 8.s in
    let v2 = const8 9.s 10.s 11.s 12.s 13.s 14.s 15.s 16.s in
    let w0_1 = float32x8_word0_int64 v1 in
    let w1_1 = float32x8_word1_int64 v1 in
    let w2_1 = float32x8_word2_int64 v1 in
    let w3_1 = float32x8_word3_int64 v1 in
    let w0_2 = float32x8_word0_int64 v2 in
    let w1_2 = float32x8_word1_int64 v2 in
    let w2_2 = float32x8_word2_int64 v2 in
    let w3_2 = float32x8_word3_int64 v2 in
    eq4 w0_1 w1_1 w2_1 w3_1 0x400000003f800000L 0x4080000040400000L
      0x40c0000040a00000L 0x4100000040e00000L;
    eq4 w0_2 w1_2 w2_2 w3_2 0x4120000041100000L 0x4140000041300000L
      0x4160000041500000L 0x4180000041700000L;
    let f0 = low_to v1 in
    let f1 = low_to v2 in
    eqf32 f0 f1 1.s 9.s
end

module Float64x4 = struct
  type t = float64x4

  external low_to : t -> float = "" "caml_float64x4_low_to_float"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : float -> t = "" "caml_float64x4_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const4 : float -> float -> float -> float -> t
    = "" "caml_float64x4_const4"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    let v1 = const1 1. in
    let v2 = const1 2. in
    let w0_1 = float64x4_word0_int64 v1 in
    let w1_1 = float64x4_word1_int64 v1 in
    let w2_1 = float64x4_word2_int64 v1 in
    let w3_1 = float64x4_word3_int64 v1 in
    let w0_2 = float64x4_word0_int64 v2 in
    let w1_2 = float64x4_word1_int64 v2 in
    let w2_2 = float64x4_word2_int64 v2 in
    let w3_2 = float64x4_word3_int64 v2 in
    eq4 w0_1 w1_1 w2_1 w3_1 0x3ff0000000000000L 0x3ff0000000000000L
      0x3ff0000000000000L 0x3ff0000000000000L;
    eq4 w0_2 w1_2 w2_2 w3_2 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L;
    let f0 = low_to v1 in
    let f1 = low_to v2 in
    eqf f0 f1 1. 2.

  let () =
    let v1 = const4 1. 2. 3. 4. in
    let v2 = const4 5. 6. 7. 8. in
    let w0_1 = float64x4_word0_int64 v1 in
    let w1_1 = float64x4_word1_int64 v1 in
    let w2_1 = float64x4_word2_int64 v1 in
    let w3_1 = float64x4_word3_int64 v1 in
    let w0_2 = float64x4_word0_int64 v2 in
    let w1_2 = float64x4_word1_int64 v2 in
    let w2_2 = float64x4_word2_int64 v2 in
    let w3_2 = float64x4_word3_int64 v2 in
    eq4 w0_1 w1_1 w2_1 w3_1 0x3ff0000000000000L 0x4000000000000000L
      0x4008000000000000L 0x4010000000000000L;
    eq4 w0_2 w1_2 w2_2 w3_2 0x4014000000000000L 0x4018000000000000L
      0x401c000000000000L 0x4020000000000000L;
    let f0 = low_to v1 in
    let f1 = low_to v2 in
    eqf f0 f1 1. 5.
end

module Int64x4 = struct
  type t = int64x4

  external low_to : t -> int64 = "" "caml_int64x4_low_to_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int64 -> t = "" "caml_int64x4_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const4 : int64 -> int64 -> int64 -> int64 -> t
    = "" "caml_int64x4_const4"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] check1 i =
    let v = const1 i in
    let w0 = int64x4_word0_int64 v in
    let w1 = int64x4_word1_int64 v in
    let w2 = int64x4_word2_int64 v in
    let w3 = int64x4_word3_int64 v in
    eq4 w0 w1 w2 w3 i i i i;
    let _i = low_to v in
    eq _i 0L i 0L

  let () =
    check1 0L;
    check1 1L;
    check1 (-1L);
    check1 0xffffL;
    check1 (-0xffffL);
    check1 Int64.min_int;
    check1 Int64.max_int

  let[@inline always] check4 i j k l =
    let v = const4 i j k l in
    let w0 = int64x4_word0_int64 v in
    let w1 = int64x4_word1_int64 v in
    let w2 = int64x4_word2_int64 v in
    let w3 = int64x4_word3_int64 v in
    eq4 w0 w1 w2 w3 i j k l;
    let _i = low_to v in
    eq _i 0L i 0L

  let () =
    check4 0L 1L 2L 3L;
    check4 4L 5L 6L 7L;
    check4 (-1L) (-2L) (-3L) (-4L);
    check4 Int64.min_int Int64.max_int Int64.min_int Int64.max_int;
    check4 Int64.max_int Int64.min_int Int64.max_int Int64.min_int
end

module Int32x8 = struct
  type t = int32x8

  external low_to : t -> int32 = "" "caml_int32x8_low_to_int32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int32 -> t = "" "caml_int32x8_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const8 :
    int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> t
    = "" "caml_int32x8_const8"
    [@@noalloc] [@@unboxed] [@@builtin]

  let i32 i = Int64.(of_int32 i |> logand 0xffffffffL)

  let[@inline always] check1 i =
    let i64 = Int64.(logor (shift_left (i32 i) 32) (i32 i)) in
    let v = const1 i in
    let w0 = int32x8_word0_int64 v in
    let w1 = int32x8_word1_int64 v in
    let w2 = int32x8_word2_int64 v in
    let w3 = int32x8_word3_int64 v in
    eq4 w0 w1 w2 w3 i64 i64 i64 i64;
    let _i = low_to v in
    eql _i 0l i 0l

  let () =
    check1 0l;
    check1 1l;
    check1 (-1l);
    check1 0xffl;
    check1 (-0xffl);
    check1 Int32.min_int;
    check1 Int32.max_int

  let[@inline always] check8 a b c d e f g h =
    let v = const8 a b c d e f g h in
    let w0 = int32x8_word0_int64 v in
    let w1 = int32x8_word1_int64 v in
    let w2 = int32x8_word2_int64 v in
    let w3 = int32x8_word3_int64 v in
    let ab = Int64.(logor (shift_left (i32 b) 32) (i32 a)) in
    let cd = Int64.(logor (shift_left (i32 d) 32) (i32 c)) in
    let ef = Int64.(logor (shift_left (i32 f) 32) (i32 e)) in
    let gh = Int64.(logor (shift_left (i32 h) 32) (i32 g)) in
    eq4 w0 w1 w2 w3 ab cd ef gh;
    let _i = low_to v in
    eql _i 0l a 0l

  let () =
    check8 0l 1l 2l 3l 4l 5l 6l 7l;
    check8 8l 9l 10l 11l 12l 13l 14l 15l;
    check8 (-1l) (-2l) (-3l) (-4l) (-5l) (-6l) (-7l) (-8l);
    check8 Int32.min_int Int32.max_int Int32.min_int Int32.max_int Int32.min_int
      Int32.max_int Int32.min_int Int32.max_int;
    check8 Int32.max_int Int32.min_int Int32.max_int Int32.min_int Int32.max_int
      Int32.min_int Int32.max_int Int32.min_int
end

module Int16x16 = struct
  type t = int16x16

  external low_to : (t[@unboxed]) -> (int[@untagged])
    = "" "caml_int16x16_low_to_int"
    [@@noalloc] [@@builtin]

  external const1 : (int[@untagged]) -> (t[@unboxed])
    = "" "caml_int16x16_const1"
    [@@noalloc] [@@builtin]

  external const16 :
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (t[@unboxed]) = "" "caml_int16x16_const16"
    [@@noalloc] [@@builtin]

  let i16 i = Int64.(of_int i |> logand 0xffffL)

  let[@inline always] check1 i =
    let i64 = i16 i in
    let i64 = Int64.(logor (shift_left i64 16) i64) in
    let i64 = Int64.(logor (shift_left i64 32) i64) in
    let v = const1 i in
    let w0 = int16x16_word0_int64 v in
    let w1 = int16x16_word1_int64 v in
    let w2 = int16x16_word2_int64 v in
    let w3 = int16x16_word3_int64 v in
    eq4 w0 w1 w2 w3 i64 i64 i64 i64;
    let _i = low_to v in
    eqi _i 0 i 0

  let () =
    check1 0;
    check1 1;
    check1 0xffff;
    check1 0x8000

  let[@inline always] check16 a b c d e f g h i j k l m n o p =
    let w0 =
      Int64.(
        logor
          (logor (shift_left (i16 d) 48) (shift_left (i16 c) 32))
          (logor (shift_left (i16 b) 16) (i16 a)))
    in
    let w1 =
      Int64.(
        logor
          (logor (shift_left (i16 h) 48) (shift_left (i16 g) 32))
          (logor (shift_left (i16 f) 16) (i16 e)))
    in
    let w2 =
      Int64.(
        logor
          (logor (shift_left (i16 l) 48) (shift_left (i16 k) 32))
          (logor (shift_left (i16 j) 16) (i16 i)))
    in
    let w3 =
      Int64.(
        logor
          (logor (shift_left (i16 p) 48) (shift_left (i16 o) 32))
          (logor (shift_left (i16 n) 16) (i16 m)))
    in
    let v = const16 a b c d e f g h i j k l m n o p in
    let _w0 = int16x16_word0_int64 v in
    let _w1 = int16x16_word1_int64 v in
    let _w2 = int16x16_word2_int64 v in
    let _w3 = int16x16_word3_int64 v in
    eq4 _w0 _w1 _w2 _w3 w0 w1 w2 w3;
    let _a = low_to v in
    eqi _a 0 a 0

  let () =
    check16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;
    check16 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31;
    check16 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff
      0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff;
    check16 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000
      0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000;
    check16 0xffff 0 0xffff 0 0xffff 0 0xffff 0 0xffff 0 0xffff 0 0xffff 0
      0xffff 0
end

module Int8x32 = struct
  type t = int8x32

  external low_to : (t[@unboxed]) -> (int[@untagged])
    = "" "caml_int8x32_low_to_int"
    [@@noalloc] [@@builtin]

  external const1 : (int[@untagged]) -> (t[@unboxed]) = "" "caml_int8x32_const1"
    [@@noalloc] [@@builtin]

  external const32 :
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (t[@unboxed]) = "" "caml_int8x32_const32"
    [@@noalloc] [@@builtin]

  let i8 i = Int64.(of_int i |> logand 0xffL)

  let[@inline always] check1 i =
    let i64 = i8 i in
    let i64 = Int64.(logor (shift_left i64 8) i64) in
    let i64 = Int64.(logor (shift_left i64 16) i64) in
    let i64 = Int64.(logor (shift_left i64 32) i64) in
    let v = const1 i in
    let w0 = int8x32_word0_int64 v in
    let w1 = int8x32_word1_int64 v in
    let w2 = int8x32_word2_int64 v in
    let w3 = int8x32_word3_int64 v in
    eq4 w0 w1 w2 w3 i64 i64 i64 i64;
    let _i = low_to v in
    eqi _i 0 i 0

  let () =
    check1 0;
    check1 1;
    check1 0xff;
    check1 0x80

  let[@inline always] check32 a b c d e f g h i j k l m n o p q r s t u v w x y
      z aa bb cc dd ee ff =
    let l32 =
      Int64.(
        logor
          (logor (shift_left (i8 d) 24) (shift_left (i8 c) 16))
          (logor (shift_left (i8 b) 8) (i8 a)))
    in
    let h32 =
      Int64.(
        logor
          (logor (shift_left (i8 h) 24) (shift_left (i8 g) 16))
          (logor (shift_left (i8 f) 8) (i8 e)))
    in
    let w0 = Int64.(logor (shift_left h32 32) l32) in
    let l32 =
      Int64.(
        logor
          (logor (shift_left (i8 l) 24) (shift_left (i8 k) 16))
          (logor (shift_left (i8 j) 8) (i8 i)))
    in
    let h32 =
      Int64.(
        logor
          (logor (shift_left (i8 p) 24) (shift_left (i8 o) 16))
          (logor (shift_left (i8 n) 8) (i8 m)))
    in
    let w1 = Int64.(logor (shift_left h32 32) l32) in
    let l32 =
      Int64.(
        logor
          (logor (shift_left (i8 t) 24) (shift_left (i8 s) 16))
          (logor (shift_left (i8 r) 8) (i8 q)))
    in
    let h32 =
      Int64.(
        logor
          (logor (shift_left (i8 x) 24) (shift_left (i8 w) 16))
          (logor (shift_left (i8 v) 8) (i8 u)))
    in
    let w2 = Int64.(logor (shift_left h32 32) l32) in
    let l32 =
      Int64.(
        logor
          (logor (shift_left (i8 bb) 24) (shift_left (i8 aa) 16))
          (logor (shift_left (i8 z) 8) (i8 y)))
    in
    let h32 =
      Int64.(
        logor
          (logor (shift_left (i8 ff) 24) (shift_left (i8 ee) 16))
          (logor (shift_left (i8 dd) 8) (i8 cc)))
    in
    let w3 = Int64.(logor (shift_left h32 32) l32) in
    let v =
      const32 a b c d e f g h i j k l m n o p q r s t u v w x y z aa bb cc dd ee
        ff
    in
    let _w0 = int8x32_word0_int64 v in
    let _w1 = int8x32_word1_int64 v in
    let _w2 = int8x32_word2_int64 v in
    let _w3 = int8x32_word3_int64 v in
    eq4 _w0 _w1 _w2 _w3 w0 w1 w2 w3;
    let _a = low_to v in
    eqi _a 0 a 0

  let () =
    check32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      26 27 28 29 30 31;
    check32 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
      55 56 57 58 59 60 61 62 63;
    check32 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
      0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
      0xff 0xff 0xff 0xff;
    check32 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80
      0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80
      0x80 0x80 0x80 0x80;
    check32 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff
      0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0
end
