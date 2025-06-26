[@@@ocaml.warning "-unused-value-declaration"]

[@@@ocaml.warning "-unused-module"]

let failmsg = ref (fun () -> ())

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

(* Helper functions for creating 256-bit vectors from int64s *)
external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x4_first_int64 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int32x8
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int32x8_first_int64 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int32x8_second_int64 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int32x8_third_int64 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int32x8_fourth_int64 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int16x16_of_int64s : int64 -> int64 -> int64 -> int64 -> int16x16
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int16x16_first_int64 : int16x16 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int16x16_second_int64 : int16x16 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int16x16_third_int64 : int16x16 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int16x16_fourth_int64 : int16x16 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external int8x32_of_int64s : int64 -> int64 -> int64 -> int64 -> int8x32
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int8x32_first_int64 : int8x32 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int8x32_second_int64 : int8x32 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int8x32_third_int64 : int8x32 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int8x32_fourth_int64 : int8x32 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external float32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> float32x8
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external float32x8_first_int64 : float32x8 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external float32x8_second_int64 : float32x8 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external float32x8_third_int64 : float32x8 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external float32x8_fourth_int64 : float32x8 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external float64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> float64x4
  = "caml_vec256_unreachable" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external float64x4_first_int64 : float64x4 -> int64
  = "caml_vec256_unreachable" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external float64x4_second_int64 : float64x4 -> int64
  = "caml_vec256_unreachable" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external float64x4_third_int64 : float64x4 -> int64
  = "caml_vec256_unreachable" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external float64x4_fourth_int64 : float64x4 -> int64
  = "caml_vec256_unreachable" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

module Vector_casts = struct
  (* Test casts between 256-bit vector types *)
  
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
    (failmsg := fun () -> Printf.printf "256-bit vector casts - int64x4!");
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
    (* Test that values are preserved *)
    let a0, a1 = int64x4_first_int64 _0, int64x4_second_int64 _0 in
    let a2, a3 = int64x4_third_int64 _0, int64x4_fourth_int64 _0 in
    eq a0 a1 1L 2L;
    eq a2 a3 3L 4L;
    let b0, b1 = int64x4_first_int64 _1, int64x4_second_int64 _1 in
    let b2, b3 = int64x4_third_int64 _1, int64x4_fourth_int64 _1 in
    eq b0 b1 5L 6L;
    eq b2 b3 7L 8L;
    let c0, c1 = int64x4_first_int64 _2, int64x4_second_int64 _2 in
    let c2, c3 = int64x4_third_int64 _2, int64x4_fourth_int64 _2 in
    eq c0 c1 9L 10L;
    eq c2 c3 11L 12L;
    let d0, d1 = int64x4_first_int64 _3, int64x4_second_int64 _3 in
    let d2, d3 = int64x4_third_int64 _3, int64x4_fourth_int64 _3 in
    eq d0 d1 13L 14L;
    eq d2 d3 15L 16L;
    let e0, e1 = int64x4_first_int64 _4, int64x4_second_int64 _4 in
    let e2, e3 = int64x4_third_int64 _4, int64x4_fourth_int64 _4 in
    eq e0 e1 17L 18L;
    eq e2 e3 19L 20L

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
    (failmsg := fun () -> Printf.printf "256-bit vector casts - int32x8!");
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
    (* Test that values are preserved *)
    let a0, a1 = int32x8_first_int64 _0, int32x8_second_int64 _0 in
    let a2, a3 = int32x8_third_int64 _0, int32x8_fourth_int64 _0 in
    eq a0 a1 1L 2L;
    eq a2 a3 3L 4L;
    let b0, b1 = int32x8_first_int64 _1, int32x8_second_int64 _1 in
    let b2, b3 = int32x8_third_int64 _1, int32x8_fourth_int64 _1 in
    eq b0 b1 5L 6L;
    eq b2 b3 7L 8L;
    let c0, c1 = int32x8_first_int64 _2, int32x8_second_int64 _2 in
    let c2, c3 = int32x8_third_int64 _2, int32x8_fourth_int64 _2 in
    eq c0 c1 9L 10L;
    eq c2 c3 11L 12L;
    let d0, d1 = int32x8_first_int64 _3, int32x8_second_int64 _3 in
    let d2, d3 = int32x8_third_int64 _3, int32x8_fourth_int64 _3 in
    eq d0 d1 13L 14L;
    eq d2 d3 15L 16L;
    let e0, e1 = int32x8_first_int64 _4, int32x8_second_int64 _4 in
    let e2, e3 = int32x8_third_int64 _4, int32x8_fourth_int64 _4 in
    eq e0 e1 17L 18L;
    eq e2 e3 19L 20L

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
    (failmsg := fun () -> Printf.printf "256-bit vector casts - float64x4!");
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
    (* Test that values are preserved *)
    let a0, a1 = float64x4_first_int64 _0, float64x4_second_int64 _0 in
    let a2, a3 = float64x4_third_int64 _0, float64x4_fourth_int64 _0 in
    eq a0 a1 1L 2L;
    eq a2 a3 3L 4L;
    let b0, b1 = float64x4_first_int64 _1, float64x4_second_int64 _1 in
    let b2, b3 = float64x4_third_int64 _1, float64x4_fourth_int64 _1 in
    eq b0 b1 5L 6L;
    eq b2 b3 7L 8L;
    let c0, c1 = float64x4_first_int64 _2, float64x4_second_int64 _2 in
    let c2, c3 = float64x4_third_int64 _2, float64x4_fourth_int64 _2 in
    eq c0 c1 9L 10L;
    eq c2 c3 11L 12L;
    let d0, d1 = float64x4_first_int64 _3, float64x4_second_int64 _3 in
    let d2, d3 = float64x4_third_int64 _3, float64x4_fourth_int64 _3 in
    eq d0 d1 13L 14L;
    eq d2 d3 15L 16L;
    let e0, e1 = float64x4_first_int64 _4, float64x4_second_int64 _4 in
    let e2, e3 = float64x4_third_int64 _4, float64x4_fourth_int64 _4 in
    eq e0 e1 17L 18L;
    eq e2 e3 19L 20L
end

(* Test completes successfully if no output is produced *)