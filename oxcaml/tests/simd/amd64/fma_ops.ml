[@@@ocaml.warning "-unused-module"]

open Utils256
open Builtins

(* Scalar float64 FMA tests *)
let () =
  (* Test mul_add: 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = 2.0 in
  let b = 3.0 in
  let c = 1.0 in
  let result = FMA.float64_mul_add a b c in
  let expect = 7.0 in
  eqf' result expect

let () =
  (* Test mul_sub: 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = 3.0 in
  let b = 2.0 in
  let c = 1.0 in
  let result = FMA.float64_mul_sub a b c in
  let expect = 5.0 in
  eqf' result expect

let () =
  (* Test neg_mul_add: -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = 2.0 in
  let b = 2.0 in
  let c = 8.0 in
  let result = FMA.float64_neg_mul_add a b c in
  let expect = 4.0 in
  eqf' result expect

let () =
  (* Test neg_mul_sub: -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = 2.0 in
  let b = 3.0 in
  let c = 1.0 in
  let result = FMA.float64_neg_mul_sub a b c in
  let expect = -7.0 in
  eqf' result expect

(* Scalar float32 FMA tests *)
let () =
  (* Test mul_add: 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = 2.0s in
  let b = 3.0s in
  let c = 1.0s in
  let result = FMA.float32_mul_add a b c in
  let expect = 7.0s in
  eqf32' result expect

let () =
  (* Test mul_sub: 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = 3.0s in
  let b = 2.0s in
  let c = 1.0s in
  let result = FMA.float32_mul_sub a b c in
  let expect = 5.0s in
  eqf32' result expect

let () =
  (* Test neg_mul_add: -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = 2.0s in
  let b = 2.0s in
  let c = 8.0s in
  let result = FMA.float32_neg_mul_add a b c in
  let expect = 4.0s in
  eqf32' result expect

let () =
  (* Test neg_mul_sub: -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = 2.0s in
  let b = 3.0s in
  let c = 1.0s in
  let result = FMA.float32_neg_mul_sub a b c in
  let expect = -7.0s in
  eqf32' result expect

(* float64x2 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = float64x2_of_int64s 0x4000000000000000L 0x4000000000000000L in
  let b = float64x2_of_int64s 0x4008000000000000L 0x4008000000000000L in
  let c = float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L in
  let result = FMA.float64x2_mul_add a b c in
  let expect = float64x2_of_int64s 0x401C000000000000L 0x401C000000000000L in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = float64x2_of_int64s 0x4008000000000000L 0x4008000000000000L in
  let b = float64x2_of_int64s 0x4000000000000000L 0x4000000000000000L in
  let c = float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L in
  let result = FMA.float64x2_mul_sub a b c in
  let expect = float64x2_of_int64s 0x4014000000000000L 0x4014000000000000L in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_addsub: [a0*b0 - c0, a1*b1 + c1] *)
  let a = float64x2_of_int64s 0x4000000000000000L 0x4008000000000000L in
  let b = float64x2_of_int64s 0x4008000000000000L 0x4000000000000000L in
  let c = float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L in
  let result = FMA.float64x2_mul_addsub a b c in
  let expect = float64x2_of_int64s 0x4014000000000000L 0x401C000000000000L in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_subadd: [a0*b0 + c0, a1*b1 - c1] *)
  let a = float64x2_of_int64s 0x4008000000000000L 0x4000000000000000L in
  let b = float64x2_of_int64s 0x4000000000000000L 0x4008000000000000L in
  let c = float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L in
  let result = FMA.float64x2_mul_subadd a b c in
  let expect = float64x2_of_int64s 0x401C000000000000L 0x4014000000000000L in
  eq_float64x2 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = float64x2_of_int64s 0x4000000000000000L 0x4000000000000000L in
  let b = float64x2_of_int64s 0x4000000000000000L 0x4000000000000000L in
  let c = float64x2_of_int64s 0x4020000000000000L 0x4020000000000000L in
  let result = FMA.float64x2_neg_mul_add a b c in
  let expect = float64x2_of_int64s 0x4010000000000000L 0x4010000000000000L in
  eq_float64x2 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = float64x2_of_int64s 0x4000000000000000L 0x4000000000000000L in
  let b = float64x2_of_int64s 0x4008000000000000L 0x4008000000000000L in
  let c = float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L in
  let result = FMA.float64x2_neg_mul_sub a b c in
  let expect = float64x2_of_int64s 0xC01C000000000000L 0xC01C000000000000L in
  eq_float64x2 ~result ~expect

(* float32x4 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = float32x4_of_int64s 0x4000000040000000L 0x4000000040000000L in
  let b = float32x4_of_int64s 0x4040000040400000L 0x4040000040400000L in
  let c = float32x4_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L in
  let result = FMA.float32x4_mul_add a b c in
  let expect = float32x4_of_int64s 0x40e0000040e00000L 0x40e0000040e00000L in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = float32x4_of_int64s 0x4040000040400000L 0x4040000040400000L in
  let b = float32x4_of_int64s 0x4000000040000000L 0x4000000040000000L in
  let c = float32x4_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L in
  let result = FMA.float32x4_mul_sub a b c in
  let expect = float32x4_of_int64s 0x40a0000040a00000L 0x40a0000040a00000L in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a = float32x4_of_int64s 0x4000000040400000L 0x4000000040400000L in
  let b = float32x4_of_int64s 0x4040000040000000L 0x4040000040000000L in
  let c = float32x4_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L in
  let result = FMA.float32x4_mul_addsub a b c in
  let expect = float32x4_of_int64s 0x40e0000040a00000L 0x40e0000040a00000L in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a = float32x4_of_int64s 0x4040000040000000L 0x4040000040000000L in
  let b = float32x4_of_int64s 0x4000000040400000L 0x4000000040400000L in
  let c = float32x4_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L in
  let result = FMA.float32x4_mul_subadd a b c in
  let expect = float32x4_of_int64s 0x40a0000040e00000L 0x40a0000040e00000L in
  eq_float32x4 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = float32x4_of_int64s 0x4000000040000000L 0x4000000040000000L in
  let b = float32x4_of_int64s 0x4000000040000000L 0x4000000040000000L in
  let c = float32x4_of_int64s 0x4100000041000000L 0x4100000041000000L in
  let result = FMA.float32x4_neg_mul_add a b c in
  let expect = float32x4_of_int64s 0x4080000040800000L 0x4080000040800000L in
  eq_float32x4 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = float32x4_of_int64s 0x4000000040000000L 0x4000000040000000L in
  let b = float32x4_of_int64s 0x4040000040400000L 0x4040000040400000L in
  let c = float32x4_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L in
  let result = FMA.float32x4_neg_mul_sub a b c in
  let expect = float32x4_of_int64s 0xc0e00000c0e00000L 0xc0e00000c0e00000L in
  eq_float32x4 ~result ~expect

(* float64x4 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a =
    float64x4_of_int64s 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L
  in
  let b =
    float64x4_of_int64s 0x4008000000000000L 0x4008000000000000L
      0x4008000000000000L 0x4008000000000000L
  in
  let c =
    float64x4_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L
      0x3FF0000000000000L 0x3FF0000000000000L
  in
  let result = FMA.float64x4_mul_add a b c in
  let expect =
    float64x4_of_int64s 0x401C000000000000L 0x401C000000000000L
      0x401C000000000000L 0x401C000000000000L
  in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a =
    float64x4_of_int64s 0x4008000000000000L 0x4008000000000000L
      0x4008000000000000L 0x4008000000000000L
  in
  let b =
    float64x4_of_int64s 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L
  in
  let c =
    float64x4_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L
      0x3FF0000000000000L 0x3FF0000000000000L
  in
  let result = FMA.float64x4_mul_sub a b c in
  let expect =
    float64x4_of_int64s 0x4014000000000000L 0x4014000000000000L
      0x4014000000000000L 0x4014000000000000L
  in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a =
    float64x4_of_int64s 0x4000000000000000L 0x4008000000000000L
      0x4000000000000000L 0x4008000000000000L
  in
  let b =
    float64x4_of_int64s 0x4008000000000000L 0x4000000000000000L
      0x4008000000000000L 0x4000000000000000L
  in
  let c =
    float64x4_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L
      0x3FF0000000000000L 0x3FF0000000000000L
  in
  let result = FMA.float64x4_mul_addsub a b c in
  let expect =
    float64x4_of_int64s 0x4014000000000000L 0x401C000000000000L
      0x4014000000000000L 0x401C000000000000L
  in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a =
    float64x4_of_int64s 0x4008000000000000L 0x4000000000000000L
      0x4008000000000000L 0x4000000000000000L
  in
  let b =
    float64x4_of_int64s 0x4000000000000000L 0x4008000000000000L
      0x4000000000000000L 0x4008000000000000L
  in
  let c =
    float64x4_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L
      0x3FF0000000000000L 0x3FF0000000000000L
  in
  let result = FMA.float64x4_mul_subadd a b c in
  let expect =
    float64x4_of_int64s 0x401C000000000000L 0x4014000000000000L
      0x401C000000000000L 0x4014000000000000L
  in
  eq_float64x4 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a =
    float64x4_of_int64s 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L
  in
  let b =
    float64x4_of_int64s 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L
  in
  let c =
    float64x4_of_int64s 0x4020000000000000L 0x4020000000000000L
      0x4020000000000000L 0x4020000000000000L
  in
  let result = FMA.float64x4_neg_mul_add a b c in
  let expect =
    float64x4_of_int64s 0x4010000000000000L 0x4010000000000000L
      0x4010000000000000L 0x4010000000000000L
  in
  eq_float64x4 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a =
    float64x4_of_int64s 0x4000000000000000L 0x4000000000000000L
      0x4000000000000000L 0x4000000000000000L
  in
  let b =
    float64x4_of_int64s 0x4008000000000000L 0x4008000000000000L
      0x4008000000000000L 0x4008000000000000L
  in
  let c =
    float64x4_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L
      0x3FF0000000000000L 0x3FF0000000000000L
  in
  let result = FMA.float64x4_neg_mul_sub a b c in
  let expect =
    float64x4_of_int64s 0xC01C000000000000L 0xC01C000000000000L
      0xC01C000000000000L 0xC01C000000000000L
  in
  eq_float64x4 ~result ~expect

(* float32x8 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a =
    float32x8_of_int64s 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L
  in
  let b =
    float32x8_of_int64s 0x4040000040400000L 0x4040000040400000L
      0x4040000040400000L 0x4040000040400000L
  in
  let c =
    float32x8_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L
  in
  let result = FMA.float32x8_mul_add a b c in
  let expect =
    float32x8_of_int64s 0x40e0000040e00000L 0x40e0000040e00000L
      0x40e0000040e00000L 0x40e0000040e00000L
  in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a =
    float32x8_of_int64s 0x4040000040400000L 0x4040000040400000L
      0x4040000040400000L 0x4040000040400000L
  in
  let b =
    float32x8_of_int64s 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L
  in
  let c =
    float32x8_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L
  in
  let result = FMA.float32x8_mul_sub a b c in
  let expect =
    float32x8_of_int64s 0x40a0000040a00000L 0x40a0000040a00000L
      0x40a0000040a00000L 0x40a0000040a00000L
  in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a =
    float32x8_of_int64s 0x4000000040400000L 0x4000000040400000L
      0x4000000040400000L 0x4000000040400000L
  in
  let b =
    float32x8_of_int64s 0x4040000040000000L 0x4040000040000000L
      0x4040000040000000L 0x4040000040000000L
  in
  let c =
    float32x8_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L
  in
  let result = FMA.float32x8_mul_addsub a b c in
  let expect =
    float32x8_of_int64s 0x40e0000040a00000L 0x40e0000040a00000L
      0x40e0000040a00000L 0x40e0000040a00000L
  in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a =
    float32x8_of_int64s 0x4040000040000000L 0x4040000040000000L
      0x4040000040000000L 0x4040000040000000L
  in
  let b =
    float32x8_of_int64s 0x4000000040400000L 0x4000000040400000L
      0x4000000040400000L 0x4000000040400000L
  in
  let c =
    float32x8_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L
  in
  let result = FMA.float32x8_mul_subadd a b c in
  let expect =
    float32x8_of_int64s 0x40a0000040e00000L 0x40a0000040e00000L
      0x40a0000040e00000L 0x40a0000040e00000L
  in
  eq_float32x8 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a =
    float32x8_of_int64s 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L
  in
  let b =
    float32x8_of_int64s 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L
  in
  let c =
    float32x8_of_int64s 0x4100000041000000L 0x4100000041000000L
      0x4100000041000000L 0x4100000041000000L
  in
  let result = FMA.float32x8_neg_mul_add a b c in
  let expect =
    float32x8_of_int64s 0x4080000040800000L 0x4080000040800000L
      0x4080000040800000L 0x4080000040800000L
  in
  eq_float32x8 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a =
    float32x8_of_int64s 0x4000000040000000L 0x4000000040000000L
      0x4000000040000000L 0x4000000040000000L
  in
  let b =
    float32x8_of_int64s 0x4040000040400000L 0x4040000040400000L
      0x4040000040400000L 0x4040000040400000L
  in
  let c =
    float32x8_of_int64s 0x3f8000003f800000L 0x3f8000003f800000L
      0x3f8000003f800000L 0x3f8000003f800000L
  in
  let result = FMA.float32x8_neg_mul_sub a b c in
  let expect =
    float32x8_of_int64s 0xc0e00000c0e00000L 0xc0e00000c0e00000L
      0xc0e00000c0e00000L 0xc0e00000c0e00000L
  in
  eq_float32x8 ~result ~expect
