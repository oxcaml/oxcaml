[@@@ocaml.warning "-unused-module"]

open Utils256
open Builtins
module Float64 = Utils256.Float64

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
  let a = Float64.to_float64x2 2.0 2.0 in
  let b = Float64.to_float64x2 3.0 3.0 in
  let c = Float64.to_float64x2 1.0 1.0 in
  let result = FMA.float64x2_mul_add a b c in
  let expect = Float64.to_float64x2 7.0 7.0 in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = Float64.to_float64x2 3.0 3.0 in
  let b = Float64.to_float64x2 2.0 2.0 in
  let c = Float64.to_float64x2 1.0 1.0 in
  let result = FMA.float64x2_mul_sub a b c in
  let expect = Float64.to_float64x2 5.0 5.0 in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_addsub: [a0*b0 - c0, a1*b1 + c1] *)
  let a = Float64.to_float64x2 2.0 3.0 in
  let b = Float64.to_float64x2 3.0 2.0 in
  let c = Float64.to_float64x2 1.0 1.0 in
  let result = FMA.float64x2_mul_addsub a b c in
  let expect = Float64.to_float64x2 5.0 7.0 in
  eq_float64x2 ~result ~expect

let () =
  (* Test mul_subadd: [a0*b0 + c0, a1*b1 - c1] *)
  let a = Float64.to_float64x2 3.0 2.0 in
  let b = Float64.to_float64x2 2.0 3.0 in
  let c = Float64.to_float64x2 1.0 1.0 in
  let result = FMA.float64x2_mul_subadd a b c in
  let expect = Float64.to_float64x2 7.0 5.0 in
  eq_float64x2 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = Float64.to_float64x2 2.0 2.0 in
  let b = Float64.to_float64x2 2.0 2.0 in
  let c = Float64.to_float64x2 8.0 8.0 in
  let result = FMA.float64x2_neg_mul_add a b c in
  let expect = Float64.to_float64x2 4.0 4.0 in
  eq_float64x2 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = Float64.to_float64x2 2.0 2.0 in
  let b = Float64.to_float64x2 3.0 3.0 in
  let c = Float64.to_float64x2 1.0 1.0 in
  let result = FMA.float64x2_neg_mul_sub a b c in
  let expect = Float64.to_float64x2 (-7.0) (-7.0) in
  eq_float64x2 ~result ~expect

(* float32x4 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = Float32.to_float32x4' 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x4' 3.0s 3.0s 3.0s 3.0s in
  let c = Float32.to_float32x4' 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x4_mul_add a b c in
  let expect = Float32.to_float32x4' 7.0s 7.0s 7.0s 7.0s in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = Float32.to_float32x4' 3.0s 3.0s 3.0s 3.0s in
  let b = Float32.to_float32x4' 2.0s 2.0s 2.0s 2.0s in
  let c = Float32.to_float32x4' 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x4_mul_sub a b c in
  let expect = Float32.to_float32x4' 5.0s 5.0s 5.0s 5.0s in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a = Float32.to_float32x4' 2.0s 3.0s 2.0s 3.0s in
  let b = Float32.to_float32x4' 3.0s 2.0s 3.0s 2.0s in
  let c = Float32.to_float32x4' 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x4_mul_addsub a b c in
  let expect = Float32.to_float32x4' 5.0s 7.0s 5.0s 7.0s in
  eq_float32x4 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a = Float32.to_float32x4' 3.0s 2.0s 3.0s 2.0s in
  let b = Float32.to_float32x4' 2.0s 3.0s 2.0s 3.0s in
  let c = Float32.to_float32x4' 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x4_mul_subadd a b c in
  let expect = Float32.to_float32x4' 7.0s 5.0s 7.0s 5.0s in
  eq_float32x4 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = Float32.to_float32x4' 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x4' 2.0s 2.0s 2.0s 2.0s in
  let c = Float32.to_float32x4' 8.0s 8.0s 8.0s 8.0s in
  let result = FMA.float32x4_neg_mul_add a b c in
  let expect = Float32.to_float32x4' 4.0s 4.0s 4.0s 4.0s in
  eq_float32x4 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = Float32.to_float32x4' 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x4' 3.0s 3.0s 3.0s 3.0s in
  let c = Float32.to_float32x4' 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x4_neg_mul_sub a b c in
  let expect = Float32.to_float32x4' (-7.0s) (-7.0s) (-7.0s) (-7.0s) in
  eq_float32x4 ~result ~expect

(* float64x4 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = Float64.to_float64x4 2.0 2.0 2.0 2.0 in
  let b = Float64.to_float64x4 3.0 3.0 3.0 3.0 in
  let c = Float64.to_float64x4 1.0 1.0 1.0 1.0 in
  let result = FMA.float64x4_mul_add a b c in
  let expect = Float64.to_float64x4 7.0 7.0 7.0 7.0 in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = Float64.to_float64x4 3.0 3.0 3.0 3.0 in
  let b = Float64.to_float64x4 2.0 2.0 2.0 2.0 in
  let c = Float64.to_float64x4 1.0 1.0 1.0 1.0 in
  let result = FMA.float64x4_mul_sub a b c in
  let expect = Float64.to_float64x4 5.0 5.0 5.0 5.0 in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a = Float64.to_float64x4 2.0 3.0 2.0 3.0 in
  let b = Float64.to_float64x4 3.0 2.0 3.0 2.0 in
  let c = Float64.to_float64x4 1.0 1.0 1.0 1.0 in
  let result = FMA.float64x4_mul_addsub a b c in
  let expect = Float64.to_float64x4 5.0 7.0 5.0 7.0 in
  eq_float64x4 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a = Float64.to_float64x4 3.0 2.0 3.0 2.0 in
  let b = Float64.to_float64x4 2.0 3.0 2.0 3.0 in
  let c = Float64.to_float64x4 1.0 1.0 1.0 1.0 in
  let result = FMA.float64x4_mul_subadd a b c in
  let expect = Float64.to_float64x4 7.0 5.0 7.0 5.0 in
  eq_float64x4 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = Float64.to_float64x4 2.0 2.0 2.0 2.0 in
  let b = Float64.to_float64x4 2.0 2.0 2.0 2.0 in
  let c = Float64.to_float64x4 8.0 8.0 8.0 8.0 in
  let result = FMA.float64x4_neg_mul_add a b c in
  let expect = Float64.to_float64x4 4.0 4.0 4.0 4.0 in
  eq_float64x4 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = Float64.to_float64x4 2.0 2.0 2.0 2.0 in
  let b = Float64.to_float64x4 3.0 3.0 3.0 3.0 in
  let c = Float64.to_float64x4 1.0 1.0 1.0 1.0 in
  let result = FMA.float64x4_neg_mul_sub a b c in
  let expect = Float64.to_float64x4 (-7.0) (-7.0) (-7.0) (-7.0) in
  eq_float64x4 ~result ~expect

(* float32x8 FMA tests *)
let () =
  (* Test mul_add: a * b + c = 2.0 * 3.0 + 1.0 = 7.0 *)
  let a = Float32.to_float32x8' 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x8' 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s in
  let c = Float32.to_float32x8' 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x8_mul_add a b c in
  let expect = Float32.to_float32x8' 7.0s 7.0s 7.0s 7.0s 7.0s 7.0s 7.0s 7.0s in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_sub: a * b - c = 3.0 * 2.0 - 1.0 = 5.0 *)
  let a = Float32.to_float32x8' 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s in
  let b = Float32.to_float32x8' 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s in
  let c = Float32.to_float32x8' 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x8_mul_sub a b c in
  let expect = Float32.to_float32x8' 5.0s 5.0s 5.0s 5.0s 5.0s 5.0s 5.0s 5.0s in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_addsub: alternating sub/add *)
  let a = Float32.to_float32x8' 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s in
  let b = Float32.to_float32x8' 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s in
  let c = Float32.to_float32x8' 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x8_mul_addsub a b c in
  let expect = Float32.to_float32x8' 5.0s 7.0s 5.0s 7.0s 5.0s 7.0s 5.0s 7.0s in
  eq_float32x8 ~result ~expect

let () =
  (* Test mul_subadd: alternating add/sub *)
  let a = Float32.to_float32x8' 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s in
  let b = Float32.to_float32x8' 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s 2.0s 3.0s in
  let c = Float32.to_float32x8' 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x8_mul_subadd a b c in
  let expect = Float32.to_float32x8' 7.0s 5.0s 7.0s 5.0s 7.0s 5.0s 7.0s 5.0s in
  eq_float32x8 ~result ~expect

let () =
  (* Test neg_mul_add: -(a * b) + c = -(2.0 * 2.0) + 8.0 = 4.0 *)
  let a = Float32.to_float32x8' 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x8' 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s in
  let c = Float32.to_float32x8' 8.0s 8.0s 8.0s 8.0s 8.0s 8.0s 8.0s 8.0s in
  let result = FMA.float32x8_neg_mul_add a b c in
  let expect = Float32.to_float32x8' 4.0s 4.0s 4.0s 4.0s 4.0s 4.0s 4.0s 4.0s in
  eq_float32x8 ~result ~expect

let () =
  (* Test neg_mul_sub: -(a * b) - c = -(2.0 * 3.0) - 1.0 = -7.0 *)
  let a = Float32.to_float32x8' 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s 2.0s in
  let b = Float32.to_float32x8' 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s 3.0s in
  let c = Float32.to_float32x8' 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s 1.0s in
  let result = FMA.float32x8_neg_mul_sub a b c in
  let expect =
    Float32.to_float32x8' (-7.0s) (-7.0s) (-7.0s) (-7.0s) (-7.0s) (-7.0s)
      (-7.0s) (-7.0s)
  in
  eq_float32x8 ~result ~expect
