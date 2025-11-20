[@@@ocaml.warning "-unused-module"]

open Utils256
open Builtins

(* Test round-trip-able values (i.e. have an exact f16 representation) *)
let () =
  let f32_vec = Float32.to_float32x4' 2.s (-2.s) 0.5s (-0.5s) in
  let f16_vec = F16C.cvt_float32x4_float16x8 0 f32_vec in
  let result = F16C.cvt_float16x8_float32x4 f16_vec in
  eq_float32x4 ~result ~expect:f32_vec

let () =
  let f32_vec =
    Float32.to_float32x8' 0.s (-0.s) 1.s (-1.s) 2.s (-2.s) 0.5s (-0.5s)
  in
  let f16_vec = F16C.cvt_float32x8_float16x8 0 f32_vec in
  let result = F16C.cvt_float16x8_float32x8 f16_vec in
  eq_float32x8 ~result ~expect:f32_vec

(* Test rounding modes *)
let () =
  let f32_vec = Float32.to_float32x4' 0.1s (-0.1s) 0.7s (-0.7s) in
  (* Nearest *)
  let expect =
    Float32.to_float32x4' 0.0999755859375s (-0.0999755859375s) 0.7001953125s
      (-0.7001953125s)
  in
  let result =
    F16C.cvt_float32x4_float16x8 0 f32_vec |> F16C.cvt_float16x8_float32x4
  in
  eq_float32x4 ~result ~expect;
  (* Down *)
  let expect =
    Float32.to_float32x4' 0.0999755859375s (-0.10003662109375s) 0.69970703125s
      (-0.7001953125s)
  in
  let result =
    F16C.cvt_float32x4_float16x8 1 f32_vec |> F16C.cvt_float16x8_float32x4
  in
  eq_float32x4 ~result ~expect;
  (* Up *)
  let expect =
    Float32.to_float32x4' 0.10003662109375s (-0.0999755859375s) 0.7001953125s
      (-0.69970703125s)
  in
  let result =
    F16C.cvt_float32x4_float16x8 2 f32_vec |> F16C.cvt_float16x8_float32x4
  in
  eq_float32x4 ~result ~expect;
  (* Trunc *)
  let expect =
    Float32.to_float32x4' 0.0999755859375s (-0.0999755859375s) 0.69970703125s
      (-0.69970703125s)
  in
  let result =
    F16C.cvt_float32x4_float16x8 3 f32_vec |> F16C.cvt_float16x8_float32x4
  in
  eq_float32x4 ~result ~expect

let () =
  let f32_vec =
    Float32.to_float32x8' 0.1s (-0.1s) 0.7s (-0.7s) 0.1s (-0.1s) 0.7s (-0.7s)
  in
  (* Nearest *)
  let expect =
    Float32.to_float32x8' 0.0999755859375s (-0.0999755859375s) 0.7001953125s
      (-0.7001953125s) 0.0999755859375s (-0.0999755859375s) 0.7001953125s
      (-0.7001953125s)
  in
  let result =
    F16C.cvt_float32x8_float16x8 0 f32_vec |> F16C.cvt_float16x8_float32x8
  in
  eq_float32x8 ~result ~expect;
  (* Down *)
  let expect =
    Float32.to_float32x8' 0.0999755859375s (-0.10003662109375s) 0.69970703125s
      (-0.7001953125s) 0.0999755859375s (-0.10003662109375s) 0.69970703125s
      (-0.7001953125s)
  in
  let result =
    F16C.cvt_float32x8_float16x8 1 f32_vec |> F16C.cvt_float16x8_float32x8
  in
  eq_float32x8 ~result ~expect;
  (* Up *)
  let expect =
    Float32.to_float32x8' 0.10003662109375s (-0.0999755859375s) 0.7001953125s
      (-0.69970703125s) 0.10003662109375s (-0.0999755859375s) 0.7001953125s
      (-0.69970703125s)
  in
  let result =
    F16C.cvt_float32x8_float16x8 2 f32_vec |> F16C.cvt_float16x8_float32x8
  in
  eq_float32x8 ~result ~expect;
  (* Trunc *)
  let expect =
    Float32.to_float32x8' 0.0999755859375s (-0.0999755859375s) 0.69970703125s
      (-0.69970703125s) 0.0999755859375s (-0.0999755859375s) 0.69970703125s
      (-0.69970703125s)
  in
  let result =
    F16C.cvt_float32x8_float16x8 3 f32_vec |> F16C.cvt_float16x8_float32x8
  in
  eq_float32x8 ~result ~expect
