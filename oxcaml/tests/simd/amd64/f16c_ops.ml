[@@@ocaml.warning "-unused-module"]

open Utils256
open Builtins

(* Test round-trip-able values *)
let () =
  let two = 0x40000000l in
  let neg_two = 0xc0000000l in
  let half = 0x3f000000l in
  let neg_half = 0xbf000000l in
  let f32_vec = Float32.to_float32x4 two neg_two half neg_half in
  let f16_vec = F16C.cvt_float32x4_float16x8 0 f32_vec in
  let result = F16C.cvt_float16x8_float32x4 f16_vec in
  eq_float32x4 ~result ~expect:f32_vec

let () =
  let zero = 0x00000000l in
  let neg_zero = 0x80000000l in
  let one = 0x3f800000l in
  let neg_one = 0xbf800000l in
  let two = 0x40000000l in
  let neg_two = 0xc0000000l in
  let half = 0x3f000000l in
  let neg_half = 0xbf000000l in
  let f32_vec =
    Float32.to_float32x8 zero neg_zero one neg_one two neg_two half neg_half
  in
  let f16_vec = F16C.cvt_float32x8_float16x8 0 f32_vec in
  let result = F16C.cvt_float16x8_float32x8 f16_vec in
  eq_float32x8 ~result ~expect:f32_vec

(* Test that different rounding modes produce different results *)
let () =
  let v1 = 0x40490001l in
  let v2 = 0xc0490001l in
  let v3 = 0x3f910001l in
  let v4 = 0xbf910001l in
  let f32_vec = Float32.to_float32x4 v1 v2 v3 v4 in
  let f16_nearest = F16C.cvt_float32x4_float16x8 0 f32_vec in
  let f16_down = F16C.cvt_float32x4_float16x8 1 f32_vec in
  let f16_up = F16C.cvt_float32x4_float16x8 2 f32_vec in
  let f16_trunc = F16C.cvt_float32x4_float16x8 3 f32_vec in
  let r_nearest = F16C.cvt_float16x8_float32x4 f16_nearest in
  let r_down = F16C.cvt_float16x8_float32x4 f16_down in
  let r_up = F16C.cvt_float16x8_float32x4 f16_up in
  let r_trunc = F16C.cvt_float16x8_float32x4 f16_trunc in
  let low_n = float32x4_low_int64 r_nearest in
  let low_d = float32x4_low_int64 r_down in
  let low_u = float32x4_low_int64 r_up in
  let low_t = float32x4_low_int64 r_trunc in
  let high_n = float32x4_high_int64 r_nearest in
  let high_d = float32x4_high_int64 r_down in
  let high_u = float32x4_high_int64 r_up in
  let high_t = float32x4_high_int64 r_trunc in
  if low_d = low_u && high_d = high_u
  then Printf.printf "round down == round up\n";
  if low_n = low_d && high_n = high_d
  then Printf.printf "round nearest == round down\n";
  if low_n = low_u && high_n = high_u
  then Printf.printf "round nearest == round up\n";
  if low_t = low_d && high_t = high_d
  then Printf.printf "truncate == round down\n";
  if low_t = low_u && high_t = high_u
  then Printf.printf "truncate == round up\n"

let () =
  let v1 = 0x3d888889l in
  let v2 = 0xbd888889l in
  let v3 = 0x40e12345l in
  let v4 = 0xc0e12345l in
  let v5 = 0x3fa98765l in
  let v6 = 0xbfa98765l in
  let v7 = 0x3e456789l in
  let v8 = 0xbe456789l in
  let f32_vec = Float32.to_float32x8 v1 v2 v3 v4 v5 v6 v7 v8 in
  let f16_nearest = F16C.cvt_float32x8_float16x8 0 f32_vec in
  let f16_down = F16C.cvt_float32x8_float16x8 1 f32_vec in
  let f16_up = F16C.cvt_float32x8_float16x8 2 f32_vec in
  let f16_trunc = F16C.cvt_float32x8_float16x8 3 f32_vec in
  let r_nearest = F16C.cvt_float16x8_float32x8 f16_nearest in
  let r_down = F16C.cvt_float16x8_float32x8 f16_down in
  let r_up = F16C.cvt_float16x8_float32x8 f16_up in
  let r_trunc = F16C.cvt_float16x8_float32x8 f16_trunc in
  let low_n = float32x8_first_int64 r_nearest in
  let low_d = float32x8_first_int64 r_down in
  let low_u = float32x8_first_int64 r_up in
  let low_t = float32x8_first_int64 r_trunc in
  let high_n = float32x8_fourth_int64 r_nearest in
  let high_d = float32x8_fourth_int64 r_down in
  let high_u = float32x8_fourth_int64 r_up in
  let high_t = float32x8_fourth_int64 r_trunc in
  if low_d = low_u && high_d = high_u
  then Printf.printf "256-bit: round down == round up\n";
  if low_n = low_d && high_n = high_d
  then Printf.printf "256-bit: round nearest == round down\n";
  if low_n = low_u && high_n = high_u
  then Printf.printf "256-bit: round nearest == round up\n";
  if low_t = low_d && high_t = high_d
  then Printf.printf "256-bit: truncate == round down\n";
  if low_t = low_u && high_t = high_u
  then Printf.printf "256-bit: truncate == round up\n"
