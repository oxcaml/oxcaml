/* Stubs for unboxed_return4_arm64.ml. The native versions return C structs
   of NEON vector types by value, so that the calling convention used by the
   OCaml compiler for unboxed products of 128-bit vectors is checked against
   the C compiler's implementation of the AAPCS64: such structs are
   homogeneous short-vector aggregates, returned in q0 and q1.

   The test using this file is native-only and arm64-only, so there are no
   bytecode stubs and arm_neon.h is included unconditionally. */

#include <stdint.h>
#include <arm_neon.h>
#include "caml/mlvalues.h"

/* Helpers for constructing and inspecting vectors. (Lane indices are passed
   as tagged OCaml integers; the NEON lane intrinsics require constant
   indices, so we go via memory instead.) */

float32x4_t make_f32x4_native(double a, double b, double c, double d) {
  float elts[4] = { (float) a, (float) b, (float) c, (float) d };
  return vld1q_f32(elts);
}

double f32x4_lane_native(float32x4_t v, value i) {
  float elts[4];
  vst1q_f32(elts, v);
  return (double) elts[Long_val(i)];
}

double f64x2_lane_native(float64x2_t v, value i) {
  double elts[2];
  vst1q_f64(elts, v);
  return elts[Long_val(i)];
}

int64_t i64x2_lane_native(int64x2_t v, value i) {
  int64_t elts[2];
  vst1q_s64(elts, v);
  return elts[Long_val(i)];
}

typedef struct { float32x4_t a; float32x4_t b; } f32x4_pair_struct;

f32x4_pair_struct ret_f32x4_pair_native(value unit) {
  float a[4] = { 1.25f, -2.5f, 3.75f, -4.0f };
  float b[4] = { 10.5f, 20.25f, -30.0f, 40.125f };
  f32x4_pair_struct res = { vld1q_f32(a), vld1q_f32(b) };
  return res;
}

typedef struct { float64x2_t a; float64x2_t b; } f64x2_pair_struct;

f64x2_pair_struct ret_f64x2_pair_native(value unit) {
  double a[2] = { 0.0625, -8.5 };
  double b[2] = { 123.25, -0.5 };
  f64x2_pair_struct res = { vld1q_f64(a), vld1q_f64(b) };
  return res;
}

typedef struct { int64x2_t a; int64x2_t b; } i64x2_pair_struct;

i64x2_pair_struct ret_i64x2_pair_native(value unit) {
  int64_t a[2] = { 111, -222 };
  int64_t b[2] = { 333, -444 };
  i64x2_pair_struct res = { vld1q_s64(a), vld1q_s64(b) };
  return res;
}

typedef struct { float32x4_t a; int64x2_t b; } mixed_pair_struct;

mixed_pair_struct ret_mixed_pair_native(value unit) {
  float a[4] = { 9.5f, -1.5f, 2.0f, 0.25f };
  int64_t b[2] = { 555, -666 };
  mixed_pair_struct res = { vld1q_f32(a), vld1q_s64(b) };
  return res;
}

f32x4_pair_struct swap_f32x4_native(float32x4_t x, float32x4_t y) {
  f32x4_pair_struct res = { y, x };
  return res;
}

typedef struct { float32x4_t a; float32x4_t b; } f32x4_arg_pair_struct;

float32x4_t add_f32x4_pairwise_native(f32x4_arg_pair_struct p) {
  return vaddq_f32(p.a, p.b);
}

typedef struct { double a; double b; double c; } f64_triple_struct;

f64_triple_struct ret_f64_triple_native(value unit) {
  f64_triple_struct res = { 0.25, -0.5, 0.75 };
  return res;
}

typedef struct { double a; double b; double c; double d; } f64_quad_struct;

f64_quad_struct ret_f64_quad_native(value unit) {
  f64_quad_struct res = { 1.5, -2.5, 3.5, -4.5 };
  return res;
}

double sum_f64_quad_native(f64_quad_struct p) {
  return p.a + p.b + p.c + p.d;
}
