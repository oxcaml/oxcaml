/* Stubs for unboxed_arm64.ml (native only). */

#include <stdint.h>
#include <arm_neon.h>
#include "caml/mlvalues.h"

/* The NEON lane intrinsics need constant indices, so these go via memory. */

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
  return vaddq_f32(p.a, vaddq_f32(p.b, p.b));
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
  return p.a + 2 * p.b + 4 * p.c + 8 * p.d;
}

typedef struct { double a; double b; } f64_pair_struct;
typedef struct { f64_pair_struct p; double c; } f64_nested_struct;

double sum_nested_f64_native(f64_nested_struct p) {
  return p.p.a + 2 * p.p.b + 4 * p.c;
}

f64_nested_struct ret_nested_f64_native(value unit) {
  f64_nested_struct res = { { 1.25, -2.5 }, 3.75 };
  return res;
}

typedef struct {
  float32x4_t a; float32x4_t b; float32x4_t c; float32x4_t d;
} f32x4_quad_struct;

f32x4_quad_struct ret_f32x4_quad_native(value unit) {
  float a[4] = { 1.0f, 2.0f, 3.0f, 4.0f };
  float b[4] = { 5.0f, 6.0f, 7.0f, 8.0f };
  float c[4] = { 9.0f, 10.0f, 11.0f, 12.0f };
  float d[4] = { 13.0f, 14.0f, 15.0f, 16.0f };
  f32x4_quad_struct res =
    { vld1q_f32(a), vld1q_f32(b), vld1q_f32(c), vld1q_f32(d) };
  return res;
}

float32x4_t sum_f32x4_quad_native(f32x4_quad_struct p) {
  float32x4_t two = vdupq_n_f32(2.0f);
  return vaddq_f32(p.a, vmulq_f32(two, vaddq_f32(p.b, vmulq_f32(two,
           vaddq_f32(p.c, vmulq_f32(two, p.d))))));
}

int64x2_t make_i64x2_native(int64_t a, int64_t b) {
  int64_t elts[2] = { a, b };
  return vld1q_s64(elts);
}

static double i64x2_lanes(int64x2_t v) {
  int64_t elts[2];
  vst1q_s64(elts, v);
  return (double) (elts[0] + 2 * elts[1]);
}

static double f32x4_lanes(float32x4_t v) {
  float elts[4];
  vst1q_f32(elts, v);
  return (double) elts[0] + 2 * (double) elts[1] + 4 * (double) elts[2]
    + 8 * (double) elts[3];
}

double f64x7_i64x2_pair_f64_native(double a0, double a1, double a2, double a3,
                                   double a4, double a5, double a6,
                                   i64x2_pair_struct p, double y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * i64x2_lanes(p.a) + 512 * i64x2_lanes(p.b) + 2048 * y;
}

double f64x8_f32x4_pair_f64_native(double a0, double a1, double a2, double a3,
                                   double a4, double a5, double a6, double a7,
                                   f32x4_pair_struct p, double y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * a7 + 256 * f32x4_lanes(p.a) + 4096 * f32x4_lanes(p.b)
    + 65536 * y;
}
