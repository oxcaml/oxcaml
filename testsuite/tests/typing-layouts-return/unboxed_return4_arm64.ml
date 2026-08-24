(* TEST
 modules = "vec128_stubs.c";
 reference = "${test_source_directory}/unboxed_return4_arm64.reference";
 flambda2;
 arch_arm64;
 {
   flags = "-extension layouts_beta -extension simd_beta";
   native;
 }
*)

(* Test of unboxed products of 128-bit vectors returned from external calls.

   On arm64 a pair of 128-bit vectors is a homogeneous short-vector
   aggregate, returned in q0 and q1; the native stubs in vec128_stubs.c
   return genuine C structs of NEON vector types, so this checks
   interoperability with the C compiler's implementation of the AAPCS64 (and
   its Apple variant).

   This test is restricted to arm64: on x86-64 the corresponding C structs
   are 32 bytes in size and are therefore returned in memory by the System V
   ABI, which cannot currently be compiled (see to_cmm_extcall.ml). The test
   is also native-only, since it is specifically about native-code calling
   conventions. *)

external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"

(* Helpers for constructing and inspecting vectors, all via C. *)
external make_f32x4 : float# -> float# -> float# -> float# -> float32x4#
  = "" "make_f32x4_native"
external f32x4_lane : float32x4# -> int -> float# = "" "f32x4_lane_native"
external f64x2_lane : float64x2# -> int -> float# = "" "f64x2_lane_native"
external i64x2_lane : int64x2# -> int -> int64# = "" "i64x2_lane_native"

let print_f32x4 name v =
  Printf.eprintf "%s: %.4f %.4f %.4f %.4f\n%!" name
    (box_float (f32x4_lane v 0))
    (box_float (f32x4_lane v 1))
    (box_float (f32x4_lane v 2))
    (box_float (f32x4_lane v 3))

let print_f64x2 name v =
  Printf.eprintf "%s: %.4f %.4f\n%!" name
    (box_float (f64x2_lane v 0))
    (box_float (f64x2_lane v 1))

let print_i64x2 name v =
  Printf.eprintf "%s: %Ld %Ld\n%!" name
    (box_int64 (i64x2_lane v 0))
    (box_int64 (i64x2_lane v 1))

(* A pair of float32x4s, returned in q0 and q1. *)
external ret_f32x4_pair : unit -> #(float32x4# * float32x4#)
  = "" "ret_f32x4_pair_native"

let () =
  let #(a, b) = ret_f32x4_pair () in
  print_f32x4 "f32x4_pair.a" a;
  print_f32x4 "f32x4_pair.b" b

(* A pair of float64x2s. *)
external ret_f64x2_pair : unit -> #(float64x2# * float64x2#)
  = "" "ret_f64x2_pair_native"

let () =
  let #(a, b) = ret_f64x2_pair () in
  print_f64x2 "f64x2_pair.a" a;
  print_f64x2 "f64x2_pair.b" b

(* A pair of int64x2s: integer-element vectors still use the SIMD
   registers. *)
external ret_i64x2_pair : unit -> #(int64x2# * int64x2#)
  = "" "ret_i64x2_pair_native"

let () =
  let #(a, b) = ret_i64x2_pair () in
  print_i64x2 "i64x2_pair.a" a;
  print_i64x2 "i64x2_pair.b" b

(* Vectors of different element types: the AAPCS64 classifies aggregates by
   the fundamental data type of their members, which for both of these is
   "128-bit short vector", so this is still a homogeneous aggregate. *)
external ret_mixed_pair : unit -> #(float32x4# * int64x2#)
  = "" "ret_mixed_pair_native"

let () =
  let #(a, b) = ret_mixed_pair () in
  print_f32x4 "mixed_pair.a" a;
  print_i64x2 "mixed_pair.b" b

(* Vector arguments (in v0 and v1) combined with a vector-pair return (in q0
   and q1). *)
external swap_f32x4 : float32x4# -> float32x4# -> #(float32x4# * float32x4#)
  = "" "swap_f32x4_native"

let () =
  let x = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let y = make_f32x4 #5.0 #6.0 #7.0 #8.0 in
  let #(a, b) = swap_f32x4 x y in
  print_f32x4 "swap.a" a;
  print_f32x4 "swap.b" b

(* [@@noalloc] version, which uses the direct call path rather than going
   via [caml_c_call]. *)
external ret_f32x4_pair_noalloc : unit -> #(float32x4# * float32x4#)
  = "" "ret_f32x4_pair_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_f32x4_pair_noalloc () in
  print_f32x4 "f32x4_pair_noalloc.a" a;
  print_f32x4 "f32x4_pair_noalloc.b" b

(* An unboxed product *argument* of two vectors: a homogeneous short-vector
   aggregate passed in v0 and v1, with the (vector) result in q0. On x86-64
   the corresponding C struct argument would be passed in memory. *)
external add_f32x4_pairwise : #(float32x4# * float32x4#) -> float32x4#
  = "" "add_f32x4_pairwise_native"

let () =
  let x = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let y = make_f32x4 #10.0 #20.0 #30.0 #40.0 in
  print_f32x4 "add_pairwise" (add_f32x4_pairwise #(x, y))

(* Homogeneous floating-point aggregates of more than two float64s: their C
   structs exceed 16 bytes, so on x86-64 they would be passed and returned in
   memory, but on arm64 they use d0-d3. *)
external ret_f64_triple : unit -> #(float# * float# * float#)
  = "" "ret_f64_triple_native"

let () =
  let #(a, b, c) = ret_f64_triple () in
  Printf.eprintf "f64_triple: %.4f %.4f %.4f\n%!" (box_float a) (box_float b)
    (box_float c)

external ret_f64_quad : unit -> #(float# * float# * float# * float#)
  = "" "ret_f64_quad_native"

let () =
  let #(a, b, c, d) = ret_f64_quad () in
  Printf.eprintf "f64_quad: %.4f %.4f %.4f %.4f\n%!" (box_float a)
    (box_float b) (box_float c) (box_float d)

external sum_f64_quad : #(float# * float# * float# * float#) -> float#
  = "" "sum_f64_quad_native"

let () =
  Printf.eprintf "sum_f64_quad: %.4f\n%!"
    (box_float (sum_f64_quad #(#1.0, #2.0, #3.0, #4.0)))
