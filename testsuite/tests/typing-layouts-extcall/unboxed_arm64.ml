(* TEST
 modules = "vec128_stubs.c";
 reference = "${test_source_directory}/unboxed_arm64.reference";
 flambda2;
 arch_arm64;
 {
   flags = "-extension layouts_beta -extension simd_beta";
   native;
 }
 {
   flags = "-extension layouts_beta -extension simd_beta -flambda2-reaper -reaper-local-fields";
   native;
 }
*)

(* Vectors and homogeneous aggregates on arm64. Not x86-64, which returns
   these in memory. *)

external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"

external make_f32x4 : float# -> float# -> float# -> float# -> float32x4#
  = "" "make_f32x4_native"
external f32x4_lane : float32x4# -> int -> float# = "" "f32x4_lane_native"
external f64x2_lane : float64x2# -> int -> float# = "" "f64x2_lane_native"
external i64x2_lane : int64x2# -> int -> int64_u = "" "i64x2_lane_native"
external make_i64x2 : int64_u -> int64_u -> int64x2# = "" "make_i64x2_native"

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

external ret_f32x4_pair : unit -> #(float32x4# * float32x4#)
  = "" "ret_f32x4_pair_native"

let () =
  let #(a, b) = ret_f32x4_pair () in
  print_f32x4 "f32x4_pair.a" a;
  print_f32x4 "f32x4_pair.b" b

external ret_f64x2_pair : unit -> #(float64x2# * float64x2#)
  = "" "ret_f64x2_pair_native"

let () =
  let #(a, b) = ret_f64x2_pair () in
  print_f64x2 "f64x2_pair.a" a;
  print_f64x2 "f64x2_pair.b" b

(* Integer-element vectors still use SIMD registers. *)
external ret_i64x2_pair : unit -> #(int64x2# * int64x2#)
  = "" "ret_i64x2_pair_native"

let () =
  let #(a, b) = ret_i64x2_pair () in
  print_i64x2 "i64x2_pair.a" a;
  print_i64x2 "i64x2_pair.b" b

(* Mixed element types still form a homogeneous short-vector aggregate. *)
external ret_mixed_pair : unit -> #(float32x4# * int64x2#)
  = "" "ret_mixed_pair_native"

let () =
  let #(a, b) = ret_mixed_pair () in
  print_f32x4 "mixed_pair.a" a;
  print_i64x2 "mixed_pair.b" b

external swap_f32x4 : float32x4# -> float32x4# -> #(float32x4# * float32x4#)
  = "" "swap_f32x4_native"

let () =
  let x = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let y = make_f32x4 #5.0 #6.0 #7.0 #8.0 in
  let #(a, b) = swap_f32x4 x y in
  print_f32x4 "swap.a" a;
  print_f32x4 "swap.b" b

external ret_f32x4_pair_noalloc : unit -> #(float32x4# * float32x4#)
  = "" "ret_f32x4_pair_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_f32x4_pair_noalloc () in
  print_f32x4 "f32x4_pair_noalloc.a" a;
  print_f32x4 "f32x4_pair_noalloc.b" b

external add_f32x4_pairwise : #(float32x4# * float32x4#) -> float32x4#
  = "" "add_f32x4_pairwise_native"

let () =
  let x = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let y = make_f32x4 #10.0 #20.0 #30.0 #40.0 in
  print_f32x4 "add_pairwise" (add_f32x4_pairwise #(x, y))

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

(* Nested products are still homogeneous aggregates. *)
external sum_nested_f64 : #(#(float# * float#) * float#) -> float#
  = "" "sum_nested_f64_native"

let () =
  Printf.eprintf "sum_nested_f64: %.4f\n%!"
    (box_float (sum_nested_f64 #(#(#1.0, #2.0), #3.0)))

external ret_nested_f64 : unit -> #(#(float# * float#) * float#)
  = "" "ret_nested_f64_native"

let () =
  let #(#(a, b), c) = ret_nested_f64 () in
  Printf.eprintf "ret_nested_f64: %.4f %.4f %.4f\n%!" (box_float a)
    (box_float b) (box_float c)

(* The largest aggregate passed in registers: 64 bytes. *)
external ret_f32x4_quad :
  unit -> #(float32x4# * float32x4# * float32x4# * float32x4#)
  = "" "ret_f32x4_quad_native"

let () =
  let #(a, b, c, d) = ret_f32x4_quad () in
  print_f32x4 "f32x4_quad.a" a;
  print_f32x4 "f32x4_quad.b" b;
  print_f32x4 "f32x4_quad.c" c;
  print_f32x4 "f32x4_quad.d" d

external sum_f32x4_quad :
  #(float32x4# * float32x4# * float32x4# * float32x4#) -> float32x4#
  = "" "sum_f32x4_quad_native"

let () =
  let a = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let b = make_f32x4 #10.0 #20.0 #30.0 #40.0 in
  let c = make_f32x4 #100.0 #200.0 #300.0 #400.0 in
  let d = make_f32x4 #1000.0 #2000.0 #3000.0 #4000.0 in
  print_f32x4 "sum_f32x4_quad" (sum_f32x4_quad #(a, b, c, d))

(* One float register left: spills, with the double after it (AAPCS64
   C.3). *)
external f64x7_i64x2_pair_f64 :
  float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(int64x2# * int64x2#) -> float# -> float#
  = "" "f64x7_i64x2_pair_f64_native"

let () =
  let a = make_i64x2 #8L #9L in
  let b = make_i64x2 #10L #11L in
  Printf.eprintf "f64x7_i64x2_pair_f64: %.4f\n%!"
    (box_float
       (f64x7_i64x2_pair_f64 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #(a, b) #12.0))

external f64x8_f32x4_pair_f64 :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32x4# * float32x4#) -> float# -> float#
  = "" "f64x8_f32x4_pair_f64_native"

let () =
  let a = make_f32x4 #1.0 #2.0 #3.0 #4.0 in
  let b = make_f32x4 #5.0 #6.0 #7.0 #8.0 in
  Printf.eprintf "f64x8_f32x4_pair_f64: %.4f\n%!"
    (box_float
       (f64x8_f32x4_pair_f64 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0 #(a, b)
          #9.0))
