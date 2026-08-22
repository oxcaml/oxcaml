(* TEST
 modules = "args_stubs.c";
 reference = "${test_source_directory}/unboxed_args.reference";
 include stdlib_stable;
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }
 {
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* Test of unboxed product arguments to external calls (without the
   [@unpacked] attribute), together with the unboxed product return shapes
   beyond pairs: products of three and four components and nested products.

   In native code an unboxed product argument or return value follows the
   calling convention of the corresponding (possibly nested) C struct; the
   native stubs in args_stubs.c really do take and return C structs by value,
   so this checks interoperability with the C compiler's implementation of
   the ABIs. In bytecode, products are passed and returned as (possibly
   nested) tuples.

   Every case in this file is passed and returned entirely in registers on
   both x86-64 (System V) and arm64 (AAPCS64 and the Apple variant). *)

open Stdlib_stable

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32# -> (int32[@local_opt]) = "%box_int32"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_float32 : float32# -> (float32[@local_opt]) = "%box_float32"
external unbox_int32 : (int32[@local_opt]) -> int32# = "%unbox_int32"
external unbox_float32 : (float32[@local_opt]) -> float32# = "%unbox_float32"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external untag_int16 : int16 -> int16# = "%untag_int16"

let i32 x = unbox_int32 x
let f32 x = unbox_float32 (Float32.of_float x)
let i16 x = untag_int16 (Int16.of_int x)
let f32' x = float_of_float32 (box_float32 x)

(* Two int64s: two integer registers on both ABIs. *)
external sum_i64_pair : #(int64# * int64#) -> int64#
  = "sum_i64_pair_bytecode" "sum_i64_pair_native"

let () =
  Printf.eprintf "sum_i64_pair: %Ld\n%!"
    (box_int64 (sum_i64_pair #(#111L, #222L)))

(* Two int32s: packed into a single integer register on both ABIs. *)
external sum_i32_pair : #(int32# * int32#) -> int64#
  = "sum_i32_pair_bytecode" "sum_i32_pair_native"

let () =
  Printf.eprintf "sum_i32_pair: %Ld\n%!"
    (box_int64 (sum_i32_pair #(i32 (-123456l), i32 987654l)))

(* int16 then int32: packed with padding into a single register. *)
external sum_i16_i32 : #(int16# * int32#) -> int64#
  = "sum_i16_i32_bytecode" "sum_i16_i32_native"

let () =
  Printf.eprintf "sum_i16_i32: %Ld\n%!"
    (box_int64 (sum_i16_i32 #(i16 (-300), i32 1000000l)))

(* Two float64s: two SSE registers on x86-64; a homogeneous aggregate in d0
   and d1 on arm64. *)
external sum_f64_pair : #(float# * float#) -> float#
  = "sum_f64_pair_bytecode" "sum_f64_pair_native"

let () =
  Printf.eprintf "sum_f64_pair: %.4f\n%!"
    (box_float (sum_f64_pair #(#1.25, #2.5)))

(* Four float32s: on x86-64, two SSE registers each packing two components;
   on arm64, a homogeneous aggregate in s0-s3. *)
external sum_f32_quad : #(float32# * float32# * float32# * float32#) -> float#
  = "sum_f32_quad_bytecode" "sum_f32_quad_native"

let () =
  Printf.eprintf "sum_f32_quad: %.4f\n%!"
    (box_float (sum_f32_quad #(f32 1.0, f32 2.5, f32 (-0.5), f32 8.0)))

(* float32 and int32 packed into a single 8-byte piece, which both ABIs pass
   in an integer register. *)
external sum_f32_i32 : #(float32# * int32#) -> float#
  = "sum_f32_i32_bytecode" "sum_f32_i32_native"

let () =
  Printf.eprintf "sum_f32_i32: %.4f\n%!"
    (box_float (sum_f32_i32 #(f32 (-1.5), i32 10l)))

(* A tagged immediate and a float64: one register of each class. *)
external sum_int_f64 : #(int * float#) -> float#
  = "sum_int_f64_bytecode" "sum_int_f64_native"

let () =
  Printf.eprintf "sum_int_f64: %.4f\n%!"
    (box_float (sum_int_f64 #(41, #0.25)))

(* float64 and int64: on arm64 this is not a homogeneous aggregate, so both
   components go in integer registers. *)
external sum_f64_i64 : #(float# * int64#) -> float#
  = "sum_f64_i64_bytecode" "sum_f64_i64_native"

let () =
  Printf.eprintf "sum_f64_i64: %.4f\n%!"
    (box_float (sum_f64_i64 #(#2.5, #100L)))

(* A nested product, corresponding to a nested C struct: the inner pair of
   int32s is packed into the first register, the int64 goes in the second. *)
external sum_nested : #(#(int32# * int32#) * int64#) -> int64#
  = "sum_nested_bytecode" "sum_nested_native"

let () =
  Printf.eprintf "sum_nested: %Ld\n%!"
    (box_int64 (sum_nested #(#(i32 7l, i32 (-9l)), #1000L)))

(* Multiple unboxed product arguments. *)
external sum_two_pairs : #(int64# * int64#) -> #(int64# * int64#) -> int64#
  = "sum_two_pairs_bytecode" "sum_two_pairs_native"

let () =
  Printf.eprintf "sum_two_pairs: %Ld\n%!"
    (box_int64 (sum_two_pairs #(#1L, #2L) #(#3L, #4L)))

(* Three pairs need six integer registers: exactly the number available for
   arguments on x86-64. *)
external sum_three_pairs :
  #(int64# * int64#) -> #(int64# * int64#) -> #(int64# * int64#) -> int64#
  = "sum_three_pairs_bytecode" "sum_three_pairs_native"

let () =
  Printf.eprintf "sum_three_pairs: %Ld\n%!"
    (box_int64 (sum_three_pairs #(#1L, #2L) #(#3L, #4L) #(#5L, #6L)))

(* Products interleaved with scalar arguments. *)
external mixed_args :
  int64# -> #(float# * float#) -> int -> #(int32# * int32#) -> float#
  -> int64#
  = "mixed_args_bytecode" "mixed_args_native"

let () =
  Printf.eprintf "mixed_args: %Ld\n%!"
    (box_int64
       (mixed_args #7L #(#0.5, #0.25) 3 #(i32 10l, i32 20l) #4.0))

(* A product argument with a void component, which is erased in native code:
   the C stub just takes an int64_t. *)
external sum_i64_void : #(int64# * unit#) -> int64#
  = "sum_i64_void_bytecode" "sum_i64_void_native"

let () =
  Printf.eprintf "sum_i64_void: %Ld\n%!"
    (box_int64 (sum_i64_void #(#123L, #())))

(* A three-component return of 16 bytes: the two int32s come back packed in
   the first result register, the int64 in the second. *)
external ret_i32i32_i64 : unit -> #(int32# * int32# * int64#)
  = "ret_i32i32_i64_bytecode" "ret_i32i32_i64_native"

let () =
  let #(a, b, c) = ret_i32i32_i64 () in
  Printf.eprintf "ret_i32i32_i64: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A four-component return: on x86-64, two SSE registers each holding two
   packed float32s; on arm64, a homogeneous aggregate returned in s0-s3. *)
external ret_f32_quad : unit -> #(float32# * float32# * float32# * float32#)
  = "ret_f32_quad_bytecode" "ret_f32_quad_native"

let () =
  let #(a, b, c, d) = ret_f32_quad () in
  Printf.eprintf "ret_f32_quad: %.4f %.4f %.4f %.4f\n%!" (f32' a) (f32' b)
    (f32' c) (f32' d)

(* A nested product return. *)
external ret_nested : unit -> #(#(int32# * int32#) * int64#)
  = "ret_nested_bytecode" "ret_nested_native"

let () =
  let #(#(a, b), c) = ret_nested () in
  Printf.eprintf "ret_nested: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A packed product argument together with a packed product return. *)
external swap_mixed : #(int32# * float32#) -> #(float32# * int32#)
  = "swap_mixed_bytecode" "swap_mixed_native"

let () =
  let #(a, b) = swap_mixed #(i32 5l, f32 (-2.5)) in
  Printf.eprintf "swap_mixed: %.4f %ld\n%!" (f32' a) (box_int32 b)
