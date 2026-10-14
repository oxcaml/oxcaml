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
   flags = "-extension layouts_beta -flambda2-reaper -reaper-local-fields";
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

   No case in this file involves a C struct that either x86-64 (System V) or
   arm64 (AAPCS64 and the Apple variant) passes in memory. Single-component
   products, which are passed like their member, may spill to the stack. *)

open Stdlib_stable

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32_u -> (int32[@local_opt]) = "%box_int32"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_float32 : float32_u -> (float32[@local_opt]) = "%box_float32"
external unbox_int32 : (int32[@local_opt]) -> int32_u = "%unbox_int32"
external unbox_float32 : (float32[@local_opt]) -> float32_u = "%unbox_float32"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external untag_int16 : int16 -> int16# = "%untag_int16"
external tag_int16 : int16# -> int16 = "%tag_int16"
external untag_int8 : int8 -> int8# = "%untag_int8"
external tag_int8 : int8# -> int8 = "%tag_int8"

let i32 x = unbox_int32 x
let f32 x = unbox_float32 (Float32.of_float x)
let i16 x = untag_int16 (Int16.of_int x)
let i16' x = Int16.to_int (tag_int16 x)
let i8 x = untag_int8 (Int8.of_int x)
let i8' x = Int8.to_int (tag_int8 x)
let f32' x = float_of_float32 (box_float32 x)

(* Two int64s: two integer registers on both ABIs. *)
external sum_i64_pair : #(int64_u * int64_u) -> int64_u
  = "sum_i64_pair_bytecode" "sum_i64_pair_native"

let () =
  Printf.eprintf "sum_i64_pair: %Ld\n%!"
    (box_int64 (sum_i64_pair #(#111L, #222L)))

(* Two int32s: packed into a single integer register on both ABIs. *)
external sum_i32_pair : #(int32_u * int32_u) -> int64_u
  = "sum_i32_pair_bytecode" "sum_i32_pair_native"

let () =
  Printf.eprintf "sum_i32_pair: %Ld\n%!"
    (box_int64 (sum_i32_pair #(i32 (-123456l), i32 987654l)))

(* int16 then int32: packed with padding into a single register. *)
external sum_i16_i32 : #(int16# * int32_u) -> int64_u
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
external sum_f32_quad : #(float32_u * float32_u * float32_u * float32_u) -> float#
  = "sum_f32_quad_bytecode" "sum_f32_quad_native"

let () =
  Printf.eprintf "sum_f32_quad: %.4f\n%!"
    (box_float (sum_f32_quad #(f32 1.0, f32 2.5, f32 (-0.5), f32 8.0)))

(* float32 and int32 packed into a single 8-byte piece, which both ABIs pass
   in an integer register. *)
external sum_f32_i32 : #(float32_u * int32_u) -> float#
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
external sum_f64_i64 : #(float# * int64_u) -> float#
  = "sum_f64_i64_bytecode" "sum_f64_i64_native"

let () =
  Printf.eprintf "sum_f64_i64: %.4f\n%!"
    (box_float (sum_f64_i64 #(#2.5, #100L)))

(* A nested product, corresponding to a nested C struct: the inner pair of
   int32s is packed into the first register, the int64 goes in the second. *)
external sum_nested : #(#(int32_u * int32_u) * int64_u) -> int64_u
  = "sum_nested_bytecode" "sum_nested_native"

let () =
  Printf.eprintf "sum_nested: %Ld\n%!"
    (box_int64 (sum_nested #(#(i32 7l, i32 (-9l)), #1000L)))

(* A nested product whose C struct layout differs from that of the flattened
   product. The inner struct { int32_t; int16_t } is 6 bytes rounded up to 8
   (its alignment), so the trailing int16 is at byte offset 8 and the struct
   occupies 12 bytes: two integer registers. The flattened struct { int32_t;
   int16_t; int16_t } would occupy 8 bytes and a single register. This checks
   that the nesting of unboxed products is preserved in the Flambda 2 arities
   all the way to the Cmm translation. *)
external sum_nested_padded : #(#(int32_u * int16#) * int16#) -> int64_u
  = "sum_nested_padded_bytecode" "sum_nested_padded_native"

let () =
  Printf.eprintf "sum_nested_padded: %Ld\n%!"
    (box_int64 (sum_nested_padded #(#(i32 100000l, i16 (-300)), i16 7)))

(* Multiple unboxed product arguments. *)
external sum_two_pairs : #(int64_u * int64_u) -> #(int64_u * int64_u) -> int64_u
  = "sum_two_pairs_bytecode" "sum_two_pairs_native"

let () =
  Printf.eprintf "sum_two_pairs: %Ld\n%!"
    (box_int64 (sum_two_pairs #(#1L, #2L) #(#3L, #4L)))

(* Three pairs need six integer registers: exactly the number available for
   arguments on x86-64. *)
external sum_three_pairs :
  #(int64_u * int64_u) -> #(int64_u * int64_u) -> #(int64_u * int64_u) -> int64_u
  = "sum_three_pairs_bytecode" "sum_three_pairs_native"

let () =
  Printf.eprintf "sum_three_pairs: %Ld\n%!"
    (box_int64 (sum_three_pairs #(#1L, #2L) #(#3L, #4L) #(#5L, #6L)))

(* Products interleaved with scalar arguments. *)
external mixed_args :
  int64_u -> #(float# * float#) -> int -> #(int32_u * int32_u) -> float#
  -> int64_u
  = "mixed_args_bytecode" "mixed_args_native"

let () =
  Printf.eprintf "mixed_args: %Ld\n%!"
    (box_int64
       (mixed_args #7L #(#0.5, #0.25) 3 #(i32 10l, i32 20l) #4.0))

(* A product argument with a void component, which is erased in native code:
   the C stub just takes an int64_t. *)
external sum_i64_void : #(int64_u * unit#) -> int64_u
  = "sum_i64_void_bytecode" "sum_i64_void_native"

let () =
  Printf.eprintf "sum_i64_void: %Ld\n%!"
    (box_int64 (sum_i64_void #(#123L, #())))

(* A product with a single non-void component, passed on the stack: eight
   int64 arguments use up the integer argument registers on both ABIs, so [x],
   the product and [y] are all passed in memory. For register assignment a
   one-member struct behaves like its member, but on Apple arm64 a struct
   passed on the stack always occupies an 8-byte slot aligned to 8, whereas a
   bare int16_t occupies 2 bytes; see [To_cmm_extcall]. *)
external stack_single :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> int32_u -> #(int16# * unit#) -> int32_u -> int64_u
  = "stack_single_bytecode" "stack_single_native"

let () =
  Printf.eprintf "stack_single: %Ld\n%!"
    (box_int64
       (stack_single #1L #2L #3L #4L #5L #6L #7L #8L (i32 100l)
          #(i16 (-5), #()) (i32 1000l)))

(* A three-component return of 16 bytes: the two int32s come back packed in
   the first result register, the int64 in the second. *)
external ret_i32i32_i64 : unit -> #(int32_u * int32_u * int64_u)
  = "ret_i32i32_i64_bytecode" "ret_i32i32_i64_native"

let () =
  let #(a, b, c) = ret_i32i32_i64 () in
  Printf.eprintf "ret_i32i32_i64: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A four-component return: on x86-64, two SSE registers each holding two
   packed float32s; on arm64, a homogeneous aggregate returned in s0-s3. *)
external ret_f32_quad : unit -> #(float32_u * float32_u * float32_u * float32_u)
  = "ret_f32_quad_bytecode" "ret_f32_quad_native"

let () =
  let #(a, b, c, d) = ret_f32_quad () in
  Printf.eprintf "ret_f32_quad: %.4f %.4f %.4f %.4f\n%!" (f32' a) (f32' b)
    (f32' c) (f32' d)

(* A nested product return. *)
external ret_nested : unit -> #(#(int32_u * int32_u) * int64_u)
  = "ret_nested_bytecode" "ret_nested_native"

let () =
  let #(#(a, b), c) = ret_nested () in
  Printf.eprintf "ret_nested: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A nested product return whose C struct layout differs from that of the
   flattened product; see [sum_nested_padded] above. The 12-byte struct is
   returned in two integer registers, the trailing int16 in the second. *)
external ret_nested_padded : unit -> #(#(int32_u * int16#) * int16#)
  = "ret_nested_padded_bytecode" "ret_nested_padded_native"

let () =
  let #(#(a, b), c) = ret_nested_padded () in
  Printf.eprintf "ret_nested_padded: %ld %d %d\n%!" (box_int32 a) (i16' b)
    (i16' c)

(* A packed product argument together with a packed product return. *)
external swap_mixed : #(int32_u * float32_u) -> #(float32_u * int32_u)
  = "swap_mixed_bytecode" "swap_mixed_native"

let () =
  let #(a, b) = swap_mixed #(i32 5l, f32 (-2.5)) in
  Printf.eprintf "swap_mixed: %.4f %ld\n%!" (f32' a) (box_int32 b)

(* Four int16s in one register. *)
external sum_i16_quad : #(int16# * int16# * int16# * int16#) -> int64_u
  = "sum_i16_quad_bytecode" "sum_i16_quad_native"

let () =
  Printf.eprintf "sum_i16_quad: %Ld\n%!"
    (box_int64 (sum_i16_quad #(i16 (-1), i16 2, i16 (-3000), i16 4000)))

(* Components of three different widths in one register. *)
external sum_i8_i8_i16_i32 : #(int8# * int8# * int16# * int32_u) -> int64_u
  = "sum_i8_i8_i16_i32_bytecode" "sum_i8_i8_i16_i32_native"

let () =
  Printf.eprintf "sum_i8_i8_i16_i32: %Ld\n%!"
    (box_int64
       (sum_i8_i8_i16_i32 #(i8 (-100), i8 50, i16 (-20000), i32 1000000l)))

(* float32 then int64: an SSE eightbyte of its own on x86-64; on arm64 the
   lone float32 travels in an integer register. *)
external sum_f32_i64 : #(float32_u * int64_u) -> float#
  = "sum_f32_i64_bytecode" "sum_f32_i64_native"

let () =
  Printf.eprintf "sum_f32_i64: %.4f\n%!"
    (box_float (sum_f32_i64 #(f32 2.5, #10L)))

(* Three float32s: on x86-64, two packed in xmm0 and one alone in xmm1; on
   arm64, a homogeneous aggregate in s0-s2. *)
external sum_f32_triple : #(float32_u * float32_u * float32_u) -> float#
  = "sum_f32_triple_bytecode" "sum_f32_triple_native"

let () =
  Printf.eprintf "sum_f32_triple: %.4f\n%!"
    (box_float (sum_f32_triple #(f32 1.5, f32 (-0.25), f32 4.0)))

(* A heap pointer as a product component. *)
external sum_string_i64 : #(string * int64_u) -> int64_u
  = "sum_string_i64_bytecode" "sum_string_i64_native"

let () =
  Printf.eprintf "sum_string_i64: %Ld\n%!"
    (box_int64 (sum_string_i64 #("hello", #100L)))

(* A trailing scalar after three pairs have used all six x86-64 integer
   argument registers. *)
external sum_three_pairs_then_int :
  #(int64_u * int64_u) -> #(int64_u * int64_u) -> #(int64_u * int64_u) -> int
  -> int64_u
  = "sum_three_pairs_then_int_bytecode" "sum_three_pairs_then_int_native"

let () =
  Printf.eprintf "sum_three_pairs_then_int: %Ld\n%!"
    (box_int64
       (sum_three_pairs_then_int #(#1L, #2L) #(#3L, #4L) #(#5L, #6L) 100))

(* A product with a single float32 component passed on the stack, after eight
   float64s have used the floating-point argument registers. On Apple arm64
   such a struct is a homogeneous aggregate and takes the 4-byte slot of its
   member, unlike the 8-byte slot of [stack_single] above. *)
external stack_single_f32 :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32_u * unit#) -> float# -> float#
  = "stack_single_f32_bytecode" "stack_single_f32_native"

let () =
  Printf.eprintf "stack_single_f32: %.4f\n%!"
    (box_float
       (stack_single_f32 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0
          #(f32 0.5, #()) #0.25))

(* [@@noalloc] with a product argument. *)
external sum_i64_pair_noalloc : #(int64_u * int64_u) -> int64_u
  = "sum_i64_pair_bytecode" "sum_i64_pair_native"
  [@@noalloc]

let () =
  Printf.eprintf "sum_i64_pair_noalloc: %Ld\n%!"
    (box_int64 (sum_i64_pair_noalloc #(#5L, #6L)))

(* Returns with more than two components per register. *)
external ret_i16_quad : unit -> #(int16# * int16# * int16# * int16#)
  = "ret_i16_quad_bytecode" "ret_i16_quad_native"

let () =
  let #(a, b, c, d) = ret_i16_quad () in
  Printf.eprintf "ret_i16_quad: %d %d %d %d\n%!" (i16' a) (i16' b) (i16' c)
    (i16' d)

external ret_i8_i8_i16_i32 : unit -> #(int8# * int8# * int16# * int32_u)
  = "ret_i8_i8_i16_i32_bytecode" "ret_i8_i8_i16_i32_native"

let () =
  let #(a, b, c, d) = ret_i8_i8_i16_i32 () in
  Printf.eprintf "ret_i8_i8_i16_i32: %d %d %d %ld\n%!" (i8' a) (i8' b) (i16' c)
    (box_int32 d)

(* See [sum_f32_triple]. *)
external ret_f32_triple : unit -> #(float32_u * float32_u * float32_u)
  = "ret_f32_triple_bytecode" "ret_f32_triple_native"

let () =
  let #(a, b, c) = ret_f32_triple () in
  Printf.eprintf "ret_f32_triple: %.4f %.4f %.4f\n%!" (f32' a) (f32' b) (f32' c)

(* A freshly allocated heap pointer returned in a product: it must be treated
   as a GC root, so it is promoted and the minor heap is then overwritten
   before it is used. *)
external ret_string_i64 : int -> #(string * int64_u)
  = "ret_string_i64_bytecode" "ret_string_i64_native"

let () =
  let #(s, n) = ret_string_i64 21 in
  Gc.minor ();
  for _ = 1 to 1_000_000 do
    ignore (Sys.opaque_identity (String.make 5 'x'))
  done;
  Printf.eprintf "ret_string_i64: %s %Ld\n%!" s (box_int64 n)
