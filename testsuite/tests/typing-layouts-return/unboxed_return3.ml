(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/unboxed_return3.reference";
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

(* Test of unboxed product returns from external calls, across the matrix of
   base kinds. In native code an unboxed product return follows the calling
   convention of the corresponding C struct; the native stubs in stubs.c
   really do return C structs by value, so this checks interoperability with
   the C compiler's implementation of the ABIs.

   Every case in this file is returned entirely in registers on both x86-64
   (System V) and arm64 (AAPCS64 and the Apple variant). The cases cover, on
   at least one of those ABIs: multiple components packed into a single
   register (with padding, with sign extension of small integers, and with
   float bit patterns moving through integer registers); homogeneous
   floating-point aggregates; and mixed integer/floating-point structs using
   one register of each class. *)

open Stdlib_stable

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32# -> (int32[@local_opt]) = "%box_int32"
external box_nativeint : nativeint# -> (nativeint[@local_opt])
  = "%box_nativeint"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_float32 : float32# -> (float32[@local_opt]) = "%box_float32"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external tag_int8 : int8# -> int8 = "%tag_int8"
external tag_int16 : int16# -> int16 = "%tag_int16"

let i8 x = Int8.to_int (tag_int8 x)
let i16 x = Int16.to_int (tag_int16 x)
let f32 x = float_of_float32 (box_float32 x)

(* Two int32s: packed into a single integer register on both ABIs
   (rax; x0). The negative component checks sign extension after
   unpacking. *)
external ret_i32_i32 : unit -> #(int32# * int32#)
  = "ret_i32_i32_bytecode" "ret_i32_i32_native"

let () =
  let #(a, b) = ret_i32_i32 () in
  Printf.eprintf "i32_i32: %ld %ld\n%!" (box_int32 a) (box_int32 b)

(* Two int16s: packed at byte offsets 0 and 2 of a single integer
   register. *)
external ret_i16_i16 : unit -> #(int16# * int16#)
  = "ret_i16_i16_bytecode" "ret_i16_i16_native"

let () =
  let #(a, b) = ret_i16_i16 () in
  Printf.eprintf "i16_i16: %d %d\n%!" (i16 a) (i16 b)

(* Two int8s: packed at byte offsets 0 and 1. *)
external ret_i8_i8 : unit -> #(int8# * int8#)
  = "ret_i8_i8_bytecode" "ret_i8_i8_native"

let () =
  let #(a, b) = ret_i8_i8 () in
  Printf.eprintf "i8_i8: %d %d\n%!" (i8 a) (i8 b)

(* int8 then int16: the int16 lies at byte offset 2, with padding at byte
   offset 1. *)
external ret_i8_i16 : unit -> #(int8# * int16#)
  = "ret_i8_i16_bytecode" "ret_i8_i16_native"

let () =
  let #(a, b) = ret_i8_i16 () in
  Printf.eprintf "i8_i16: %d %d\n%!" (i8 a) (i16 b)

(* int16 then int32: the int32 lies at byte offset 4, with padding. *)
external ret_i16_i32 : unit -> #(int16# * int32#)
  = "ret_i16_i32_bytecode" "ret_i16_i32_native"

let () =
  let #(a, b) = ret_i16_i32 () in
  Printf.eprintf "i16_i32: %d %ld\n%!" (i16 a) (box_int32 b)

(* int8 then int32. *)
external ret_i8_i32 : unit -> #(int8# * int32#)
  = "ret_i8_i32_bytecode" "ret_i8_i32_native"

let () =
  let #(a, b) = ret_i8_i32 () in
  Printf.eprintf "i8_i32: %d %ld\n%!" (i8 a) (box_int32 b)

(* float32 and int32 packed into a single 8-byte piece: since the piece is
   not purely floating-point, both ABIs return it in an integer register,
   so the float32's bit pattern moves through an integer register. *)
external ret_f32_i32 : unit -> #(float32# * int32#)
  = "ret_f32_i32_bytecode" "ret_f32_i32_native"

let () =
  let #(a, b) = ret_f32_i32 () in
  Printf.eprintf "f32_i32: %.4f %ld\n%!" (f32 a) (box_int32 b)

external ret_i32_f32 : unit -> #(int32# * float32#)
  = "ret_i32_f32_bytecode" "ret_i32_f32_native"

let () =
  let #(a, b) = ret_i32_f32 () in
  Printf.eprintf "i32_f32: %ld %.4f\n%!" (box_int32 a) (f32 b)

(* int16 and float32 in a single 8-byte piece, with padding. *)
external ret_i16_f32 : unit -> #(int16# * float32#)
  = "ret_i16_f32_bytecode" "ret_i16_f32_native"

let () =
  let #(a, b) = ret_i16_f32 () in
  Printf.eprintf "i16_f32: %d %.4f\n%!" (i16 a) (f32 b)

(* Two float32s: on x86-64 both are packed into the bottom half of xmm0; on
   arm64 this is a homogeneous aggregate returned in s0 and s1. *)
external ret_f32_f32 : unit -> #(float32# * float32#)
  = "ret_f32_f32_bytecode" "ret_f32_f32_native"

let () =
  let #(a, b) = ret_f32_f32 () in
  Printf.eprintf "f32_f32: %.4f %.4f\n%!" (f32 a) (f32 b)

(* float32 then float64: two 8-byte pieces. On x86-64 these are two SSE
   pieces (xmm0, xmm1); on arm64 the struct is not a homogeneous aggregate,
   so both components come back in integer registers (x0, x1). *)
external ret_f32_f64 : unit -> #(float32# * float#)
  = "ret_f32_f64_bytecode" "ret_f32_f64_native"

let () =
  let #(a, b) = ret_f32_f64 () in
  Printf.eprintf "f32_f64: %.4f %.4f\n%!" (f32 a) (box_float b)

external ret_f64_f32 : unit -> #(float# * float32#)
  = "ret_f64_f32_bytecode" "ret_f64_f32_native"

let () =
  let #(a, b) = ret_f64_f32 () in
  Printf.eprintf "f64_f32: %.4f %.4f\n%!" (box_float a) (f32 b)

(* int32 then float64: an integer piece then a floating-point piece. On
   x86-64 these use rax and xmm0; on arm64, x0 and x1. *)
external ret_i32_f64 : unit -> #(int32# * float#)
  = "ret_i32_f64_bytecode" "ret_i32_f64_native"

let () =
  let #(a, b) = ret_i32_f64 () in
  Printf.eprintf "i32_f64: %ld %.4f\n%!" (box_int32 a) (box_float b)

external ret_f64_i32 : unit -> #(float# * int32#)
  = "ret_f64_i32_bytecode" "ret_f64_i32_native"

let () =
  let #(a, b) = ret_f64_i32 () in
  Printf.eprintf "f64_i32: %.4f %ld\n%!" (box_float a) (box_int32 b)

(* Tagged immediates mixed with floats. *)
external ret_int_f64 : unit -> #(int * float#)
  = "ret_int_f64_bytecode" "ret_int_f64_native"

let () =
  let #(a, b) = ret_int_f64 () in
  Printf.eprintf "int_f64: %d %.4f\n%!" a (box_float b)

external ret_f64_int : unit -> #(float# * int)
  = "ret_f64_int_bytecode" "ret_f64_int_native"

let () =
  let #(a, b) = ret_f64_int () in
  Printf.eprintf "f64_int: %.4f %d\n%!" (box_float a) b

(* Two tagged immediates. *)
external ret_int_int : unit -> #(int * int)
  = "ret_int_int_bytecode" "ret_int_int_native"

let () =
  let #(a, b) = ret_int_int () in
  Printf.eprintf "int_int: %d %d\n%!" a b

(* nativeint and int32. *)
external ret_n_i32 : unit -> #(nativeint# * int32#)
  = "ret_n_i32_bytecode" "ret_n_i32_native"

let () =
  let #(a, b) = ret_n_i32 () in
  Printf.eprintf "n_i32: %nd %ld\n%!" (box_nativeint a) (box_int32 b)

(* Products involving void: the void component is erased in native code, so
   the C stubs return plain scalars. *)
external ret_i64_void : unit -> #(int64# * unit#)
  = "ret_i64_void_bytecode" "ret_i64_void_native"

let () =
  let #(a, #()) = ret_i64_void () in
  Printf.eprintf "i64_void: %Ld\n%!" (box_int64 a)

external ret_void_f64 : unit -> #(unit# * float#)
  = "ret_void_f64_bytecode" "ret_void_f64_native"

let () =
  let #(#(), b) = ret_void_f64 () in
  Printf.eprintf "void_f64: %.4f\n%!" (box_float b)

(* [@@noalloc] versions, which use the direct call path in native code
   rather than going via [caml_c_call]. *)
external ret_i32_i32_noalloc : unit -> #(int32# * int32#)
  = "ret_i32_i32_bytecode" "ret_i32_i32_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_i32_i32_noalloc () in
  Printf.eprintf "i32_i32_noalloc: %ld %ld\n%!" (box_int32 a) (box_int32 b)

external ret_f32_f32_noalloc : unit -> #(float32# * float32#)
  = "ret_f32_f32_bytecode" "ret_f32_f32_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_f32_f32_noalloc () in
  Printf.eprintf "f32_f32_noalloc: %.4f %.4f\n%!" (f32 a) (f32 b)

(* Ten scalar arguments (enough to spill onto the stack on both ABIs)
   combined with an unboxed product return. *)
external combine10 :
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  -> #(int32# * float32#)
  = "combine10_bytecode" "combine10_native"

let () =
  let #(a, b) = combine10 1 2 3 4 5 6 7 8 9 10 in
  Printf.eprintf "combine10: %ld %.4f\n%!" (box_int32 a) (f32 b)

(* An [@unpacked] product argument (whose components are passed as separate
   C arguments) combined with an unboxed product return (which follows the C
   struct convention). *)
external swap_f64_i64 : (#(float# * int64#) [@unpacked]) -> #(int64# * float#)
  = "swap_f64_i64_bytecode" "swap_f64_i64_native"

let () =
  let #(a, b) = swap_f64_i64 #(#3.25, #111L) in
  Printf.eprintf "swap_f64_i64: %Ld %.4f\n%!" (box_int64 a) (box_float b)
