(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/unboxed_return.reference";
 include stdlib_stable;
 flambda2;
 {
   flags = "-bin-annot-cms -extension layouts_beta";
   native;
 }
 {
   flags = "-bin-annot-cms -extension layouts_beta -flambda2-reaper -reaper-local-fields";
   native;
 }
 {
   flags = "-bin-annot-cms -extension layouts_beta";
   bytecode;
 }
*)

(* Test of unboxed product returns from external calls, across the matrix of
   base kinds and beyond pairs. The native stubs in stubs.c return C structs by
   value, checking interoperability with the C compiler. Every case is returned
   entirely in registers on both x86-64 and arm64. *)

open Stdlib_stable

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32_u -> (int32[@local_opt]) = "%box_int32"
external box_nativeint : nativeint_u -> (nativeint[@local_opt])
  = "%box_nativeint"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_float32 : float32_u -> (float32[@local_opt]) = "%box_float32"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external tag_int8 : int8# -> int8 = "%tag_int8"
external tag_int16 : int16# -> int16 = "%tag_int16"

let i8 x = Int8.to_int (tag_int8 x)
let i16 x = Int16.to_int (tag_int16 x)
let f32 x = float_of_float32 (box_float32 x)

(* Pairs of 64-bit components: one register of each component's class. *)
external ret_i64_i64 : unit -> #(int64_u * int64_u)
  = "ui64_ui64_make_bytecode" "ui64_ui64_make"

let () =
  let #(a, b) = ret_i64_i64 () in
  Printf.eprintf "i64_i64: %Ld %Ld\n%!" (box_int64 a) (box_int64 b)

external ret_f64_f64 : unit -> #(float# * float#)
  = "f64_f64_make_bytecode" "f64_f64_make"

let () =
  let #(a, b) = ret_f64_f64 () in
  Printf.eprintf "f64_f64: %f %f\n%!" (box_float a) (box_float b)

external ret_i64_f64 : unit -> #(int64_u * float#)
  = "ui64_f64_make_bytecode" "ui64_f64_make"

let () =
  let #(a, b) = ret_i64_f64 () in
  Printf.eprintf "i64_f64: %Ld %f\n%!" (box_int64 a) (box_float b)

external ret_f64_i64 : unit -> #(float# * int64_u)
  = "f64_ui64_make_bytecode" "f64_ui64_make"

let () =
  let #(a, b) = ret_f64_i64 () in
  Printf.eprintf "f64_i64: %f %Ld\n%!" (box_float a) (box_int64 b)

(* Two int32s: packed into a single integer register on both ABIs
   (rax; x0). The negative component checks sign extension after
   unpacking. *)
external ret_i32_i32 : unit -> #(int32_u * int32_u)
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
external ret_i16_i32 : unit -> #(int16# * int32_u)
  = "ret_i16_i32_bytecode" "ret_i16_i32_native"

let () =
  let #(a, b) = ret_i16_i32 () in
  Printf.eprintf "i16_i32: %d %ld\n%!" (i16 a) (box_int32 b)

(* int8 then int32. *)
external ret_i8_i32 : unit -> #(int8# * int32_u)
  = "ret_i8_i32_bytecode" "ret_i8_i32_native"

let () =
  let #(a, b) = ret_i8_i32 () in
  Printf.eprintf "i8_i32: %d %ld\n%!" (i8 a) (box_int32 b)

(* float32 and int32 packed into a single 8-byte piece: since the piece is
   not purely floating-point, both ABIs return it in an integer register,
   so the float32's bit pattern moves through an integer register. *)
external ret_f32_i32 : unit -> #(float32_u * int32_u)
  = "ret_f32_i32_bytecode" "ret_f32_i32_native"

let () =
  let #(a, b) = ret_f32_i32 () in
  Printf.eprintf "f32_i32: %.4f %ld\n%!" (f32 a) (box_int32 b)

external ret_i32_f32 : unit -> #(int32_u * float32_u)
  = "ret_i32_f32_bytecode" "ret_i32_f32_native"

let () =
  let #(a, b) = ret_i32_f32 () in
  Printf.eprintf "i32_f32: %ld %.4f\n%!" (box_int32 a) (f32 b)

(* int16 and float32 in a single 8-byte piece, with padding. *)
external ret_i16_f32 : unit -> #(int16# * float32_u)
  = "ret_i16_f32_bytecode" "ret_i16_f32_native"

let () =
  let #(a, b) = ret_i16_f32 () in
  Printf.eprintf "i16_f32: %d %.4f\n%!" (i16 a) (f32 b)

(* Two float32s: on x86-64 both are packed into the bottom half of xmm0; on
   arm64 this is a homogeneous aggregate returned in s0 and s1. *)
external ret_f32_f32 : unit -> #(float32_u * float32_u)
  = "ret_f32_f32_bytecode" "ret_f32_f32_native"

let () =
  let #(a, b) = ret_f32_f32 () in
  Printf.eprintf "f32_f32: %.4f %.4f\n%!" (f32 a) (f32 b)

(* float32 then float64: two 8-byte pieces. On x86-64 these are two SSE
   pieces (xmm0, xmm1); on arm64 the struct is not a homogeneous aggregate,
   so both components come back in integer registers (x0, x1). *)
external ret_f32_f64 : unit -> #(float32_u * float#)
  = "ret_f32_f64_bytecode" "ret_f32_f64_native"

let () =
  let #(a, b) = ret_f32_f64 () in
  Printf.eprintf "f32_f64: %.4f %.4f\n%!" (f32 a) (box_float b)

external ret_f64_f32 : unit -> #(float# * float32_u)
  = "ret_f64_f32_bytecode" "ret_f64_f32_native"

let () =
  let #(a, b) = ret_f64_f32 () in
  Printf.eprintf "f64_f32: %.4f %.4f\n%!" (box_float a) (f32 b)

(* int32 then float64: an integer piece then a floating-point piece. On
   x86-64 these use rax and xmm0; on arm64, x0 and x1. *)
external ret_i32_f64 : unit -> #(int32_u * float#)
  = "ret_i32_f64_bytecode" "ret_i32_f64_native"

let () =
  let #(a, b) = ret_i32_f64 () in
  Printf.eprintf "i32_f64: %ld %.4f\n%!" (box_int32 a) (box_float b)

external ret_f64_i32 : unit -> #(float# * int32_u)
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
external ret_n_i32 : unit -> #(nativeint_u * int32_u)
  = "ret_n_i32_bytecode" "ret_n_i32_native"

let () =
  let #(a, b) = ret_n_i32 () in
  Printf.eprintf "n_i32: %nd %ld\n%!" (box_nativeint a) (box_int32 b)

(* A three-component return of 16 bytes: the two int32s come back packed in
   the first result register, the int64 in the second. *)
external ret_i32i32_i64 : unit -> #(int32_u * int32_u * int64_u)
  = "ret_i32i32_i64_bytecode" "ret_i32i32_i64_native"

let () =
  let #(a, b, c) = ret_i32i32_i64 () in
  Printf.eprintf "i32i32_i64: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A four-component return: on x86-64, two SSE registers each holding two
   packed float32s; on arm64, a homogeneous aggregate returned in s0-s3. *)
external ret_f32_quad : unit -> #(float32_u * float32_u * float32_u * float32_u)
  = "ret_f32_quad_bytecode" "ret_f32_quad_native"

let () =
  let #(a, b, c, d) = ret_f32_quad () in
  Printf.eprintf "f32_quad: %.4f %.4f %.4f %.4f\n%!" (f32 a) (f32 b)
    (f32 c) (f32 d)

(* A nested product return. *)
external ret_nested : unit -> #(#(int32_u * int32_u) * int64_u)
  = "ret_nested_bytecode" "ret_nested_native"

let () =
  let #(#(a, b), c) = ret_nested () in
  Printf.eprintf "nested: %ld %ld %Ld\n%!" (box_int32 a) (box_int32 b)
    (box_int64 c)

(* A nested product whose C struct layout differs from that of the flattened
   product: the inner struct { int32_t; int16_t } is padded to 8 bytes, so the
   whole struct occupies 12 bytes (two registers) rather than 8 (one). *)
external ret_nested_padded : unit -> #(#(int32_u * int16#) * int16#)
  = "ret_nested_padded_bytecode" "ret_nested_padded_native"

let () =
  let #(#(a, b), c) = ret_nested_padded () in
  Printf.eprintf "nested_padded: %ld %d %d\n%!" (box_int32 a) (i16 b)
    (i16 c)

(* Returns with more than two components per register. *)
external ret_i16_quad : unit -> #(int16# * int16# * int16# * int16#)
  = "ret_i16_quad_bytecode" "ret_i16_quad_native"

let () =
  let #(a, b, c, d) = ret_i16_quad () in
  Printf.eprintf "i16_quad: %d %d %d %d\n%!" (i16 a) (i16 b) (i16 c)
    (i16 d)

external ret_i8_i8_i16_i32 : unit -> #(int8# * int8# * int16# * int32_u)
  = "ret_i8_i8_i16_i32_bytecode" "ret_i8_i8_i16_i32_native"

let () =
  let #(a, b, c, d) = ret_i8_i8_i16_i32 () in
  Printf.eprintf "i8_i8_i16_i32: %d %d %d %ld\n%!" (i8 a) (i8 b) (i16 c)
    (box_int32 d)

(* Three float32s: on x86-64, two packed in xmm0 and one alone in xmm1; on
   arm64, a homogeneous aggregate in s0-s2. *)
external ret_f32_triple : unit -> #(float32_u * float32_u * float32_u)
  = "ret_f32_triple_bytecode" "ret_f32_triple_native"

let () =
  let #(a, b, c) = ret_f32_triple () in
  Printf.eprintf "f32_triple: %.4f %.4f %.4f\n%!" (f32 a) (f32 b) (f32 c)

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
  Printf.eprintf "string_i64: %s %Ld\n%!" s (box_int64 n)

(* Products involving void: the void component is erased in native code, so
   the C stubs return plain scalars. *)
external ret_i64_void : unit -> #(int64_u * unit#)
  = "ret_i64_void_bytecode" "ret_i64_void_native"

let () =
  let #(a, #()) = ret_i64_void () in
  Printf.eprintf "i64_void: %Ld\n%!" (box_int64 a)

external ret_void_f64 : unit -> #(unit# * float#)
  = "ret_void_f64_bytecode" "ret_void_f64_native"

let () =
  let #(#(), b) = ret_void_f64 () in
  Printf.eprintf "void_f64: %.4f\n%!" (box_float b)

(* Voids between, around and nested among other components. *)
external ret_void_mixed : unit -> #(float32_u * unit# * int32_u * unit#)
  = "ret_void_mixed_bytecode" "ret_void_mixed_native"

let () =
  let #(a, #(), b, #()) = ret_void_mixed () in
  Printf.eprintf "void_mixed: %.4f %ld\n%!" (f32 a) (box_int32 b)

external ret_void_nested_f64 :
  unit -> #(#(unit# * float#) * unit# * #(float# * unit#))
  = "ret_void_nested_f64_bytecode" "ret_void_nested_f64_native"

let () =
  let #(#(#(), a), #(), #(b, #())) = ret_void_nested_f64 () in
  Printf.eprintf "void_nested_f64: %.4f %.4f\n%!" (box_float a) (box_float b)

(* A single non-void component nested among voids comes back like that
   component, sign extension included. *)
external ret_void_single_nested : unit -> #(unit# * #(int16# * unit#))
  = "ret_void_single_nested_bytecode" "ret_void_single_nested_native"

let () =
  let #(#(), #(a, #())) = ret_void_single_nested () in
  Printf.eprintf "void_single_nested: %d\n%!" (i16 a)

(* A product all of whose components are void: no C result. *)
external all_void_return : unit -> #(unit# * unit#)
  = "all_void_return_bytecode" "all_void_return_native"

let () =
  let #(#(), #()) = all_void_return () in
  Printf.eprintf "all_void_return: ok\n%!"

(* [@@noalloc] versions, which use the direct call path in native code
   rather than going via [caml_c_call]. *)
external ret_i32_i32_noalloc : unit -> #(int32_u * int32_u)
  = "ret_i32_i32_bytecode" "ret_i32_i32_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_i32_i32_noalloc () in
  Printf.eprintf "i32_i32_noalloc: %ld %ld\n%!" (box_int32 a) (box_int32 b)

external ret_f32_f32_noalloc : unit -> #(float32_u * float32_u)
  = "ret_f32_f32_bytecode" "ret_f32_f32_native"
  [@@noalloc]

let () =
  let #(a, b) = ret_f32_f32_noalloc () in
  Printf.eprintf "f32_f32_noalloc: %.4f %.4f\n%!" (f32 a) (f32 b)

(* Ten scalar arguments (enough to spill onto the stack on both ABIs)
   combined with an unboxed product return. *)
external combine10 :
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  -> #(int32_u * float32_u)
  = "combine10_bytecode" "combine10_native"

let () =
  let #(a, b) = combine10 1 2 3 4 5 6 7 8 9 10 in
  Printf.eprintf "combine10: %ld %.4f\n%!" (box_int32 a) (f32 b)

(* An [@unpacked] product argument (whose components are passed as separate
   C arguments) combined with an unboxed product return (which follows the C
   struct convention). *)
external swap_f64_i64 : (#(float# * int64_u) [@unpacked]) -> #(int64_u * float#)
  = "swap_f64_i64_bytecode" "swap_f64_i64_native"

let () =
  let #(a, b) = swap_f64_i64 #(#3.25, #111L) in
  Printf.eprintf "swap_f64_i64: %Ld %.4f\n%!" (box_int64 a) (box_float b)

(* 12 bytes: two registers, the second holding one int32. *)
external ret_i32_triple : unit -> #(int32_u * int32_u * int32_u)
  = "ret_i32_triple_bytecode" "ret_i32_triple_native"

let () =
  let #(a, b, c) = ret_i32_triple () in
  Printf.eprintf "i32_triple: %ld %ld %ld\n%!" (box_int32 a) (box_int32 b)
    (box_int32 c)

(* xmm0 and a packed xmm1 on x86-64; x0 and x1 on arm64. *)
external ret_f64_f32_f32 : unit -> #(float# * float32_u * float32_u)
  = "ret_f64_f32_f32_bytecode" "ret_f64_f32_f32_native"

let () =
  let #(a, b, c) = ret_f64_f32_f32 () in
  Printf.eprintf "f64_f32_f32: %.4f %.4f %.4f\n%!" (box_float a) (f32 b)
    (f32 c)

(* Padding at the end. *)
external ret_i64_i8 : unit -> #(int64_u * int8#)
  = "ret_i64_i8_bytecode" "ret_i64_i8_native"

let () =
  let #(a, b) = ret_i64_i8 () in
  Printf.eprintf "i64_i8: %Ld %d\n%!" (box_int64 a) (i8 b)

(* Padding after the int8. *)
external ret_i8_f64 : unit -> #(int8# * float#)
  = "ret_i8_f64_bytecode" "ret_i8_f64_native"

let () =
  let #(a, b) = ret_i8_f64 () in
  Printf.eprintf "i8_f64: %d %.4f\n%!" (i8 a) (box_float b)

(* Padding after the float32. *)
external ret_f32_i64 : unit -> #(float32_u * int64_u)
  = "ret_f32_i64_bytecode" "ret_f32_i64_native"

let () =
  let #(a, b) = ret_f32_i64 () in
  Printf.eprintf "f32_i64: %.4f %Ld\n%!" (f32 a) (box_int64 b)

(* Two packed int32s, then a float64. *)
external ret_i32_i32_f64 : unit -> #(int32_u * int32_u * float#)
  = "ret_i32_i32_f64_bytecode" "ret_i32_i32_f64_native"

let () =
  let #(a, b, c) = ret_i32_i32_f64 () in
  Printf.eprintf "i32_i32_f64: %ld %ld %.4f\n%!" (box_int32 a) (box_int32 b)
    (box_float c)

(* Three levels of nesting. *)
external ret_nested3 : unit -> #(#(#(int8# * int8#) * int16#) * int32_u)
  = "ret_nested3_bytecode" "ret_nested3_native"

let () =
  let #(#(#(a, b), c), d) = ret_nested3 () in
  Printf.eprintf "nested3: %d %d %d %ld\n%!" (i8 a) (i8 b) (i16 c)
    (box_int32 d)

(* Padding before an inner struct more aligned than the member before it. *)
external ret_nested_inner_padded : unit -> #(int8# * #(int32_u * int8#))
  = "ret_nested_inner_padded_bytecode" "ret_nested_inner_padded_native"

let () =
  let #(a, #(b, c)) = ret_nested_inner_padded () in
  Printf.eprintf "nested_inner_padded: %d %ld %d\n%!" (i8 a) (box_int32 b)
    (i8 c)
