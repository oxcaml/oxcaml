(* TEST
 modules = "memory_stubs.c";
 reference = "${test_source_directory}/unboxed_args_memory.reference";
 include stdlib_stable;
 flambda2;
 arch_amd64;
 {
   flags = "-extension layouts_beta";
   native;
 }
 {
   flags = "-extension layouts_beta -flambda2-reaper -reaper-local-fields";
   native;
 }
*)

(* Test of unboxed product arguments to external calls whose C structs are
   over 16 bytes. The x86-64 psABI classifies them as MEMORY and passes them
   by value on the stack. The stubs in memory_stubs.c take such structs,
   checking interoperability with the C compiler.

   Restricted to x86-64, since arm64 passes these structs by reference, which
   is unsupported. *)

open Stdlib_stable

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external unbox_int32 : (int32[@local_opt]) -> int32_u = "%unbox_int32"
external unbox_float32 : (float32[@local_opt]) -> float32_u = "%unbox_float32"
external untag_int16 : int16 -> int16# = "%untag_int16"
external untag_int8 : int8 -> int8# = "%untag_int8"

let i32 x = unbox_int32 x
let f32 x = unbox_float32 (Float32.of_float x)
let i16 x = untag_int16 (Int16.of_int x)
let i8 x = untag_int8 (Int8.of_int x)

(* Three int64s: 24 bytes. *)
external i64x3 : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "i64x3_native"

let () =
  Printf.eprintf "i64x3: %Ld\n%!" (box_int64 (i64x3 #(#1L, #2L, #3L)))

(* The same with [@@noalloc]. *)
external i64x3_noalloc : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "i64x3_native"
  [@@noalloc]

let () =
  Printf.eprintf "i64x3_noalloc: %Ld\n%!"
    (box_int64 (i64x3_noalloc #(#1L, #2L, #3L)))

(* Five floats: 40 bytes. *)
external f64x5 : #(float# * float# * float# * float# * float#) -> float#
  = "" "f64x5_native"

let () =
  Printf.eprintf "f64x5: %.4f\n%!"
    (box_float (f64x5 #(#1.0, #2.0, #3.0, #4.0, #5.0)))

(* The struct goes on the stack although all registers are free; the scalars
   after it take the first registers. *)
external i64x3_i64_f64 :
  #(int64_u * int64_u * int64_u) -> int64_u -> float# -> float#
  = "" "i64x3_i64_f64_native"

let () =
  Printf.eprintf "i64x3_i64_f64: %.4f\n%!"
    (box_float (i64x3_i64_f64 #(#1L, #2L, #3L) #4L #0.5))

(* Mixed members: 24 bytes. *)
external mixed : #(int32_u * float32_u * int64_u * float32_u) -> float#
  = "" "mixed_native"

let () =
  Printf.eprintf "mixed: %.4f\n%!"
    (box_float (mixed #(i32 1l, f32 0.5, #3L, f32 0.25)))

(* Padding between and after members: 24 bytes. *)
external padded : #(int16# * int64_u * int8#) -> int64_u
  = "" "padded_native"

let () =
  Printf.eprintf "padded: %Ld\n%!"
    (box_int64 (padded #(i16 (-1), #2L, i8 3)))

(* Two structs on the stack after the seventh int64, then an int64 after
   them. *)
external i64x7_i64x3_i64x3_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * int64_u * int64_u) -> #(int64_u * int64_u * int64_u)
  -> int64_u -> int64_u
  = "" "i64x7_i64x3_i64x3_i64_native"

let () =
  Printf.eprintf "i64x7_i64x3_i64x3_i64: %Ld\n%!"
    (box_int64
       (i64x7_i64x3_i64x3_i64 #1L #2L #3L #4L #5L #6L #7L #(#8L, #9L, #10L)
          #(#11L, #12L, #13L) #14L))

(* Nested products: the inner product is a nested C struct. *)
external nested_i64x3 : #(#(int64_u * int64_u) * int64_u) -> int64_u
  = "" "nested_i64x3_native"

let () =
  Printf.eprintf "nested_i64x3: %Ld\n%!"
    (box_int64 (nested_i64x3 #(#(#1L, #2L), #3L)))

(* The inner struct is padded to 16 bytes, so the outer int8 starts a third
   eightbyte. *)
external nested_tail_padded : #(#(int64_u * int8#) * int8#) -> int64_u
  = "" "nested_tail_padded_native"

let () =
  Printf.eprintf "nested_tail_padded: %Ld\n%!"
    (box_int64 (nested_tail_padded #(#(#1L, i8 2), i8 3)))

(* 20 bytes: the third slot holds one float32. *)
external f32x5 :
  #(float32_u * float32_u * float32_u * float32_u * float32_u) -> float#
  = "" "f32x5_native"

let () =
  Printf.eprintf "f32x5: %.4f\n%!"
    (box_float (f32x5 #(f32 1.0, f32 2.0, f32 3.0, f32 4.0, f32 (-5.0))))
