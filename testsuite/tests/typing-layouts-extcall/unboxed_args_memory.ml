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

(* Structs over 16 bytes, which x86-64 passes on the stack (class MEMORY).
   Not arm64, which passes them by reference. *)

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

external i64x3 : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "i64x3_native"

let () =
  Printf.eprintf "i64x3: %Ld\n%!" (box_int64 (i64x3 #(#1L, #2L, #3L)))

external i64x3_noalloc : #(int64_u * int64_u * int64_u) -> int64_u
  = "" "i64x3_native"
  [@@noalloc]

let () =
  Printf.eprintf "i64x3_noalloc: %Ld\n%!"
    (box_int64 (i64x3_noalloc #(#1L, #2L, #3L)))

external f64x5 : #(float# * float# * float# * float# * float#) -> float#
  = "" "f64x5_native"

let () =
  Printf.eprintf "f64x5: %.4f\n%!"
    (box_float (f64x5 #(#1.0, #2.0, #3.0, #4.0, #5.0)))

(* The scalars after the struct take the first registers. *)
external i64x3_i64_f64 :
  #(int64_u * int64_u * int64_u) -> int64_u -> float# -> float#
  = "" "i64x3_i64_f64_native"

let () =
  Printf.eprintf "i64x3_i64_f64: %.4f\n%!"
    (box_float (i64x3_i64_f64 #(#1L, #2L, #3L) #4L #0.5))

external mixed : #(int32_u * float32_u * int64_u * float32_u) -> float#
  = "" "mixed_native"

let () =
  Printf.eprintf "mixed: %.4f\n%!"
    (box_float (mixed #(i32 1l, f32 0.5, #3L, f32 0.25)))

external padded : #(int16# * int64_u * int8#) -> int64_u
  = "" "padded_native"

let () =
  Printf.eprintf "padded: %Ld\n%!"
    (box_int64 (padded #(i16 (-1), #2L, i8 3)))

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

external nested_i64x3 : #(#(int64_u * int64_u) * int64_u) -> int64_u
  = "" "nested_i64x3_native"

let () =
  Printf.eprintf "nested_i64x3: %Ld\n%!"
    (box_int64 (nested_i64x3 #(#(#1L, #2L), #3L)))

(* The inner struct is padded to 16 bytes. *)
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

(* 88 bytes, with packed and padded units. *)
type big =
  #(#(int64_u * int64_u * int64_u * int64_u)
   * #(float# * float# * float# * float#)
   * #(int32_u * float32_u * int16# * int8#)
   * int64_u)

external big_pair : int64_u -> big -> float# -> big -> float#
  = "" "big_pair_native"

external big_pair_noalloc : int64_u -> big -> float# -> big -> float#
  = "" "big_pair_native"
  [@@noalloc]

let p : big =
  #( #(#1L, #2L, #3L, #4L),
     #(#5.0, #6.0, #7.0, #8.0),
     #(i32 9l, f32 10.0, i16 (-11), i8 (-12)),
     #13L )

let q : big =
  #( #(#14L, #15L, #16L, #17L),
     #(#18.0, #19.0, #20.0, #21.0),
     #(i32 22l, f32 23.0, i16 (-24), i8 (-25)),
     #26L )

let () =
  Printf.eprintf "big_pair: %.4f\n%!" (box_float (big_pair #3L p #0.5 q));
  Printf.eprintf "big_pair_noalloc: %.4f\n%!"
    (box_float (big_pair_noalloc #3L p #0.5 q))
