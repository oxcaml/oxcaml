(* TEST
 modules = "stack_stubs.c";
 reference = "${test_source_directory}/unboxed_args_stack.reference";
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
*)

(* Test of unboxed product arguments to external calls that C passes by value
   on the stack, because too few argument registers remain for them. The
   stubs in stack_stubs.c take C structs by value, checking interoperability
   with the C compiler.

   Every struct here is at most 16 bytes or an arm64 homogeneous aggregate, so
   that both ABIs pass it by value rather than by reference. *)

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

(* Eight int64s use up the integer argument registers on both ABIs (x86-64 has
   six; the last two int64s already go on the stack), so the pair follows them
   on the stack. *)
external i64x8_pair :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int64_u * int64_u) -> int64_u
  = "" "i64x8_pair_native"

let () =
  Printf.eprintf "i64x8_pair: %Ld\n%!"
    (box_int64 (i64x8_pair #1L #2L #3L #4L #5L #6L #7L #8L #(#9L, #10L)))

(* The same call with [@@noalloc], which bypasses [caml_c_call_stack_args]. *)
external i64x8_pair_noalloc :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int64_u * int64_u) -> int64_u
  = "" "i64x8_pair_native"
  [@@noalloc]

let () =
  Printf.eprintf "i64x8_pair_noalloc: %Ld\n%!"
    (box_int64
       (i64x8_pair_noalloc #1L #2L #3L #4L #5L #6L #7L #8L #(#9L, #10L)))

(* On x86-64, one integer register remains for the pair, so it goes on the
   stack and the register goes to the int64 after it. *)
external i64x5_pair_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> #(int64_u * int64_u)
  -> int64_u -> int64_u
  = "" "i64x5_pair_i64_native"

let () =
  Printf.eprintf "i64x5_pair_i64: %Ld\n%!"
    (box_int64 (i64x5_pair_i64 #1L #2L #3L #4L #5L #(#6L, #7L) #8L))

(* On arm64, one integer register remains for the pair, so it goes on the
   stack and, by AAPCS64 rule C.13, so does the int64 after it. *)
external i64x7_pair_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * int64_u) -> int64_u -> int64_u
  = "" "i64x7_pair_i64_native"

let () =
  Printf.eprintf "i64x7_pair_i64: %Ld\n%!"
    (box_int64 (i64x7_pair_i64 #1L #2L #3L #4L #5L #6L #7L #(#8L, #9L) #10L))

(* One floating-point register remains for the pair, so it goes on the stack;
   the float after it takes the register on x86-64 and, by AAPCS64 rule C.3,
   goes on the stack on arm64. *)
external f64x7_pair_f64 :
  float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float# * float#) -> float# -> float#
  = "" "f64x7_pair_f64_native"

let () =
  Printf.eprintf "f64x7_pair_f64: %.4f\n%!"
    (box_float
       (f64x7_pair_f64 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #(#8.0, #9.0) #10.0))

(* No floating-point register remains: the pair and the float after it go on
   the stack on both ABIs. *)
external f64x8_pair_f64 :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float# * float#) -> float# -> float#
  = "" "f64x8_pair_f64_native"

let () =
  Printf.eprintf "f64x8_pair_f64: %.4f\n%!"
    (box_float
       (f64x8_pair_f64 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0 #(#9.0, #10.0)
          #11.0))

(* A 12-byte struct of float32s on the stack, followed by a float32 whose
   position depends on the struct's size there: 16 bytes on x86-64 and Linux
   arm64, 12 bytes on Apple arm64. *)
external f64x7_f32x3_f32 :
  float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32_u * float32_u * float32_u) -> float32_u -> float#
  = "" "f64x7_f32x3_f32_native"

let () =
  Printf.eprintf "f64x7_f32x3_f32: %.4f\n%!"
    (box_float
       (f64x7_f32x3_f32 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0
          #(f32 0.5, f32 0.25, f32 0.125)
          (f32 2.0)))

(* On x86-64 the mixed pair needs an integer register and none remains, so it
   goes on the stack while the float after it still takes an SSE register. On
   arm64 the pair fits in the two remaining integer registers. *)
external i64x6_mixed_f64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * float#) -> float# -> float#
  = "" "i64x6_mixed_f64_native"

let () =
  Printf.eprintf "i64x6_mixed_f64: %.4f\n%!"
    (box_float (i64x6_mixed_f64 #1L #2L #3L #4L #5L #6L #(#7L, #0.5) #0.25))

(* The mixed pair goes on the stack on both ABIs; the float after it takes a
   register on both, since exhausting the integer registers does not affect
   the floating-point ones. *)
external i64x8_mixed_f64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int64_u * float#) -> float# -> float#
  = "" "i64x8_mixed_f64_native"

let () =
  Printf.eprintf "i64x8_mixed_f64: %.4f\n%!"
    (box_float
       (i64x8_mixed_f64 #1L #2L #3L #4L #5L #6L #7L #8L #(#9L, #0.5) #0.25))

(* Two structs on the stack in a row, then an int64 on the stack and a float
   in a register. *)
external i64x8_pairs_i64_f64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int64_u * int64_u) -> #(int16# * int32_u) -> int64_u
  -> float# -> float#
  = "" "i64x8_pairs_i64_f64_native"

let () =
  Printf.eprintf "i64x8_pairs_i64_f64: %.4f\n%!"
    (box_float
       (i64x8_pairs_i64_f64 #1L #2L #3L #4L #5L #6L #7L #8L #(#9L, #10L)
          #(i16 (-11), i32 12l)
          #13L #0.5))

(* A struct of small integers packed into one 8-byte stack slot, followed by
   an int32 (which on Apple arm64 takes a 4-byte slot right after it). *)
external i64x8_i16_i32_i32 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int16# * int32_u) -> int32_u -> int64_u
  = "" "i64x8_i16_i32_i32_native"

let () =
  Printf.eprintf "i64x8_i16_i32_i32: %Ld\n%!"
    (box_int64
       (i64x8_i16_i32_i32 #1L #2L #3L #4L #5L #6L #7L #8L
          #(i16 (-9), i32 (-10l))
          (i32 11l)))

(* A struct containing a value on the stack. *)
external i64x8_val_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int * int64_u) -> int64_u
  = "" "i64x8_val_i64_native"

let () =
  Printf.eprintf "i64x8_val_i64: %Ld\n%!"
    (box_int64 (i64x8_val_i64 #1L #2L #3L #4L #5L #6L #7L #8L #(9, #10L)))

(* Nested products on the stack: the inner product is a nested C struct. *)
external i64x8_nested :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(#(int32_u * int32_u) * int64_u) -> int64_u
  = "" "i64x8_nested_native"

let () =
  Printf.eprintf "i64x8_nested: %Ld\n%!"
    (box_int64
       (i64x8_nested #1L #2L #3L #4L #5L #6L #7L #8L #(#(i32 9l, i32 10l), #11L)))

(* A nested struct with padding after the inner struct's members. *)
external i64x8_nested_padded :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(#(int16# * int8#) * int64_u) -> int64_u
  = "" "i64x8_nested_padded_native"

let () =
  Printf.eprintf "i64x8_nested_padded: %Ld\n%!"
    (box_int64
       (i64x8_nested_padded #1L #2L #3L #4L #5L #6L #7L #8L
          #(#(i16 (-9), i8 10), #11L)))

(* A nested homogeneous aggregate of three doubles: 24 bytes, on the stack on
   both ABIs (class MEMORY on x86-64). *)
external f64x8_nested_hfa :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(#(float# * float#) * float#) -> float#
  = "" "f64x8_nested_hfa_native"

let () =
  Printf.eprintf "f64x8_nested_hfa: %.4f\n%!"
    (box_float
       (f64x8_nested_hfa #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0
          #(#(#9.0, #10.0), #11.0)))

(* A homogeneous aggregate of four doubles (32 bytes) with only three
   floating-point registers left: on the stack on arm64 (and the double after
   it too, by AAPCS64 rule C.3); class MEMORY on x86-64, where the double after
   it still takes a register. *)
external f64x5_quad_f64 :
  float# -> float# -> float# -> float# -> float#
  -> #(float# * float# * float# * float#) -> float# -> float#
  = "" "f64x5_quad_f64_native"

let () =
  Printf.eprintf "f64x5_quad_f64: %.4f\n%!"
    (box_float
       (f64x5_quad_f64 #1.0 #2.0 #3.0 #4.0 #5.0 #(#6.0, #7.0, #8.0, #9.0) #10.0))

(* Four float32s (16 bytes) with one floating-point register left: two SSE
   eightbytes on x86-64, a four-member homogeneous aggregate on arm64; on the
   stack on both. *)
external f64x7_f32x4_f32 :
  float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32_u * float32_u * float32_u * float32_u) -> float32_u -> float#
  = "" "f64x7_f32x4_f32_native"

let () =
  Printf.eprintf "f64x7_f32x4_f32: %.4f\n%!"
    (box_float
       (f64x7_f32x4_f32 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0
          #(f32 0.5, f32 0.25, f32 0.125, f32 0.0625)
          (f32 2.0)))

(* On x86-64 the int64 pair spills (one register left) but the int32 pair after
   it, which needs one register, still goes in it; the int64 after that goes on
   the stack after the int64 pair. *)
external i64x5_pair_i32pair_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> #(int64_u * int64_u)
  -> #(int32_u * int32_u) -> int64_u -> int64_u
  = "" "i64x5_pair_i32pair_i64_native"

let () =
  Printf.eprintf "i64x5_pair_i32pair_i64: %Ld\n%!"
    (box_int64
       (i64x5_pair_i32pair_i64 #1L #2L #3L #4L #5L #(#6L, #7L)
          #(i32 8l, i32 9l)
          #10L))

(* All argument registers of both classes used up, then structs and scalars of
   both classes on the stack, interleaved. *)
external i64x6_f64x8_mixed :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> float#
  -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(int64_u * int64_u) -> #(float# * float#) -> int64_u -> float# -> float#
  = "" "i64x6_f64x8_mixed_native"

let () =
  Printf.eprintf "i64x6_f64x8_mixed: %.4f\n%!"
    (box_float
       (i64x6_f64x8_mixed #1L #2L #3L #4L #5L #6L #1.0 #2.0 #3.0 #4.0 #5.0 #6.0
          #7.0 #8.0 #(#7L, #8L) #(#9.0, #10.0) #9L #11.0))

(* 12 bytes: two slots, the second half used. *)
external i64x8_i32_triple :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int32_u * int32_u * int32_u) -> int64_u
  = "" "i64x8_i32_triple_native"

let () =
  Printf.eprintf "i64x8_i32_triple: %Ld\n%!"
    (box_int64
       (i64x8_i32_triple #1L #2L #3L #4L #5L #6L #7L #8L
          #(i32 9l, i32 (-10l), i32 11l)))

(* Padding inside a struct on the stack. *)
external i64x8_i8_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int8# * int64_u) -> int64_u
  = "" "i64x8_i8_i64_native"

let () =
  Printf.eprintf "i64x8_i8_i64: %Ld\n%!"
    (box_int64 (i64x8_i8_i64 #1L #2L #3L #4L #5L #6L #7L #8L #(i8 (-9), #10L)))

(* One 8-byte slot, or two 4-byte ones on Apple arm64. *)
external f64x8_f32_pair :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32_u * float32_u) -> float#
  = "" "f64x8_f32_pair_native"

let () =
  Printf.eprintf "f64x8_f32_pair: %.4f\n%!"
    (box_float
       (f64x8_f32_pair #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0
          #(f32 9.0, f32 (-10.0))))
