(* TEST
 modules = "vec128_amd64_stubs.c";
 reference = "${test_source_directory}/unboxed_args_vec_amd64.reference";
 flambda2;
 arch_amd64;
 {
   flags = "-extension layouts_beta -extension simd_beta";
   native;
 }
 {
   flags = "-extension layouts_beta -extension simd_beta -flambda2-reaper -reaper-local-fields";
   native;
 }
*)

(* Test of unboxed products of 128-bit vectors and other members in external
   calls on x86-64. Their C structs are over 16 bytes, so class MEMORY, passed
   by value on the stack at the struct's alignment (16), with padding around
   the vector members. The stubs in vec128_amd64_stubs.c use C structs of SSE
   vector types, checking interoperability with the C compiler.

   Restricted to x86-64, since arm64 passes these structs by reference (they
   are not homogeneous aggregates), which is unsupported. *)

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"

external make_i64x2 : int64_u -> int64_u -> int64x2#
  = "" "make_i64x2_native"

(* Two vectors: 32 bytes with no padding. *)
external v_v : #(int64x2# * int64x2#) -> int64_u = "" "v_v_native"

let () =
  Printf.eprintf "v_v: %Ld\n%!"
    (box_int64 (v_v #(make_i64x2 #1L #2L, make_i64x2 #3L #4L)))

(* An int64 then a vector: padding between them. *)
external i64_v : #(int64_u * int64x2#) -> int64_u = "" "i64_v_native"

let () =
  Printf.eprintf "i64_v: %Ld\n%!"
    (box_int64 (i64_v #(#1L, make_i64x2 #2L #3L)))

(* The same with [@@noalloc]. *)
external i64_v_noalloc : #(int64_u * int64x2#) -> int64_u = "" "i64_v_native"
  [@@noalloc]

let () =
  Printf.eprintf "i64_v_noalloc: %Ld\n%!"
    (box_int64 (i64_v_noalloc #(#1L, make_i64x2 #2L #3L)))

(* A vector then an int64: padding after the int64, which the int64 scalar on
   the stack after the struct must skip. *)
external i64x6_v_i64_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64x2# * int64_u) -> int64_u -> int64_u
  = "" "i64x6_v_i64_i64_native"

let () =
  Printf.eprintf "i64x6_v_i64_i64: %Ld\n%!"
    (box_int64
       (i64x6_v_i64_i64 #1L #2L #3L #4L #5L #6L #(make_i64x2 #7L #8L, #9L) #10L))

(* With an int64 already on the stack, the 16-aligned struct cannot start
   right after it although its first slot is only 8 bytes. *)
external i64x7_i64_v_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * int64x2#) -> int64_u -> int64_u
  = "" "i64x7_i64_v_i64_native"

let () =
  Printf.eprintf "i64x7_i64_v_i64: %Ld\n%!"
    (box_int64
       (i64x7_i64_v_i64 #1L #2L #3L #4L #5L #6L #7L #(#8L, make_i64x2 #9L #10L)
          #11L))

(* As above with a struct starting with a vector. *)
external i64x7_v_v_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64x2# * int64x2#) -> int64_u -> int64_u
  = "" "i64x7_v_v_i64_native"

let () =
  Printf.eprintf "i64x7_v_v_i64: %Ld\n%!"
    (box_int64
       (i64x7_v_v_i64 #1L #2L #3L #4L #5L #6L #7L
          #(make_i64x2 #8L #9L, make_i64x2 #10L #11L)
          #12L))

(* A double next to a vector; the double after the struct takes a register. *)
external f64_v_f64 : #(float# * int64x2#) -> float# -> float#
  = "" "f64_v_f64_native"

let () =
  Printf.eprintf "f64_v_f64: %.4f\n%!"
    (box_float (f64_v_f64 #(#1.0, make_i64x2 #2L #3L) #0.5))
