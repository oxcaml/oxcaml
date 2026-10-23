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

(* Structs with 128-bit vectors on x86-64: class MEMORY, on the stack at
   16-byte alignment. Not arm64, which passes them by reference. *)

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"

external make_i64x2 : int64_u -> int64_u -> int64x2#
  = "" "make_i64x2_native"

external v_v : #(int64x2# * int64x2#) -> int64_u = "" "v_v_native"

let () =
  Printf.eprintf "v_v: %Ld\n%!"
    (box_int64 (v_v #(make_i64x2 #1L #2L, make_i64x2 #3L #4L)))

external i64_v : #(int64_u * int64x2#) -> int64_u = "" "i64_v_native"

let () =
  Printf.eprintf "i64_v: %Ld\n%!"
    (box_int64 (i64_v #(#1L, make_i64x2 #2L #3L)))

external i64_v_noalloc : #(int64_u * int64x2#) -> int64_u = "" "i64_v_native"
  [@@noalloc]

let () =
  Printf.eprintf "i64_v_noalloc: %Ld\n%!"
    (box_int64 (i64_v_noalloc #(#1L, make_i64x2 #2L #3L)))

(* The int64 after the struct must skip its tail padding. *)
external i64x6_v_i64_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64x2# * int64_u) -> int64_u -> int64_u
  = "" "i64x6_v_i64_i64_native"

let () =
  Printf.eprintf "i64x6_v_i64_i64: %Ld\n%!"
    (box_int64
       (i64x6_v_i64_i64 #1L #2L #3L #4L #5L #6L #(make_i64x2 #7L #8L, #9L) #10L))

(* The 16-aligned struct cannot start right after the int64 on the stack. *)
external i64x7_i64_v_i64 :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> #(int64_u * int64x2#) -> int64_u -> int64_u
  = "" "i64x7_i64_v_i64_native"

let () =
  Printf.eprintf "i64x7_i64_v_i64: %Ld\n%!"
    (box_int64
       (i64x7_i64_v_i64 #1L #2L #3L #4L #5L #6L #7L #(#8L, make_i64x2 #9L #10L)
          #11L))

(* As above, with the vector first. *)
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

external f64_v_f64 : #(float# * int64x2#) -> float# -> float#
  = "" "f64_v_f64_native"

let () =
  Printf.eprintf "f64_v_f64: %.4f\n%!"
    (box_float (f64_v_f64 #(#1.0, make_i64x2 #2L #3L) #0.5))
