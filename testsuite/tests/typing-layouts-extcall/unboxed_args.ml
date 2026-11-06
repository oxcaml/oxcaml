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

(* Unboxed product arguments in registers. Single-component products are
   passed like their member, so may spill. *)

open Stdlib_stable

external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32_u -> (int32[@local_opt]) = "%box_int32"
external box_float : float# -> (float[@local_opt]) = "%box_float"
external box_float32 : float32_u -> (float32[@local_opt]) = "%box_float32"
external unbox_int32 : (int32[@local_opt]) -> int32_u = "%unbox_int32"
external unbox_float32 : (float32[@local_opt]) -> float32_u = "%unbox_float32"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external untag_int16 : int16 -> int16# = "%untag_int16"
external untag_int8 : int8 -> int8# = "%untag_int8"

let i32 x = unbox_int32 x
let f32 x = unbox_float32 (Float32.of_float x)
let i16 x = untag_int16 (Int16.of_int x)
let i8 x = untag_int8 (Int8.of_int x)
let f32' x = float_of_float32 (box_float32 x)

external sum_i64_pair : #(int64_u * int64_u) -> int64_u
  = "sum_i64_pair_bytecode" "sum_i64_pair_native"

let () =
  Printf.eprintf "sum_i64_pair: %Ld\n%!"
    (box_int64 (sum_i64_pair #(#111L, #222L)))

(* Packed into one register. *)
external sum_i32_pair : #(int32_u * int32_u) -> int64_u
  = "sum_i32_pair_bytecode" "sum_i32_pair_native"

let () =
  Printf.eprintf "sum_i32_pair: %Ld\n%!"
    (box_int64 (sum_i32_pair #(i32 (-123456l), i32 987654l)))

(* Packed with padding into one register. *)
external sum_i16_i32 : #(int16# * int32_u) -> int64_u
  = "sum_i16_i32_bytecode" "sum_i16_i32_native"

let () =
  Printf.eprintf "sum_i16_i32: %Ld\n%!"
    (box_int64 (sum_i16_i32 #(i16 (-300), i32 1000000l)))

(* Two SSE registers on x86-64; a homogeneous aggregate on arm64. *)
external sum_f64_pair : #(float# * float#) -> float#
  = "sum_f64_pair_bytecode" "sum_f64_pair_native"

let () =
  Printf.eprintf "sum_f64_pair: %.4f\n%!"
    (box_float (sum_f64_pair #(#1.25, #2.5)))

(* Two packed SSE registers on x86-64; a homogeneous aggregate on arm64. *)
external sum_f32_quad : #(float32_u * float32_u * float32_u * float32_u) -> float#
  = "sum_f32_quad_bytecode" "sum_f32_quad_native"

let () =
  Printf.eprintf "sum_f32_quad: %.4f\n%!"
    (box_float (sum_f32_quad #(f32 1.0, f32 2.5, f32 (-0.5), f32 8.0)))

(* Mixed classes in one unit, so an integer register. *)
external sum_f32_i32 : #(float32_u * int32_u) -> float#
  = "sum_f32_i32_bytecode" "sum_f32_i32_native"

let () =
  Printf.eprintf "sum_f32_i32: %.4f\n%!"
    (box_float (sum_f32_i32 #(f32 (-1.5), i32 10l)))

external sum_int_f64 : #(int * float#) -> float#
  = "sum_int_f64_bytecode" "sum_int_f64_native"

let () =
  Printf.eprintf "sum_int_f64: %.4f\n%!"
    (box_float (sum_int_f64 #(41, #0.25)))

(* Not a homogeneous aggregate on arm64, so two integer registers. *)
external sum_f64_i64 : #(float# * int64_u) -> float#
  = "sum_f64_i64_bytecode" "sum_f64_i64_native"

let () =
  Printf.eprintf "sum_f64_i64: %.4f\n%!"
    (box_float (sum_f64_i64 #(#2.5, #100L)))

(* The inner pair is packed into the first register. *)
external sum_nested : #(#(int32_u * int32_u) * int64_u) -> int64_u
  = "sum_nested_bytecode" "sum_nested_native"

let () =
  Printf.eprintf "sum_nested: %Ld\n%!"
    (box_int64 (sum_nested #(#(i32 7l, i32 (-9l)), #1000L)))

(* The inner struct is padded to 8 bytes: 12 bytes overall, not 8. *)
external sum_nested_padded : #(#(int32_u * int16#) * int16#) -> int64_u
  = "sum_nested_padded_bytecode" "sum_nested_padded_native"

let () =
  Printf.eprintf "sum_nested_padded: %Ld\n%!"
    (box_int64 (sum_nested_padded #(#(i32 100000l, i16 (-300)), i16 7)))

external sum_two_pairs : #(int64_u * int64_u) -> #(int64_u * int64_u) -> int64_u
  = "sum_two_pairs_bytecode" "sum_two_pairs_native"

let () =
  Printf.eprintf "sum_two_pairs: %Ld\n%!"
    (box_int64 (sum_two_pairs #(#1L, #2L) #(#3L, #4L)))

(* Exactly the six x86-64 integer argument registers. *)
external sum_three_pairs :
  #(int64_u * int64_u) -> #(int64_u * int64_u) -> #(int64_u * int64_u) -> int64_u
  = "sum_three_pairs_bytecode" "sum_three_pairs_native"

let () =
  Printf.eprintf "sum_three_pairs: %Ld\n%!"
    (box_int64 (sum_three_pairs #(#1L, #2L) #(#3L, #4L) #(#5L, #6L)))

external mixed_args :
  int64_u -> #(float# * float#) -> int -> #(int32_u * int32_u) -> float#
  -> int64_u
  = "mixed_args_bytecode" "mixed_args_native"

let () =
  Printf.eprintf "mixed_args: %Ld\n%!"
    (box_int64
       (mixed_args #7L #(#0.5, #0.25) 3 #(i32 10l, i32 20l) #4.0))

(* The void component is erased: the stub takes an int64_t. *)
external sum_i64_void : #(int64_u * unit#) -> int64_u
  = "sum_i64_void_bytecode" "sum_i64_void_native"

let () =
  Printf.eprintf "sum_i64_void: %Ld\n%!"
    (box_int64 (sum_i64_void #(#123L, #())))

(* On the stack in an 8-byte slot, even on Apple arm64 (unlike a bare
   int16_t). *)
external stack_single :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> int32_u -> #(int16# * unit#) -> int32_u -> int64_u
  = "stack_single_bytecode" "stack_single_native"

let () =
  Printf.eprintf "stack_single: %Ld\n%!"
    (box_int64
       (stack_single #1L #2L #3L #4L #5L #6L #7L #8L (i32 100l)
          #(i16 (-5), #()) (i32 1000l)))

external swap_mixed : #(int32_u * float32_u) -> #(float32_u * int32_u)
  = "swap_mixed_bytecode" "swap_mixed_native"

let () =
  let #(a, b) = swap_mixed #(i32 5l, f32 (-2.5)) in
  Printf.eprintf "swap_mixed: %.4f %ld\n%!" (f32' a) (box_int32 b)

external sum_i16_quad : #(int16# * int16# * int16# * int16#) -> int64_u
  = "sum_i16_quad_bytecode" "sum_i16_quad_native"

let () =
  Printf.eprintf "sum_i16_quad: %Ld\n%!"
    (box_int64 (sum_i16_quad #(i16 (-1), i16 2, i16 (-3000), i16 4000)))

external sum_i8_i8_i16_i32 : #(int8# * int8# * int16# * int32_u) -> int64_u
  = "sum_i8_i8_i16_i32_bytecode" "sum_i8_i8_i16_i32_native"

let () =
  Printf.eprintf "sum_i8_i8_i16_i32: %Ld\n%!"
    (box_int64
       (sum_i8_i8_i16_i32 #(i8 (-100), i8 50, i16 (-20000), i32 1000000l)))

(* On arm64, the float32 travels in an integer register. *)
external sum_f32_i64 : #(float32_u * int64_u) -> float#
  = "sum_f32_i64_bytecode" "sum_f32_i64_native"

let () =
  Printf.eprintf "sum_f32_i64: %.4f\n%!"
    (box_float (sum_f32_i64 #(f32 2.5, #10L)))

(* xmm0 (packed) and xmm1 on x86-64; a homogeneous aggregate on arm64. *)
external sum_f32_triple : #(float32_u * float32_u * float32_u) -> float#
  = "sum_f32_triple_bytecode" "sum_f32_triple_native"

let () =
  Printf.eprintf "sum_f32_triple: %.4f\n%!"
    (box_float (sum_f32_triple #(f32 1.5, f32 (-0.25), f32 4.0)))

external sum_string_i64 : #(string * int64_u) -> int64_u
  = "sum_string_i64_bytecode" "sum_string_i64_native"

let () =
  Printf.eprintf "sum_string_i64: %Ld\n%!"
    (box_int64 (sum_string_i64 #("hello", #100L)))

(* After the six x86-64 integer argument registers are used up. *)
external sum_three_pairs_then_int :
  #(int64_u * int64_u) -> #(int64_u * int64_u) -> #(int64_u * int64_u) -> int
  -> int64_u
  = "sum_three_pairs_then_int_bytecode" "sum_three_pairs_then_int_native"

let () =
  Printf.eprintf "sum_three_pairs_then_int: %Ld\n%!"
    (box_int64
       (sum_three_pairs_then_int #(#1L, #2L) #(#3L, #4L) #(#5L, #6L) 100))

(* A 4-byte stack slot on Apple arm64, like a bare float32; the float32 after
   it checks this. *)
external stack_single_f32 :
  float# -> float# -> float# -> float# -> float# -> float# -> float# -> float#
  -> #(float32_u * unit#) -> float32_u -> float#
  = "stack_single_f32_bytecode" "stack_single_f32_native"

let () =
  Printf.eprintf "stack_single_f32: %.4f\n%!"
    (box_float
       (stack_single_f32 #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #7.0 #8.0
          #(f32 0.5, #()) (f32 0.25)))

external sum_i64_pair_noalloc : #(int64_u * int64_u) -> int64_u
  = "sum_i64_pair_bytecode" "sum_i64_pair_native"
  [@@noalloc]

let () =
  Printf.eprintf "sum_i64_pair_noalloc: %Ld\n%!"
    (box_int64 (sum_i64_pair_noalloc #(#5L, #6L)))

(* The pair still goes in registers once the ints spill on x86-64. *)
external seven_ints_f64_pair :
  int -> int -> int -> int -> int -> int -> int -> #(float# * float#) -> float#
  = "seven_ints_f64_pair_bytecode" "seven_ints_f64_pair_native"

let () =
  Printf.eprintf "seven_ints_f64_pair: %.4f\n%!"
    (box_float (seven_ints_f64_pair 1 2 3 4 5 6 7 #(#0.5, #0.25)))

external stack_single_value :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(int * unit#) -> int -> int64_u
  = "stack_single_value_bytecode" "stack_single_value_native"

let () =
  Printf.eprintf "stack_single_value: %Ld\n%!"
    (box_int64
       (stack_single_value #1L #2L #3L #4L #5L #6L #7L #8L #(5, #()) 7))

(* No C arguments. *)
external all_void_arg : #(unit# * unit#) -> int64_u
  = "all_void_arg_bytecode" "all_void_arg_native"

let () =
  Printf.eprintf "all_void_arg: %Ld\n%!" (box_int64 (all_void_arg #(#(), #())))

external void_arg : unit# -> int64_u = "void_arg_bytecode" "void_arg_native"

let () = Printf.eprintf "void_arg: %Ld\n%!" (box_int64 (void_arg #()))

external void_between_args :
  int64_u -> unit# -> #(int32_u * int32_u) -> unit# -> int64_u -> int64_u
  = "void_between_args_bytecode" "void_between_args_native"

let () =
  Printf.eprintf "void_between_args: %Ld\n%!"
    (box_int64 (void_between_args #1L #() #(i32 10l, i32 100l) #() #1000L))

(* Void components contribute no members or padding. *)
external void_inside_f32 :
  #(unit# * float32_u * unit# * float32_u * unit#) -> float#
  = "void_inside_f32_bytecode" "void_inside_f32_native"

let () =
  Printf.eprintf "void_inside_f32: %.4f\n%!"
    (box_float (void_inside_f32 #(#(), f32 1.5, #(), f32 (-0.25), #())))

(* An all-void inner product has neither size nor alignment. *)
external void_nested :
  #(#(unit# * int16#) * #(unit# * unit#) * #(int32_u * unit#)) -> int64_u
  = "void_nested_bytecode" "void_nested_native"

let () =
  Printf.eprintf "void_nested: %Ld\n%!"
    (box_int64
       (void_nested #(#(#(), i16 (-300)), #(#(), #()), #(i32 70000l, #()))))

(* Like [stack_single], nested among voids. *)
external void_stack_single_nested :
  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u -> #(unit# * #(unit# * int16#) * unit#) -> int32_u -> int64_u
  = "void_stack_single_nested_bytecode" "void_stack_single_nested_native"

let () =
  Printf.eprintf "void_stack_single_nested: %Ld\n%!"
    (box_int64
       (void_stack_single_nested #1L #2L #3L #4L #5L #6L #7L #8L
          #(#(), #(#(), i16 (-5)), #()) (i32 7l)))

(* Void arguments take no registers: these use exactly all the x86-64
   argument registers of one class. *)
external void_regs_int :
  int64_u -> int64_u -> int64_u -> int64_u -> unit# -> #(unit# * unit#)
  -> #(int64_u * int64_u) -> int64_u
  = "void_regs_int_bytecode" "void_regs_int_native"

let () =
  Printf.eprintf "void_regs_int: %Ld\n%!"
    (box_int64 (void_regs_int #1L #2L #3L #4L #() #(#(), #()) #(#5L, #6L)))

external void_regs_float :
  float# -> float# -> float# -> float# -> float# -> float# -> #(unit# * unit#)
  -> unit# -> #(float# * float#) -> float#
  = "void_regs_float_bytecode" "void_regs_float_native"

let () =
  Printf.eprintf "void_regs_float: %.4f\n%!"
    (box_float
       (void_regs_float #1.0 #2.0 #3.0 #4.0 #5.0 #6.0 #(#(), #()) #()
          #(#7.0, #8.0)))

type i32_void_i64 = #{ x : int32_u; v : unit#; y : int64_u }

external void_record : i32_void_i64 -> int64_u
  = "void_record_bytecode" "void_record_native"

let () =
  Printf.eprintf "void_record: %Ld\n%!"
    (box_int64 (void_record #{ x = i32 (-3l); v = #(); y = #50L }))

external void_swap :
  unit# -> #(int32_u * unit# * float32_u) -> unit#
  -> #(unit# * float32_u * int32_u)
  = "void_swap_bytecode" "void_swap_native"

let () =
  let #(#(), a, b) = void_swap #() #(i32 12l, #(), f32 0.75) #() in
  Printf.eprintf "void_swap: %.4f %ld\n%!" (f32' a) (box_int32 b)

(* One SSE register on x86-64; a homogeneous aggregate on arm64. *)
external sum_f32_pair : #(float32_u * float32_u) -> float#
  = "sum_f32_pair_bytecode" "sum_f32_pair_native"

let () =
  Printf.eprintf "sum_f32_pair: %.4f\n%!"
    (box_float (sum_f32_pair #(f32 1.5, f32 (-0.25))))

(* 12 bytes: two registers, the second holding one int32. *)
external sum_i32_triple : #(int32_u * int32_u * int32_u) -> int64_u
  = "sum_i32_triple_bytecode" "sum_i32_triple_native"

let () =
  Printf.eprintf "sum_i32_triple: %Ld\n%!"
    (box_int64 (sum_i32_triple #(i32 (-1l), i32 20l, i32 300l)))

(* xmm0 and a packed xmm1 on x86-64; two integer registers on arm64. *)
external sum_f64_f32_f32 : #(float# * float32_u * float32_u) -> float#
  = "sum_f64_f32_f32_bytecode" "sum_f64_f32_f32_native"

let () =
  Printf.eprintf "sum_f64_f32_f32: %.4f\n%!"
    (box_float (sum_f64_f32_f32 #(#1.5, f32 0.5, f32 0.25)))

external sum_i8_f64 : #(int8# * float#) -> float#
  = "sum_i8_f64_bytecode" "sum_i8_f64_native"

let () =
  Printf.eprintf "sum_i8_f64: %.4f\n%!"
    (box_float (sum_i8_f64 #(i8 (-7), #0.5)))

external sum_f32_f64 : #(float32_u * float#) -> float#
  = "sum_f32_f64_bytecode" "sum_f32_f64_native"

let () =
  Printf.eprintf "sum_f32_f64: %.4f\n%!"
    (box_float (sum_f32_f64 #(f32 0.25, #3.0)))

external sum_i64_i8 : #(int64_u * int8#) -> int64_u
  = "sum_i64_i8_bytecode" "sum_i64_i8_native"

let () =
  Printf.eprintf "sum_i64_i8: %Ld\n%!"
    (box_int64 (sum_i64_i8 #(#1000L, i8 (-3))))

external sum_i16_f32 : #(int16# * float32_u) -> float#
  = "sum_i16_f32_bytecode" "sum_i16_f32_native"

let () =
  Printf.eprintf "sum_i16_f32: %.4f\n%!"
    (box_float (sum_i16_f32 #(i16 (-300), f32 0.5)))

external sum_i32_i32_f64 : #(int32_u * int32_u * float#) -> float#
  = "sum_i32_i32_f64_bytecode" "sum_i32_i32_f64_native"

let () =
  Printf.eprintf "sum_i32_i32_f64: %.4f\n%!"
    (box_float (sum_i32_i32_f64 #(i32 1l, i32 (-2l), #0.5)))

external sum_int_int : #(int * int) -> int
  = "sum_int_int_bytecode" "sum_int_int_native"

let () = Printf.eprintf "sum_int_int: %d\n%!" (sum_int_int #(17, -4))

external sum_n_i64 : #(nativeint_u * int64_u) -> int64_u
  = "sum_n_i64_bytecode" "sum_n_i64_native"

let () =
  Printf.eprintf "sum_n_i64: %Ld\n%!" (box_int64 (sum_n_i64 #(#5n, #7L)))

external sum_nested3 : #(#(#(int8# * int8#) * int16#) * int32_u) -> int64_u
  = "sum_nested3_bytecode" "sum_nested3_native"

let () =
  Printf.eprintf "sum_nested3: %Ld\n%!"
    (box_int64 (sum_nested3 #(#(#(i8 1, i8 (-2)), i16 300), i32 (-40000l))))

(* Padding before an inner struct more aligned than the member before it. *)
external sum_nested_inner_padded : #(int8# * #(int32_u * int8#)) -> int64_u
  = "sum_nested_inner_padded_bytecode" "sum_nested_inner_padded_native"

let () =
  Printf.eprintf "sum_nested_inner_padded: %Ld\n%!"
    (box_int64 (sum_nested_inner_padded #(i8 (-1), #(i32 100000l, i8 7))))

external attr_scalars :
  (int[@untagged]) -> #(int32_u * int32_u) -> (float[@unboxed]) -> float#
  = "attr_scalars_bytecode" "attr_scalars_native"

let () =
  Printf.eprintf "attr_scalars: %.4f\n%!"
    (box_float (attr_scalars 3 #(i32 10l, i32 20l) 0.5))
