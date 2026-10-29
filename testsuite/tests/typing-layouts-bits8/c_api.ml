(* TEST
 modules = "c_functions.c";
 include stdlib_stable;
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* This file tests using external C functions with int8#. *)
open Stdlib_stable

external to_int8 : int8# -> int8 = "%tag_int8"

let print_int8 s f = Printf.printf "%s: %d\n" s (Int8.to_int f)
let print_int8u s f = print_int8 s (to_int8 f)

(* Various combinations of arguments int8, int8 [@unboxed], and
   int8# *)
external lognot_UtoU : (int8#[@unsafe_unextended]) -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int8 -> int8# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : (int8#[@unsafe_unextended]) -> int8 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int8[@unboxed]) -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : (int8#[@unsafe_unextended]) -> (int8[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (Int8_u.of_int 42) in
  print_int8u "int8# -> int8#, ~42" i

let () =
  let i = lognot_BtoU ((Int8.of_int (-100))) in
  print_int8u "int8 -> int8#, ~(-100)" i

let () =
  let f = lognot_UtoB (Int8_u.of_int 255) in
  print_int8 "int8# -> int8, ~255" f

let () =
  let f = lognot_BUtoU (Int8.of_int 1024) in
  print_int8u "(int8[@unboxed]) -> int8#, ~1024" f

let () =
  let f = lognot_UtoBU ((Int8_u.of_int (-1726))) in
  print_int8 "int8# -> (int8[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  (int8#[@unsafe_unextended]) -> int8 -> (int8#[@unsafe_unextended]) -> int8 ->
  (int8#[@unsafe_unextended]) -> int8 -> (int8#[@unsafe_unextended]) -> int8# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (Int8_u.of_int 1) (Stdlib_stable.Int8.of_int 2) (Stdlib_stable.Int8_u.of_int 3) (Stdlib_stable.Int8.of_int 4)
      (Int8_u.of_int 5) (Stdlib_stable.Int8.of_int 6) (Stdlib_stable.Int8_u.of_int 7)
  in
  print_int8u "Function of 7 args, 1+2+3+4+5+6+7" f

(* Small integers passed on the stack: with more arguments than there are
   integer argument registers, the trailing small ints are stack-passed.  On
   macOS arm64 they are byte-packed (each occupies only its own size, where
   the AAPCS64 pads to 8 bytes), so each argument's store must write only its
   own bytes. *)
external sum_stack_args
  :  int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u -> int64_u
  -> int64_u
  -> (int8#[@unsafe_unextended])
  -> (int16#[@unsafe_unextended])
  -> (int8#[@unsafe_unextended])
  -> (int8#[@unsafe_unextended])
  -> (int[@untagged])
  = "sum_stack_args_bytecode" "sum_stack_args"
  [@@noalloc]

let () =
  let n =
    sum_stack_args #1L #2L #3L #4L #5L #6L #7L #8L (Int8_u.of_int 10)
      (Stdlib_stable.Int16_u.of_int 1000) (Int8_u.of_int 20) (Int8_u.of_int 30)
  in
  Printf.printf "Stack args, 36+10+1000+20+30: %d\n" n
