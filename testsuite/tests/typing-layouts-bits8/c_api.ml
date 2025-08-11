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
external lognot_UtoU : int8# -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int8 -> int8# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int8# -> int8 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int8[@unboxed]) -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int8# -> (int8[@unboxed]) =
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
  int8# -> int8 -> int8# -> int8 ->
  int8# -> int8 -> int8# -> int8# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (Int8_u.of_int 1) (Stdlib_stable.Int8.of_int 2) (Stdlib_stable.Int8_u.of_int 3) (Stdlib_stable.Int8.of_int 4)
      (Int8_u.of_int 5) (Stdlib_stable.Int8.of_int 6) (Stdlib_stable.Int8_u.of_int 7)
  in
  print_int8u "Function of 7 args, 1+2+3+4+5+6+7" f
