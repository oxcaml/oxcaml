(* TEST
 modules = "c_functions.c";
 flambda2;
 {
   native;
 }{
   bytecode;
 }{
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

(* This file tests using external C functions with int#. *)

external to_int : int# -> (int[@local_opt]) = "%box_int"

let print_intu s f = Printf.printf "%s: %nd\n" s (to_int f)
let print_int s f = Printf.printf "%s: %nd\n" s f

(* Various combinations of arguments int, int [@unboxed], and
   int# *)
external lognot_UtoU : int# -> int# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int -> int# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int# -> int =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int[@unboxed]) -> int# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int# -> (int[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU #42n in
  print_intu "int# -> int#, ~42" i

let () =
  let i = lognot_BtoU (-100n) in
  print_intu "int -> int#, ~(-100)" i

let () =
  let f = lognot_UtoB #255n in
  print_int "int# -> int, ~255" f

let () =
  let f = lognot_BUtoU 1024n in
  print_intu "(int[@unboxed]) -> int#, ~1024" f

let () =
  let f = lognot_UtoBU (-#1726n) in
  print_int "int# -> (int[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int# -> int -> int# -> int ->
  int# -> int -> int# -> int# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      #1n 2n #3n 4n
      #5n 6n #7n
  in
  print_intu "Function of 7 args, 1+2+3+4+5+6+7" f
