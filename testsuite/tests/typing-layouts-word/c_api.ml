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

(* This file tests using external C functions with nativeint_u. *)

external to_nativeint : nativeint_u -> (nativeint[@local_opt]) = "%box_nativeint"

let print_nativeintu s f = Printf.printf "%s: %nd\n" s (to_nativeint f)
let print_nativeint s f = Printf.printf "%s: %nd\n" s f

(* Various combinations of arguments nativeint, nativeint [@unboxed], and
   nativeint_u *)
external lognot_UtoU : nativeint_u -> nativeint_u =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : nativeint -> nativeint_u =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : nativeint_u -> nativeint =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (nativeint[@unboxed]) -> nativeint_u =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : nativeint_u -> (nativeint[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU #42n in
  print_nativeintu "nativeint_u -> nativeint_u, ~42" i

let () =
  let i = lognot_BtoU (-100n) in
  print_nativeintu "nativeint -> nativeint_u, ~(-100)" i

let () =
  let f = lognot_UtoB #255n in
  print_nativeint "nativeint_u -> nativeint, ~255" f

let () =
  let f = lognot_BUtoU 1024n in
  print_nativeintu "(nativeint[@unboxed]) -> nativeint_u, ~1024" f

let () =
  let f = lognot_UtoBU (-#1726n) in
  print_nativeint "nativeint_u -> (nativeint[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  nativeint_u -> nativeint -> nativeint_u -> nativeint ->
  nativeint_u -> nativeint -> nativeint_u -> nativeint_u =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      #1n 2n #3n 4n
      #5n 6n #7n
  in
  print_nativeintu "Function of 7 args, 1+2+3+4+5+6+7" f
