(* TEST
 modules = "c_functions.c";
 include stdlib_beta;
 flambda2;

 {
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   bytecode;
 }
*)

(* This file tests using external C functions with int#. *)

external to_int : int# -> (int[@local_opt]) = "%tag_int"

let print_intu s f = Printf.printf "%s: %d\n" s (to_int f)
let print_int s f = Printf.printf "%s: %d\n" s f

(* Various combinations of arguments int, int [@untagged], and
   int# *)
external lognot_UtoU : int# -> int# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int -> int# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int# -> int =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int[@untagged]) -> int# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int# -> (int[@untagged]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (Stdlib_beta.Int_u.of_int 42) in
  print_intu "int# -> int#, ~42" i

let () =
  let i = lognot_BtoU (-100) in
  print_intu "int -> int#, ~(-100)" i

let () =
  let f = lognot_UtoB (Stdlib_beta.Int_u.of_int 255) in
  print_int "int# -> int, ~255" f

let () =
  let f = lognot_BUtoU 1024 in
  print_intu "(int[@untagged]) -> int#, ~1024" f

let () =
  let f = lognot_UtoBU ((Stdlib_beta.Int_u.of_int (-1726))) in
  print_int "int# -> (int[@untagged]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int# -> int -> int# -> int ->
  int# -> int -> int# -> int# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let () =
  let f =
    sum_7
      (Stdlib_beta.Int_u.of_int 1) 2 (Stdlib_beta.Int_u.of_int 3) 4
      (Stdlib_beta.Int_u.of_int 5) 6 (Stdlib_beta.Int_u.of_int 7)
  in
  print_intu "Function of 7 args, 1+2+3+4+5+6+7" f


external is_sign_extended : int# -> bool = "return_true" "is_sign_extended"

external invalid_sign_bit : unit -> int# = "negative_one" "invalid_sign_bit"

(* For now, we must ensure that values are stored sign-extended, even when we get a non
   sign-extended value back from C *)
let () = assert (is_sign_extended (invalid_sign_bit()))
