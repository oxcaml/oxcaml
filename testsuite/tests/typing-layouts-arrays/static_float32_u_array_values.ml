(* TEST
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   jsir;
 }
*)

[@@@ocaml.flambda_o3]

external length : ('a : float32). 'a iarray -> int = "%array_length"
external get : ('a : float32). 'a iarray -> int -> 'a = "%array_safe_get"
external box : float32_u -> float32 = "%box_float32"
external to_float : float32 -> float = "%floatoffloat32"

let print_array name (arr : float32_u iarray) =
  Printf.printf "%s:" name;
  for i = 0 to length arr - 1 do
    Printf.printf " %h" (to_float (box (get arr i)))
  done;
  print_newline ()

let one : float32_u iarray = [: #42.0s :]
let two : float32_u iarray = [: #1.0s; #2.5s :]
let three : float32_u iarray = [: #1.0s; #2.5s; -#3.0s :]
let special : float32_u iarray = [: #0.0s; -#0.0s; #0.1s; #1e10s; -#1e-10s :]

let () =
  print_array "one" one;
  print_array "two" two;
  print_array "three" three;
  print_array "special" special
