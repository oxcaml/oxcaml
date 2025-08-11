(* TEST
 include stdlib_stable;
 expect;
*)

(* This test verifies that small_numbers are available by default with stable universe *)

type t = int8;;
[%%expect{|
type t = int8
|}];;

type t = int16;;
[%%expect{|
type t = int16
|}];;

let i8 = Stdlib_stable.Int8.minus_one;;
[%%expect{|
val i8 : int8 = -1
|}];;

let i16 = Stdlib_stable.Int16.minus_one;;
[%%expect{|
val i16 : int16 = -1
|}];;

(* Simple literals *)
let _ = #127y;;
[%%expect{|
- : int8 = 127
|}];;

let _ = #32767s;;
[%%expect{|
- : int16 = 32767
|}];;

(* Basic operations work *)
let x = Stdlib_stable.Int8.(add (of_int 3) (of_int 5));;
[%%expect{|
val x : int8 = 8
|}];;

let y = Stdlib_stable.Int16.(mul (of_int 10) (of_int 10));;
[%%expect{|
val y : int16 = 100
|}];;