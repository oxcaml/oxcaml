(* TEST
   expect;
*)

let double x = 2*x
let add x y = x + y
[%%expect{|
val double : int -> int = <fun>
val add : int -> int -> int = <fun>
|}]

let partial_application = add 5
let error = double partial_application
[%%expect{|
val partial_application : int -> int = <fun>
Line 1, characters 19-38:
1 | let error = double partial_application
                       ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int -> int"
       but an expression was expected of type "int"
|}]

let f x y condition = if condition then add x else x * y
[%%expect{|
Line 1, characters 51-56:
1 | let f x y condition = if condition then add x else x * y
                                                       ^^^^^
Error: This expression has type "int" but an expression was expected of type
         "int -> int"
|}]
