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
Line 2, characters 19-38:
2 | let error = double partial_application
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


let foo_1 a b ~c = (a + b) * c
let foo_2 a b = foo_1 (a + 1) (b + 1)
let foo_3 a b condition = if condition then foo_2 a b else 0
[%%expect{|
val foo_1 : int -> int -> c:int -> int = <fun>
val foo_2 : int -> int -> c:int -> int = <fun>
Line 3, characters 59-60:
3 | let foo_3 a b condition = if condition then foo_2 a b else 0
                                                               ^
Error: This expression has type "int" but an expression was expected of type
         "c:int -> int"
|}]
