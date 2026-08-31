(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of polymorphic mode variables and bounds in
 * annotations on let bindings and expressions
*)

let f : 'a @ [< 'm] -> 'a @ [> 'm] = fun x -> x
[%%expect{|
val f : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let i = (fun x -> x : 'a @ [< 'm] -> 'a @ [> 'm])
[%%expect{|
val i : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

(* Constant bounds are allowed in let binding annotations *)

let j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = fun x -> x
[%%expect{|
val j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}]

(* Combined bounds are allowed in let binding annotations *)

let k : 'a @ [< 'n > 'm] -> 'a @ [< 'm > 'n] = fun x -> x
[%%expect{|
val k : 'a @ [< 'n > 'm] -> 'a @ [< 'm > 'n] = <fun>
|}]

(* Invalid: mode variables are only allowed on function types *)

let (x @ 'm) = fun y -> y
[%%expect{|
Line 1, characters 9-11:
1 | let (x @ 'm) = fun y -> y
             ^^
Error: Mode variables and mode bounds are only allowed on function types.
|}]

let x : int @ [< 'm] = 5
[%%expect{|
Line 1, characters 14-20:
1 | let x : int @ [< 'm] = 5
                  ^^^^^^
Error: Mode variables and mode bounds are only allowed on function types.
|}]
