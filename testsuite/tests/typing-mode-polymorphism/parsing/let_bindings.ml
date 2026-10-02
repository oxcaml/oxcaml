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
|}, Principal{|
val f :
  'a @ [< global many portable uncontended forkable unyielding stateless read_write] ->
  'a @ [> aliased nonportable contended stateful immutable dynamic] = <fun>
|}]

let i = (fun x -> x : 'a @ [< 'm] -> 'a @ [> 'm])
[%%expect{|
val i : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}, Principal{|
val i :
  'a @ [< global many portable uncontended forkable unyielding stateless read_write] ->
  'a @ [> aliased nonportable contended stateful immutable dynamic] = <fun>
|}]

(* Constant bounds are allowed in let binding annotations *)

let j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = fun x -> x
[%%expect{|
val j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}, Principal{|
val j :
  'a @ [< global many portable uncontended forkable unyielding stateless read_write] ->
  'a @ [> aliased nonportable contended stateful immutable dynamic] = <fun>
|}]

(* Combined bounds are allowed in let binding annotations *)

let k : 'a @ [< 'n > 'm] -> 'a @ [< 'm > 'n] = fun x -> x
[%%expect{|
val k : 'a @ [< 'n > 'm] -> 'a @ [< 'm > 'n] = <fun>
|}, Principal{|
val k :
  'a @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] ->
  'a @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] =
  <fun>
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

(* Mode variables are scoped like type variables *)

let f (_ : unit -> 'a @ 'm) : unit -> 'a @ 'm = fun () -> ""
[%%expect{|
val f :
  (unit -> string @ [< 'm > 'n]) @ 'p -> (unit -> string @ [< 'n > 'm]) @ 'o =
  <fun>
|}]
