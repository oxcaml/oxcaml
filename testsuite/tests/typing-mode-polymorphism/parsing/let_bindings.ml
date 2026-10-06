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

(* CR ageorges: The following two examples have bad principal types. When looking at their
   underlying graphs, this is due to an ill-leveled mode graph, which can result in
   bad zapping for printing.

   The ill-leveled graph is due to to a bug in the typing of let: In principal mode, the
   typing of let stores a type which has a generalized structure into the environment
   The type is then instantiated, inference continues, and finally the copy is fully
   generalized. However, this does not yield the same modes as generalizing the original
   type in the environment, and leads to an ill-leveled exposed mode graph. *)

let j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = fun x -> x
[%%expect{|
val j : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}, Principal{|
val j :
  'a @ [< 'm & global many portable forkable unyielding stateless] ->
  'a @ [> 'm | nonportable stateful] = <fun>
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
