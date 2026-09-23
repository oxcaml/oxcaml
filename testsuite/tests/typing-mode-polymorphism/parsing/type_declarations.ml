(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of polymorphic mode variables and bounds in
 * type declarations
*)

(* Mode variables are allowed on function types in record fields *)

type ('a, 'b) fn = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
[%%expect{|
Line 1, characters 33-35:
1 | type ('a, 'b) fn = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
                                     ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Mode variables are allowed on function types in constructor arguments *)

type ('a, 'b) v = Fn of ('a @ [< 'm] -> 'b @ [> 'm])
[%%expect{|
Line 1, characters 33-35:
1 | type ('a, 'b) v = Fn of ('a @ [< 'm] -> 'b @ [> 'm])
                                     ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Mode variables are allowed on function types in type abbreviations *)

type ('a, 'b) arrow = 'a @ [< 'm] -> 'b @ [> 'm]
[%%expect{|
Line 1, characters 30-32:
1 | type ('a, 'b) arrow = 'a @ [< 'm] -> 'b @ [> 'm]
                                  ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Mode variables are allowed on function types in GADT constructors *)

type ('a, 'b) g = G : ('a @ [< 'm] -> 'b @ [> 'm]) -> ('a, 'b) g
[%%expect{|
type ('a, 'b) g = G : ('a @ [< 'm] -> 'b @ [> 'm]) -> ('a, 'b) g
|}]

