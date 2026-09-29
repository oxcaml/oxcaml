(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests parsing of polymorphic mode variables and bounds in
 * type declarations
*)

(* Unbound mode variables in record fields cause an error *)

type ('a, 'b) fn = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
[%%expect{|
Line 1, characters 33-35:
1 | type ('a, 'b) fn = { f : 'a @ [< 'm] -> 'b @ [> 'm] }
                                     ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Unbound mode variables in constructor arguments cause an error *)

type ('a, 'b) v = Fn of ('a @ [< 'm] -> 'b @ [> 'm])
[%%expect{|
Line 1, characters 33-35:
1 | type ('a, 'b) v = Fn of ('a @ [< 'm] -> 'b @ [> 'm])
                                     ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Unbound mode variables in type abbreviations cause an error *)

type ('a, 'b) arrow = 'a @ [< 'm] -> 'b @ [> 'm]
[%%expect{|
Line 1, characters 30-32:
1 | type ('a, 'b) arrow = 'a @ [< 'm] -> 'b @ [> 'm]
                                  ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}]

(* Unbound mode variables in GADT constructors are implicitly quantified *)

type (_, _) g = G : ('a @ [< 'm] -> 'b @ [> 'm]) -> ('a, 'b) g
[%%expect{|
type (_, _) g = G : ('a @ [< 'm] -> 'b @ [> 'm]) -> ('a, 'b) g
|}]

(* Unbound mode variables in constraints are implicitly quantified *)

type ghost
type ('a, 'nonsense) t = 'a @ [< 'm] -> 'a @ [> 'm]
  constraint 'nonsense = ghost -> unit @ 'm
[%%expect{|
type ghost
type ('a, 'b) t = 'a @ [< 'o & 'n] -> 'a @ [> 'o | 'm]
  constraint 'b = ghost -> unit @ [< 'm > 'n]
|}]
