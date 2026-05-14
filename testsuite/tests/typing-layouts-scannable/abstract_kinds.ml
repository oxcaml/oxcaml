(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any non_pointer) require_non_pointer
[%%expect{|
type ('a : any non_pointer) require_non_pointer
|}]

(* [non_pointer] works on kind constructor when it's concrete *)
kind_ k = value
type t : k non_pointer
type check = t require_non_pointer
[%%expect{|
kind_ k = value
type t : value non_pointer
type check = t require_non_pointer
|}]

(* [non_pointer] but not when it's abstract *)
(* CR layouts-scannable: support scannable axes on abstract kinds *)
kind_ k
type t : k non_pointer
type check = t require_non_pointer
[%%expect{|
kind_ k
Line 2, characters 11-22:
2 | type t : k non_pointer
               ^^^^^^^^^^^
Error: Abstract kinds with kind modifiers are not yet supported.
|}]
