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
type t : k
Line 3, characters 13-14:
3 | type check = t require_non_pointer
                 ^
Error: This type "t" should be an instance of type "('a : any non_pointer)"
       The kind of t is k
         because of the definition of t at line 2, characters 0-22.
       But the kind of t must be a subkind of any non_pointer
         because of the definition of require_non_pointer at line 1, characters 0-47.
|}]
