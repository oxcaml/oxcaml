(* TEST
 flags += "-extension refinements";
 expect;
*)

(* Negative typechecking of theorem declarations (with the extension on). *)

(* A predicate must have type [bool].  Here [sqrt 1.] has type [float], so the
   predicate is rejected. *)
module type S = sig
  val sqrt : float -> float
  thm_? bad : {[ sqrt 1. ]}
end
[%%expect{|
Line 3, characters 17-24:
3 |   thm_? bad : {[ sqrt 1. ]}
                     ^^^^^^^
Error: This expression has type "float" but an expression was expected of type
         "bool"
|}]

(* Refinements are only allowed at theorem argument / result positions.  A
   refinement under a type constructor (here [list]) is rejected. *)
module type C = sig
  thm_? bad : (x : int | x > 0) list -> {[ true ]}
end
[%%expect{|
Line 2, characters 14-31:
2 |   thm_? bad : (x : int | x > 0) list -> {[ true ]}
                  ^^^^^^^^^^^^^^^^^
Error: Refinements are only allowed at theorem argument/result positions
|}]
