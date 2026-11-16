(* The laws traced by examples_test.ml. *)

(* Clauses: laws with no, one and two assumptions. *)

let double x = 2 * x

law_? trivial : 1 = 1
law_? double_grows (x : int) : x > 0 implies_ double x > x
law_? sum_grows (x : int) (y : int) :
  x > 0 implies_ y > 0 implies_ x + y > x

(* Parameters: types that the laws file must write back in its input types,
   namely type variables with and without kinds, modes, unboxed types and
   aliases, and a keyword as a name. *)

let apply (g : local_ int -> int) = g 1
external box_float : float# -> float = "%box_float"
let of_unboxed x = box_float x
let immutable (_ : ('a : immutable_data)) = true

law_? poly (xs : 'a list) (f : 'a -> 'b) :
  List.length (List.map f xs) = List.length xs
law_? kinded (x : ('a : immutable_data)) : immutable x
law_? modes (g : local_ int -> int) : apply g = apply g
law_? unboxed (x : float#) : of_unboxed x = of_unboxed x
law_? alias (x : (int as 'a)) : x = x
law_? \#type (\#method : int) : \#method = \#method
