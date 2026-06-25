(* TEST
 flags += "-extension refinements";
 expect;
*)

(* Positive typechecking of theorem declarations.  Refinements are erased to
   their base types; the resulting value type is discharged by an ordinary
   [let] in the structure.  Predicates are typechecked against [bool] in a
   left-to-right dependent-arrow scope (each argument binder is in scope in
   its own predicate, in later argument predicates, and in the result
   predicate). *)

(* A module signature with a value and two theorems about it.  The erased
   types are [unit] (for [sqrt_1]) and [float -> unit] (for [small]); the
   plain [let]s in the structure discharge them. *)
module M : sig
  val sqrt : float -> float
  thm_? sqrt_1 : {[ sqrt 1. = 1. ]}
  thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}
end = struct
  let sqrt x = x
  let sqrt_1 = ()
  let small _ = ()
end
[%%expect{|
module M :
  sig
    val sqrt : float -> float
    thm_? sqrt_1 : unit
    thm_? small : float -> unit
  end
|}]

(* The dependent-arrow binder is in scope in its own predicate and in the
   result predicate. *)
module type T = sig
  thm_? t1 : (x : int | x > 0) -> {[ x >= 0 ]}
end
[%%expect{|
module type T = sig thm_? t1 : int -> unit end
|}]

(* The result binder from [(y : R | q)] is in scope in [q], and earlier
   argument binders are visible there too. *)
module type R = sig
  thm_? r1 : (x : int | x > 0) -> (y : int | y > x)
end
[%%expect{|
module type R = sig thm_? r1 : int -> int end
|}]
