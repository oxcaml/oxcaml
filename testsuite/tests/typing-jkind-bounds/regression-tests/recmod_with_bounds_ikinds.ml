(* TEST
 expect;
*)

(* Regression test: an abstract type with a [with]-bounded kind in a recursive
   module used to be over-approximated to layout [any] during the [module rec]
   approximation pass. *)

type ('a : value) require_value

module rec T1 : sig
  type 'a t : value mod portable with 'a
end =
  T1

and T2 : sig
  type t = { x : int T1.t require_value }
end =
  T2
[%%expect{|
type 'a require_value
Line 9, characters 17-25:
9 |   type t = { x : int T1.t require_value }
                     ^^^^^^^^
Error: This type "int T1.t" should be an instance of type "('a : value)"
       The layout of int T1.t is any
         because the compiler failed to deduce its exact kind
         due to with-bound checking limitations.
       But the layout of int T1.t must be a value layout
         because of the definition of require_value at line 1, characters 0-31.
|}]
