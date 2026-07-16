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
module rec T1 : sig type 'a t : value mod portable with 'a end
and T2 : sig type t = { x : int T1.t require_value; } end
|}]

(* The layout also stays precise when the [with]-bounded kind is spelled with
   an abbreviation that carries its own bounds. *)
module rec T1 : sig
  type 'a t : immutable_data with 'a
end =
  T1

and T2 : sig
  type t = { x : int T1.t require_value }
end =
  T2
[%%expect{|
module rec T1 : sig type 'a t : immutable_data with 'a end
and T2 : sig type t = { x : int T1.t require_value; } end
|}]

(* CR layouts v2.8: fix this. Internal ticket 5127 *)
module rec T1 : sig
  type 'a t = A
end =
  T1

and T2 : sig
  type t : value mod portable = { x : int T1.t }
end =
  T2
[%%expect{|
Line 7, characters 2-48:
7 |   type t : value mod portable = { x : int T1.t }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation value mod portable,
       because T1.t is not mod portable.
|}]
