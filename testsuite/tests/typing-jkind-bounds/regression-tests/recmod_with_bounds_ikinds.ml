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
Error: The kind of type "t" is immutable_data with int T1.t
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]
