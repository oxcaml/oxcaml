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
