(* TEST
 expect;
*)

(* The [addressable] operator itself requires only stable layouts; the kind
   it applies to may require more. *)

type t : bits64 addressable [@@warning "-183"]
[%%expect{|
type t : bits64
|}]

type t : value addressable [@@warning "-183"]
[%%expect{|
type t
|}]

type t : bits8 addressable
[%%expect{|
type t : bits8 addressable
|}]
