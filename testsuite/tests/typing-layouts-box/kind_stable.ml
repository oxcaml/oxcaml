(* TEST
 expect;
*)

(* The [box] operator itself requires only stable layouts; the kind it
   applies to may require more. *)

type t : value box
[%%expect{|
type t : value box
|}]

type t : float64 box
[%%expect{|
type t : float64 box
|}]

type t : bits8 box
[%%expect{|
type t : bits8 box
|}]
