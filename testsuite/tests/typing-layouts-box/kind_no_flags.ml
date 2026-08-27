(* TEST
 expect;
*)

(* The box operator requires no extension flags *)

type t : value box
[%%expect{|
type t : value box
|}]

type t : bits8 box
[%%expect{|
type t : bits8 box
|}]
