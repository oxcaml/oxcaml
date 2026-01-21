(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type t = { x : float } [@@unboxed]

[%%expect{|
type t = { x : float; } [@@unboxed]
|}]
