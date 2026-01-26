(* TEST
 flags = "-extension small_numbers -ikinds";
 expect;
*)

type t = { x : float } [@@unboxed]

[%%expect{|
type t = { x : float; } [@@unboxed]
|}]
