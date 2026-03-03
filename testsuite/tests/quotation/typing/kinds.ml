(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Test that quotes in kinds work **)

(* Annotated quote-kinded abstract types *)
type t : <[value]>
[%%expect {|
type t
|}]
type t : <[immediate]>
[%%expect {|
type t : immediate
|}]
type t : <[value mod portable]>
[%%expect {|
type t : value mod portable
|}]
type t : <[<[value]>]>
[%%expect {|
type t
|}]
