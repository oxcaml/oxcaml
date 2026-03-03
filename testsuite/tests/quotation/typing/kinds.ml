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

(* Annotated quoted types *)
type t : <[value]> = <[bytes]>
[%%expect {|
|}]
type t : <[immediate]> = <[int]>
[%%expect {|
|}]
type t : <[<[value]>]> = <[<[bytes]>]>
[%%expect {|
|}]

(* Unannotated quoted types *)
type t = <[bytes]>
[%%expect {|
|}]
type t = <[int]>
[%%expect {|
|}]
type t = <[<[bytes]>]>
[%%expect {|
|}]
