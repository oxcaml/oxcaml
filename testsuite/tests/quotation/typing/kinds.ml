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
type t : <[value mod portable]>
|}]
type t : <[<[value]>]>
[%%expect {|
type t
|}]

(* Annotated quoted types *)
type t : <[value]> = <[bytes]>
[%%expect {|
type t = <[bytes]>
|}]
type t : <[immediate]> = <[int]>
[%%expect {|
Line 1, characters 0-32:
1 | type t : <[immediate]> = <[int]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "<[int]>" is value
         because it's a staged type.
       But the kind of type "<[int]>" must be a subkind of immediate
         because of the definition of t at line 1, characters 0-32.
|}]
type t : <[<[value]>]> = <[<[bytes]>]>
[%%expect {|
type t = <[<[bytes]>]>
|}]

(* Unannotated quoted types *)
type t = <[bytes]>
[%%expect {|
type t = <[bytes]>
|}]
type t = <[int]>
[%%expect {|
type t = <[int]>
|}]
type t = <[<[bytes]>]>
[%%expect {|
type t = <[<[bytes]>]>
|}]
