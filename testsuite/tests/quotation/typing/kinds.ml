(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Test that quotes in kinds work **)

(* Annotated quote-kinded abstract types *)
type t0 : value
type t1 : <[value]>
type t2 : <[<[value]>]>
[%%expect {|
type t0
type t1 : <[value]>
type t2 : <[<[value]>]>
|}]
type t0 : immediate
type t1 : <[immediate]>
type t2 : <[<[immediate]>]>
[%%expect {|
type t0 : immediate
type t1 : <[immediate]>
type t2 : <[<[immediate]>]>
|}]
type t0 : value & immediate & bits64
type t1 : <[value & immediate & bits64]>
type t2 : <[<[value & immediate & bits64]>]>
[%%expect {|
type t0 : value & value & bits64
type t1 : <[value & value & bits64]>
type t2 : <[<[value & value & bits64]>]>
|}]
type t0 : value mod portable
type t1 : <[value]> mod portable
type t2 : <[<[value]>]> mod portable
[%%expect {|
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
|}]
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
[%%expect {|
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
|}]

(* No quoted kinds in products *)
type t : value & <[value]>
[%%expect {|
Line 1, characters 9-26:
1 | type t : value & <[value]>
             ^^^^^^^^^^^^^^^^^
Error: Quoted kinds cannot occur in products.
|}]
type t : <[value]> & <[value]>
[%%expect {|
Line 1, characters 9-30:
1 | type t : <[value]> & <[value]>
             ^^^^^^^^^^^^^^^^^^^^^
Error: Quoted kinds cannot occur in products.
|}]

(* Annotated quoted types *)
type t0 : value = bytes
type t1 : <[value]> = <[bytes]>
type t2 : <[<[value]>]> = <[<[bytes]>]>
[%%expect {|
type t0 = bytes
Line 2, characters 0-31:
2 | type t1 : <[value]> = <[bytes]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "<[bytes]>" is value
         because it's a staged type.
       But the kind of type "<[bytes]>" must be a subkind of <[value]>
         because of the definition of t1 at line 2, characters 0-31.
|}]
type t0 : immediate = int
type t1 : <[immediate]> = <[int]>
type t2 : <[<[immediate]>]> = <[<[int]>]>
[%%expect {|
type t0 = int
Line 2, characters 0-33:
2 | type t1 : <[immediate]> = <[int]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "<[int]>" is value
         because it's a staged type.
       But the kind of type "<[int]>" must be a subkind of <[immediate]>
         because of the definition of t1 at line 2, characters 0-33.
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
