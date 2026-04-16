(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* CR-soon quoted-kinds jbachurski: Many kind annotations that appear
   in these tests should be quoted (e.g. <[value]> as opposed to value). *)

(** Expr kinding **)

type t : value = <[bytes]> expr
[%%expect {|
type t = <[bytes]> expr
|}]

type t : value = <[int]> expr
[%%expect {|
type t = <[int]> expr
|}]

type t : value = <[int64#]> expr
[%%expect {|
type t = <[int64#]> expr
|}]

type t : value = <[#(bytes * float#)]> expr
[%%expect {|
type t = <[#(bytes * float#)]> expr
|}]

(* CR quoted-kinds jbachurski: This should probably be ill-kinded. *)
type t : value = int expr
[%%expect {|
type t = int expr
|}]

(* CR quoted-kinds jbachurski: Likewise probably ill-kinded. *)
type t : value = <[<[int]>]> expr
[%%expect {|
type t = <[<[int]>]> expr
|}]

(** Quote kinding **)

type t : value = <[bytes]>
[%%expect {|
type t = <[bytes]>
|}]

type t : immediate = <[int]>
[%%expect {|
type t = <[int]>
|}]

type t : bits64 = <[int64#]>
[%%expect {|
type t = <[int64#]>
|}]

type t : value & float64 = <[#(bytes * float#)]>
[%%expect {|
type t = <[#(bytes * float#)]>
|}]

type t : value = <[int64#]> (* should error! *)
[%%expect {|
Line 1, characters 0-27:
1 | type t : value = <[int64#]> (* should error! *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "<[int64#]>" is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of type "<[int64#]>" must be a sublayout of value
         because of the definition of t at line 1, characters 0-27.
|}]

(** Splice kinding **)

type t : bits64 = <[$(int64#)]>
[%%expect {|
type t = int64#
|}]

type ('a : float64) t : value & float64 = <[#(bytes * $('a))]>
[%%expect {|
type ('a : float64) t = <[#(bytes * $('a))]>
|}]

type t : value = <[$(int64#)]> (* should error! *)
[%%expect {|
Line 1, characters 0-30:
1 | type t : value = <[$(int64#)]> (* should error! *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "int64#" is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of type "int64#" must be a sublayout of value
         because of the definition of t at line 1, characters 0-30.
|}]

(** Eval kinding **)

type i64 : bits64

type t : bits64 = i64 eval
[%%expect {|
type i64 : bits64
type t = i64 eval
|}]

type ('a : float64) t : float64 = 'a eval
[%%expect {|
type ('a : float64) t = 'a eval
|}]

type ('a : value & float64) t : value & float64 = 'a eval
[%%expect {|
type ('a : value & float64) t = 'a eval
|}]

type t : value = i64 eval (* should error! *)
[%%expect {|
Line 1, characters 0-25:
1 | type t : value = i64 eval (* should error! *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "i64 eval" is bits64
         because of the definition of i64 at line 3, characters 0-17.
       But the layout of type "i64 eval" must be a sublayout of value
         because of the definition of t at line 1, characters 0-25.
|}]
