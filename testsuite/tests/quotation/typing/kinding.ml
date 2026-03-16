(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* CR quoted-kinds jbachurski: A lot of the kinds that appear in these tests
   should be quoted. *)

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
Line 1, characters 0-28:
1 | type t : immediate = <[int]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "<[int]>" is value
         because it's a staged type.
       But the kind of type "<[int]>" must be a subkind of immediate
         because of the definition of t at line 1, characters 0-28.
|}]

type t : bits64 = <[int64#]>
[%%expect {|
Line 1, characters 0-28:
1 | type t : bits64 = <[int64#]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "<[int64#]>" is value
         because it's a staged type.
       But the layout of type "<[int64#]>" must be a sublayout of bits64
         because of the definition of t at line 1, characters 0-28.
|}]

type t : value & float64 = <[#(bytes * float#)]>
[%%expect {|
Line 1, characters 0-48:
1 | type t : value & float64 = <[#(bytes * float#)]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "<[#(bytes * float#)]>" is value
         because it's a staged type.
       But the layout of type "<[#(bytes * float#)]>" must be a sublayout of
           value & float64
         because of the definition of t at line 1, characters 0-48.
|}]

type t : value = <[int64#]> (* error! *)
[%%expect {|
type t = <[int64#]>
|}]

(** Splice kinding **)

type t : bits64 = <[$(int64#)]>
[%%expect {|
Line 3, characters 0-31:
3 | type t : bits64 = <[$(int64#)]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "int64#" is value
         because it's a staged type.
       But the layout of type "int64#" must be a sublayout of bits64
         because of the definition of t at line 3, characters 0-31.
|}]

type ('a : float64) t : value & float64 = <[#(bytes * $('a))]>
[%%expect {|
Line 1, characters 0-62:
1 | type ('a : float64) t : value & float64 = <[#(bytes * $('a))]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "<[#(bytes * $('a))]>" is value
         because it's a staged type.
       But the layout of type "<[#(bytes * $('a))]>" must be a sublayout of
           value & float64
         because of the definition of t at line 1, characters 0-62.
|}]

type t : value = <[$(int64#)]> (* error! *)
[%%expect {|
type t = int64#
|}]

(** Eval kinding **)

type i64 : bits64

type t : bits64 = i64 eval
[%%expect {|
type i64 : bits64
Line 5, characters 18-21:
5 | type t : bits64 = i64 eval
                      ^^^
Error: This type "i64" should be an instance of type "('a : value)"
       The layout of i64 is bits64
         because of the definition of i64 at line 3, characters 0-17.
       But the layout of i64 must be a sublayout of value
         because the type argument of eval has layout value.
|}]

type ('a : float64) t : float64 = 'a eval
[%%expect {|
Line 1, characters 34-36:
1 | type ('a : float64) t : float64 = 'a eval
                                      ^^
Error: This type "('a : float64)" should be an instance of type "('b : value)"
       The layout of 'a is float64
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because the type argument of eval has layout value.
|}]

type ('a : value & float64) t : value & float64 = 'a eval
[%%expect {|
Line 1, characters 50-52:
1 | type ('a : value & float64) t : value & float64 = 'a eval
                                                      ^^
Error: This type "('a : value & float64)" should be an instance of type
         "('b : value)"
       The layout of 'a is value & float64
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because the type argument of eval has layout value.
|}]

type t : value = i64 eval (* error! *)
[%%expect {|
Line 1, characters 17-20:
1 | type t : value = i64 eval (* error! *)
                     ^^^
Error: This type "i64" should be an instance of type "('a : value)"
       The layout of i64 is bits64
         because of the definition of i64 at line 3, characters 0-17.
       But the layout of i64 must be a sublayout of value
         because the type argument of eval has layout value.
|}]
