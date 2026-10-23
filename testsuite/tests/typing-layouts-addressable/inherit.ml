(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* [inherit] fields are not made addressable *)

type ('a : any addressable) req
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
[%%expect{|
type ('a : any addressable) req
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
|}]

(**** The sole field of an unboxed record ****)

type t : bits8 = #{ inherit a : int8# }
[%%expect{|
type t = #{ inherit a : int8#; }
|}]

type t : bits8 addressable = #{ inherit a : int8# }
[%%expect{|
Line 1, characters 0-51:
1 | type t : bits8 addressable = #{ inherit a : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : void = #{ inherit u : unit# }
[%%expect{|
type t = #{ inherit u : unit#; }
|}]

(* On an addressable field, [inherit] changes nothing *)
type t : bits64 = #{ inherit i : int64_u }
type t : value = #{ inherit s : string }
type t : bits8 & bits16 = #{ inherit p : #(int8# * int16#) }
[%%expect{|
type t = #{ inherit i : int64_u; }
type t = #{ inherit s : string; }
type t = #{ inherit p : #(int8# * int16#); }
|}]

type t = #{ inherit a : int8# @@ portable }
[%%expect{|
type t = #{ inherit a : int8# @@ portable; }
|}]

(* Fields of unknown kind *)
type ('a : any) t = #{ inherit a : 'a }
module M : sig
  type ('a : any) t : any
end = struct
  type ('a : any) t = #{ inherit a : 'a }
end
[%%expect{|
type ('a : any) t = #{ inherit a : 'a; }
module M : sig type ('a : any) t : any end
|}]

type ok = int8# t req8
[%%expect{|
type ok = int8# t req8
|}]

type bad = int8# t req8a
[%%expect{|
Line 1, characters 11-18:
1 | type bad = int8# t req8a
               ^^^^^^^
Error: This type "int8# t" should be an instance of type
         "('a : bits8 addressable)"
       The layout of int8# t is bits8
         because of the definition of t at line 1, characters 0-39.
       But the layout of int8# t must be a sublayout of bits8 addressable
         because of the definition of req8a at line 3, characters 0-35.
|}]

let f (x : 'a t) = (x : ('b : bits8))
[%%expect{|
val f : ('a : bits8). 'a t -> 'a t = <fun>
|}]

let f (x : 'a t) = (x : ('b : bits8 addressable))
[%%expect{|
val f : ('a : bits8 addressable). 'a t -> 'a t = <fun>
|}]

(* Expressions *)
type t = #{ inherit a : int8# }
let f (x : t) : ('b : bits8) = x
[%%expect{|
type t = #{ inherit a : int8#; }
val f : t -> t = <fun>
|}]

let f (x : t) : ('b : bits8 addressable) = x
[%%expect{|
Line 1, characters 43-44:
1 | let f (x : t) : ('b : bits8 addressable) = x
                                               ^
Error: The value "x" has type "t" but an expression was expected of type
         "('b : bits8 addressable)"
       The layout of t is bits8
         because of the definition of t at line 1, characters 0-31.
       But the layout of t must be a sublayout of bits8 addressable
         because of the annotation on the type variable 'b.
|}]

(* Nested *)
type u : bits8 = #{ inherit t : t }
type v : bits8 addressable = #{ t : t }
[%%expect{|
type u = #{ inherit t : t; }
type v = #{ t : t; }
|}]

(**** [inherit] must agree between a signature and its implementation ****)

module M : sig
  type t = #{ inherit a : int8# }
end = struct
  type t = #{ a : int8# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ a : int8# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ a : int8#; } end
       is not included in
         sig type t = #{ inherit a : int8#; } end
       Type declarations do not match:
         type t = #{ a : int8#; }
       is not included in
         type t = #{ inherit a : int8#; }
       Fields do not match:
         "a : int8#;"
       is not the same as:
         "inherit a : int8#;"
       The second is inherit and the first is not.
|}]

type t = #{ a : int8# }
type u = t = #{ inherit a : int8# }
[%%expect{|
type t = #{ a : int8#; }
Line 2, characters 0-35:
2 | type u = t = #{ inherit a : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t"
       Fields do not match:
         "a : int8#;"
       is not the same as:
         "inherit a : int8#;"
       This is inherit and the original is not.
|}]

(**** [inherit] is only supported on the sole field of an unboxed record ****)

type t = #{ inherit a : int8#; b : int }
[%%expect{|
Line 1, characters 12-30:
1 | type t = #{ inherit a : int8#; b : int }
                ^^^^^^^^^^^^^^^^^^
Error: "inherit" is only supported on the sole field of an unboxed record
|}]

type t = { inherit a : int8# }
[%%expect{|
Line 1, characters 11-28:
1 | type t = { inherit a : int8# }
               ^^^^^^^^^^^^^^^^^
Error: "inherit" is only supported on the sole field of an unboxed record
|}]

type t = A of { inherit a : int8# }
[%%expect{|
Line 1, characters 16-33:
1 | type t = A of { inherit a : int8# }
                    ^^^^^^^^^^^^^^^^^
Error: "inherit" is only supported on the sole field of an unboxed record
|}]

type t = ..
type t += A of { inherit a : int8# }
[%%expect{|
type t = ..
Line 2, characters 17-34:
2 | type t += A of { inherit a : int8# }
                     ^^^^^^^^^^^^^^^^^
Error: "inherit" is only supported on the sole field of an unboxed record
|}]

(* A mutable field must be addressable *)
type t = #{ inherit mutable a : int8# }
[%%expect{|
Line 1, characters 12-37:
1 | type t = #{ inherit mutable a : int8# }
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "inherit" fields cannot be mutable
|}]

type t = #{ inherit a : int8# [@atomic] }
[%%expect{|
Line 1, characters 12-39:
1 | type t = #{ inherit a : int8# [@atomic] }
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The label "a" must be mutable to be declared atomic.
|}]
