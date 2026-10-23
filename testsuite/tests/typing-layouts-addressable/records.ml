(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Records and unboxed records make their fields addressable. For unboxed
   records with several fields this is implied by [&]; this file is mostly
   about singleton records, whose kind is the field's kind made
   addressable. *)

type ('a : any addressable) req
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
[%%expect{|
type ('a : any addressable) req
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
|}]

(**** Singleton unboxed records ****)

type t : bits8 addressable = #{ a : int8# }
[%%expect{|
Line 1, characters 0-43:
1 | type t : bits8 addressable = #{ a : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : bits8 = #{ a : int8# }
[%%expect{|
type t = #{ a : int8#; }
|}]

type t : float64 addressable = #{ f : float# }
[%%expect{|
Line 1, characters 0-46:
1 | type t : float64 addressable = #{ f : float# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is float64
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of float64 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : float64 = #{ f : float# }
[%%expect{|
type t = #{ f : float#; }
|}]

type t : void addressable = #{ u : unit# }
[%%expect{|
Line 1, characters 0-42:
1 | type t : void addressable = #{ u : unit# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is void
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of void addressable
         because of the annotation on the declaration of the type t.
|}]

type t : void = #{ u : unit# }
[%%expect{|
type t = #{ u : unit#; }
|}]

(* Already-addressable fields are unchanged *)
type t : bits64 = #{ i : int64_u }
type t : value = #{ s : string }
type t : bits8 & bits16 = #{ p : #(int8# * int16#) }
[%%expect{|
type t = #{ i : int64_u; }
type t = #{ s : string; }
type t = #{ p : #(int8# * int16#); }
|}]

(* Nested singletons *)
type t = #{ a : int8# }
type t' : bits8 addressable = #{ t : t }
[%%expect{|
type t = #{ a : int8#; }
Line 2, characters 0-40:
2 | type t' : bits8 addressable = #{ t : t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t'" is bits8
         because it is an unboxed record.
       But the layout of type "t'" must be a sublayout of bits8 addressable
         because of the annotation on the declaration of the type t'.
|}]

(* ... in the same recursive group *)
type t = #{ a : int8# }
and u = #{ t : t }
type ok = u req8a
[%%expect{|
type t = #{ a : int8#; }
and u = #{ t : t; }
Line 3, characters 10-11:
3 | type ok = u req8a
              ^
Error: This type "u" should be an instance of type "('a : bits8 addressable)"
       The layout of u is bits8
         because of the definition of u at line 2, characters 0-18.
       But the layout of u must be a sublayout of bits8 addressable
         because of the definition of req8a at line 3, characters 0-35.
|}]

(* The kind is printed on the unboxed record declaration only when annotated,
   so we observe it through requirements. *)
type t = #{ a : int8# }
type ok = t req
type ok = t req8a
[%%expect{|
type t = #{ a : int8#; }
Line 2, characters 10-11:
2 | type ok = t req
              ^
Error: This type "t" should be an instance of type "('a : any addressable)"
       The layout of t is bits8
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type bad = t req8
[%%expect{|
type bad = t req8
|}]

(* Mode crossing is unaffected *)
module M : sig
  type t : bits8 addressable mod everything
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
         sig type t : (bits8 mod everything) addressable end
       Type declarations do not match:
         type t = #{ a : int8#; }
       is not included in
         type t : (bits8 mod everything) addressable
       The layout of the first is bits8
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 2, characters 2-43.
|}]

type ('a : bits8) t = #{ a : 'a }
module M : sig
  type ('a : bits8) t : bits8 addressable mod everything with 'a
end = struct
  type ('a : bits8) t = #{ a : 'a }
end
[%%expect{|
type ('a : bits8) t = #{ a : 'a; }
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type ('a : bits8) t = #{ a : 'a }
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : bits8) t = #{ a : 'a; } end
       is not included in
         sig
           type ('a : bits8) t : (bits8 mod everything) addressable with 'a
         end
       Type declarations do not match:
         type ('a : bits8) t = #{ a : 'a; }
       is not included in
         type ('a : bits8) t : (bits8 mod everything) addressable with 'a
       The layout of the first is bits8
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 3, characters 2-64.
|}]

(**** The unboxed version of a boxed record ****)

type r = { a : int8# }
type ok = r# req8a
[%%expect{|
type r = { a : int8#; }
Line 2, characters 10-12:
2 | type ok = r# req8a
              ^^
Error: This type "r#" should be an instance of type "('a : bits8 addressable)"
       The layout of r# is bits8
         because it is an unboxed record.
       But the layout of r# must be a sublayout of bits8 addressable
         because of the definition of req8a at line 3, characters 0-35.
|}]

type bad = r# req8
[%%expect{|
type bad = r# req8
|}]

type r = { f : float# }
type ok : float64 addressable = r#
[%%expect{|
type r = { f : float#; }
Line 2, characters 0-34:
2 | type ok : float64 addressable = r#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r#" is float64
         because it is an unboxed record.
       But the layout of type "r#" must be a sublayout of float64 addressable
         because of the definition of ok at line 2, characters 0-34.
|}]

type r = { s : string }
type ok : value = r#
[%%expect{|
type r = { s : string; }
type ok = r#
|}]

(**** Fields of unknown kind ****)

type ('a : any) t = #{ a : 'a }
[%%expect{|
type ('a : any) t = #{ a : 'a; }
|}]

module M : sig
  type ('a : any) t : any addressable
end = struct
  type ('a : any) t = #{ a : 'a }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : any) t = #{ a : 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : any) t = #{ a : 'a; } end
       is not included in
         sig type ('a : any) t : any addressable end
       Type declarations do not match:
         type ('a : any) t = #{ a : 'a; }
       is not included in
         type ('a : any) t : any addressable
       The layout of the first is any
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of any addressable
         because of the definition of t at line 2, characters 2-37.
|}]

type ok = int8# t req8a
type ok = int8# t req
type ok = int t req
[%%expect{|
Line 1, characters 10-17:
1 | type ok = int8# t req8a
              ^^^^^^^
Error: This type "int8# t" should be an instance of type
         "('a : bits8 addressable)"
       The layout of int8# t is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# t must be a sublayout of bits8 addressable
         because of the definition of req8a at line 3, characters 0-35.
|}]

type bad = int8# t req8
[%%expect{|
type bad = int8# t req8
|}]

type t8a : bits8 addressable
type ok = t8a t req8a
[%%expect{|
type t8a : bits8 addressable
type ok = t8a t req8a
|}]

(* Inferring the field's kind from the record's kind picks the plain kind *)
let f (x : 'a t) = (x : ('b : bits8 addressable))
[%%expect{|
val f : ('a : bits8 addressable). 'a t -> 'a t = <fun>
|}]

let f (x : ('a : bits8 addressable) t) = (x : ('b : bits8 addressable))
[%%expect{|
val f : ('a : bits8 addressable). 'a t -> 'a t = <fun>
|}]

let f (x : 'a t) = (x : ('b : bits8))
[%%expect{|
val f : ('a : bits8). 'a t -> 'a t = <fun>
|}]

external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
let f (x : 'a t) = id x
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val f : 'a t -> 'a t = <fun>
|}]

external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
let f (x : 'a t) = id_addressable x
[%%expect{|
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
val f : 'a t -> 'a t = <fun>
|}]

(**** Expressions ****)

type t = #{ a : int8# }
let f (x : t) : ('b : bits8 addressable) = x
[%%expect{|
type t = #{ a : int8#; }
Line 2, characters 43-44:
2 | let f (x : t) : ('b : bits8 addressable) = x
                                               ^
Error: The value "x" has type "t" but an expression was expected of type
         "('b : bits8 addressable)"
       The layout of t is bits8
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of bits8 addressable
         because of the annotation on the type variable 'b.
|}]

let f (x : t) : ('b : bits8) = x
[%%expect{|
val f : t -> t = <fun>
|}]

let f (x : t) : ('b : bits8) = x.#a
[%%expect{|
val f : t -> int8# = <fun>
|}]

let f (#{ a } : t) : ('b : bits8) = a
[%%expect{|
val f : t -> int8# = <fun>
|}]

let f (x : int8#) : t = #{ a = x }
[%%expect{|
val f : int8# -> t = <fun>
|}]

let f (x : t) = id x
[%%expect{|
val f : t -> t = <fun>
|}]

let f (x : t) = id_addressable x
[%%expect{|
Line 1, characters 31-32:
1 | let f (x : t) = id_addressable x
                                   ^
Error: The value "x" has type "t" but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of t is bits8
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be addressable
         because we must know concretely how to return a function result.
|}, Principal{|
Line 1, characters 31-32:
1 | let f (x : t) = id_addressable x
                                   ^
Error: The value "x" has type "t" but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of t is bits8
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

type r = { a : int8# }
let f (x : r#) = id_addressable x
[%%expect{|
type r = { a : int8#; }
Line 2, characters 32-33:
2 | let f (x : r#) = id_addressable x
                                    ^
Error: The value "x" has type "r#" but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of r# is bits8
         because it is an unboxed record.
       But the layout of r# must be addressable
         because we must know concretely how to return a function result.
|}, Principal{|
type r = { a : int8#; }
Line 2, characters 32-33:
2 | let f (x : r#) = id_addressable x
                                    ^
Error: The value "x" has type "r#" but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of r# is bits8
         because it is an unboxed record.
       But the layout of r# must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(**** Boxed records ****)

(* The boxed record's own kind is unchanged *)
module M : sig
  type r : immutable_data
end = struct
  type r = { a : int8# }
end
[%%expect{|
module M : sig type r : immutable_data end
|}]

(* [@@unboxed] types do not (yet) make their contents addressable *)
type u : bits8 = { a : int8# } [@@unboxed]
[%%expect{|
type u = { a : int8#; } [@@unboxed]
|}]
