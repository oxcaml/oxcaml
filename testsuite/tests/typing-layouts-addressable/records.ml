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
type t = #{ a : int8#; }
|}]

type t : bits8 = #{ a : int8# }
[%%expect{|
Line 1, characters 0-31:
1 | type t : bits8 = #{ a : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8 addressable
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8
         because of the annotation on the declaration of the type t.
|}]

type t : float64 addressable = #{ f : float# }
[%%expect{|
type t = #{ f : float#; }
|}]

type t : float64 = #{ f : float# }
[%%expect{|
Line 1, characters 0-34:
1 | type t : float64 = #{ f : float# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is float64 addressable
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of float64
         because of the annotation on the declaration of the type t.
|}]

type t : void addressable = #{ u : unit# }
[%%expect{|
type t = #{ u : unit#; }
|}]

type t : void = #{ u : unit# }
[%%expect{|
Line 1, characters 0-30:
1 | type t : void = #{ u : unit# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is void addressable
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of void
         because of the annotation on the declaration of the type t.
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
type t' = #{ t : t; }
|}]

(* ... in the same recursive group *)
type t = #{ a : int8# }
and u = #{ t : t }
type ok = u req8a
[%%expect{|
type t = #{ a : int8#; }
and u = #{ t : t; }
type ok = u req8a
|}]

(* The kind is printed on the unboxed record declaration only when annotated,
   so we observe it through requirements. *)
type t = #{ a : int8# }
type ok = t req
type ok = t req8a
[%%expect{|
type t = #{ a : int8#; }
type ok = t req
type ok = t req8a
|}]

type bad = t req8
[%%expect{|
Line 1, characters 11-12:
1 | type bad = t req8
               ^
Error: This type "t" should be an instance of type "('a : bits8)"
       The layout of t is bits8 addressable
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of bits8
         because of the definition of req8 at line 2, characters 0-22.
|}]

(* Mode crossing is unaffected *)
module M : sig
  type t : bits8 addressable mod everything
end = struct
  type t = #{ a : int8# }
end
[%%expect{|
module M : sig type t : (bits8 mod everything) addressable end
|}]

type ('a : bits8) t = #{ a : 'a }
module M : sig
  type ('a : bits8) t : bits8 addressable mod everything with 'a
end = struct
  type ('a : bits8) t = #{ a : 'a }
end
[%%expect{|
type ('a : bits8) t = #{ a : 'a; }
module M :
  sig type ('a : bits8) t : (bits8 mod everything) addressable with 'a end
|}]

(**** The unboxed version of a boxed record ****)

type r = { a : int8# }
type ok = r# req8a
[%%expect{|
type r = { a : int8#; }
type ok = r# req8a
|}]

type bad = r# req8
[%%expect{|
Line 1, characters 11-13:
1 | type bad = r# req8
               ^^
Error: This type "r#" should be an instance of type "('a : bits8)"
       The layout of r# is bits8 addressable
         because it is an unboxed record.
       But the layout of r# must be a sublayout of bits8
         because of the definition of req8 at line 2, characters 0-22.
|}]

type r = { f : float# }
type ok : float64 addressable = r#
[%%expect{|
type r = { f : float#; }
type ok = r#
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
module M : sig type ('a : any) t : any addressable end
|}]

type ok = int8# t req8a
type ok = int8# t req
type ok = int t req
[%%expect{|
type ok = int8# t req8a
type ok = int8# t req
type ok = int t req
|}]

type bad = int8# t req8
[%%expect{|
Line 1, characters 11-18:
1 | type bad = int8# t req8
               ^^^^^^^
Error: This type "int8# t" should be an instance of type "('a : bits8)"
       The layout of int8# t is any addressable
         because of the definition of t at line 1, characters 0-31.
       But the layout of int8# t must be a sublayout of bits8
         because of the definition of req8 at line 2, characters 0-22.
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
val f : ('a : bits8). 'a t -> 'a t = <fun>
|}]

let f (x : ('a : bits8 addressable) t) = (x : ('b : bits8 addressable))
[%%expect{|
val f : ('a : bits8 addressable). 'a t -> 'a t = <fun>
|}]

let f (x : 'a t) = (x : ('b : bits8))
[%%expect{|
Line 1, characters 20-21:
1 | let f (x : 'a t) = (x : ('b : bits8))
                        ^
Error: The value "x" has type "'a t" but an expression was expected of type
         "('b : bits8)"
       The layout of 'a t is any addressable
         because of the definition of t at line 1, characters 0-31.
       But the layout of 'a t must be a sublayout of bits8
         because of the annotation on the type variable 'b.
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
val f : t -> t = <fun>
|}]

let f (x : t) : ('b : bits8) = x
[%%expect{|
Line 1, characters 31-32:
1 | let f (x : t) : ('b : bits8) = x
                                   ^
Error: The value "x" has type "t" but an expression was expected of type
         "('b : bits8)"
       The layout of t is bits8 addressable
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of bits8
         because of the annotation on the type variable 'b.
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
val f : t -> t = <fun>
|}]

type r = { a : int8# }
let f (x : r#) = id_addressable x
[%%expect{|
type r = { a : int8#; }
val f : r# -> r# = <fun>
|}]

(**** Externals ****)

(* Addressability does not change the unboxed representation, so a singleton
   record can still be passed to C as its field *)
type t = #{ f : float# }
external f : (t [@unboxed]) -> (float [@unboxed]) = "foo" "bar"
[%%expect{|
type t = #{ f : float#; }
external f : t -> (float [@unboxed]) = "foo" "bar"
|}]

type t = #{ i : int32_u }
external f : (t [@unboxed]) -> (int32 [@unboxed]) = "foo" "bar"
[%%expect{|
type t = #{ i : int32_u; }
external f : t -> (int32 [@unboxed]) = "foo" "bar"
|}]

type t = #{ u : unit# }
external f : (t [@unboxed]) -> int = "foo" "bar"
[%%expect{|
type t = #{ u : unit#; }
Line 2, characters 14-15:
2 | external f : (t [@unboxed]) -> int = "foo" "bar"
                  ^
Error: Don't know how to unbox this type.
       Only "float", "int8", "int16", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
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
