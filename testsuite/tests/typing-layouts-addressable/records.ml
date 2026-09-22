(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(**** Records make their fields addressable ****)

(* A record with one field has the kind of that field made addressable *)
type t : bits8 addressable = #{ i : int8# }
[%%expect{|
type t = #{ i : int8#; }
|}]

type t : bits8 = #{ i : int8# }
[%%expect{|
Line 1, characters 0-31:
1 | type t : bits8 = #{ i : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8 addressable
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8
         because of the annotation on the declaration of the type t.
|}]

type r : bits8 addressable box = { i : int8# }
[%%expect{|
type r = { i : int8#; }
|}]

type r : bits8 box = { i : int8# }
[%%expect{|
Line 1, characters 0-34:
1 | type r : bits8 box = { i : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r" is bits8 addressable box
         because it's a boxed record type.
       But the layout of type "r" must be a sublayout of bits8 box
         because of the annotation on the declaration of the type r.
|}]

type r = { i : int8# }
type t : bits8 addressable = r#
[%%expect{|
type r = { i : int8#; }
type t = r#
|}]

type t : bits8 = r#
[%%expect{|
Line 1, characters 0-19:
1 | type t : bits8 = r#
    ^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r#" is bits8 addressable
         because it is an unboxed record.
       But the layout of type "r#" must be a sublayout of bits8
         because of the definition of t at line 1, characters 0-19.
|}]

(* An already-addressable field is unchanged *)
type t : bits64 = #{ i : int64_u }
type t : value = #{ s : string }
[%%expect{|
type t = #{ i : int64_u; }
type t = #{ s : string; }
|}]

(* The fields of a multi-field record are the components of a product, which
   makes them addressable *)
type t2 : bits8 & bits16 = #{ i : int8#; j : int16# }
type r2 : (bits8 & bits16) box = { i : int8#; j : int16# }
[%%expect{|
type t2 = #{ i : int8#; j : int16#; }
type r2 = { i : int8#; j : int16#; }
|}]

(* Projections have the field's type, at its own kind *)
type t8 = #{ i : int8# }
let proj (x : t8) : int8# = x.#i
[%%expect{|
type t8 = #{ i : int8#; }
val proj : t8 -> int8# = <fun>
|}]

type ('a : bits8) req8
type ('a : bits8 addressable) req8a
[%%expect{|
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
|}]

type ok = t8 req8a
[%%expect{|
type ok = t8 req8a
|}]

type bad = t8 req8
[%%expect{|
Line 1, characters 11-13:
1 | type bad = t8 req8
               ^^
Error: This type "t8" should be an instance of type "('a : bits8)"
       The layout of t8 is bits8 addressable
         because of the definition of t8 at line 1, characters 0-24.
       But the layout of t8 must be a sublayout of bits8
         because of the definition of req8 at line 1, characters 0-22.
|}]

type ok = r# req8a
[%%expect{|
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
         because of the definition of req8 at line 1, characters 0-22.
|}]

(* A record whose field has kind [any] *)
type ('a : any) t = #{ a : 'a }
[%%expect{|
type ('a : any) t = #{ a : 'a; }
|}]

type ok = int8# t req8a
[%%expect{|
type ok = int8# t req8a
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
         because of the definition of req8 at line 1, characters 0-22.
|}]

type ('a : bits64) req64
type ok = int64_u t req64
[%%expect{|
type ('a : bits64) req64
type ok = int64_u t req64
|}]

(* Checking against a made-addressable kind refines the field's kind to the
   plain kind *)
let f (x : 'b t) : 'b t req8a option = None
[%%expect{|
val f : ('b : bits8). 'b t -> 'b t req8a option = <fun>
|}]

(* A field whose kind is already made addressable *)
let need8a (type a : bits8 addressable) (_ : a) = ()
[%%expect{|
val need8a : ('a : bits8 addressable). 'a -> unit = <fun>
|}]

type ok = t8 t req8a
[%%expect{|
type ok = t8 t req8a
|}]

type u : bits8 addressable
type ok = u t req8a
[%%expect{|
type u : bits8 addressable
type ok = u t req8a
|}]

let g (type b : bits8 addressable) (x : b t) = need8a x
[%%expect{|
val g : ('b : bits8 addressable). 'b t -> unit = <fun>
|}]

let h (y : ('c : bits8 addressable)) = need8a #{ a = y }
[%%expect{|
val h : ('c : bits8 addressable). 'c -> unit = <fun>
|}]

let k (y : ('d : bits8)) = need8a #{ a = y }
[%%expect{|
val k : ('d : bits8). 'd -> unit = <fun>
|}]

(* The variable of a type annotation is constrained through the record before
   it is unified with the variable of the same name outside, so it commits to
   the plain kind first. *)
let f (x : ('b : bits8 addressable) t) : 'b t req8a option = None
[%%expect{|
Line 1, characters 41-43:
1 | let f (x : ('b : bits8 addressable) t) : 'b t req8a option = None
                                             ^^
Error: This type "('b : bits8)" should be an instance of type
         "('b0 : bits8 addressable)"
       The layout of 'b is bits8 addressable
         because of the annotation on the type variable 'b.
       But the layout of 'b must overlap with bits8
         because of the definition of req8a at line 2, characters 0-35.
|}]

(* Inclusion *)
module M : sig
  type t : bits8
end = struct
  type t = #{ i : int8# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ i : int8# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ i : int8#; } end
       is not included in
         sig type t : bits8 end
       Type declarations do not match:
         type t = #{ i : int8#; }
       is not included in
         type t : bits8
       The layout of the first is bits8 addressable
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits8
         because of the definition of t at line 2, characters 2-16.
|}]

module M : sig
  type t : bits8 addressable
end = struct
  type t = #{ i : int8# }
end
[%%expect{|
module M : sig type t : bits8 addressable end
|}]

module M : sig
  type t : bits8 addressable box
end = struct
  type t = { i : int8# }
end
[%%expect{|
module M : sig type t : bits8 addressable box end
|}]

(**** [@@unboxed] types make their contents addressable ****)

type t : float64 addressable = { f : float# } [@@unboxed]
[%%expect{|
type t = { f : float#; } [@@unboxed]
|}]

type t : float64 = { f : float# } [@@unboxed]
[%%expect{|
Line 1, characters 0-45:
1 | type t : float64 = { f : float# } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is float64 addressable
         because it is the unboxed version of the primitive type float.
       But the layout of type "t" must be a sublayout of float64
         because of the annotation on the declaration of the type t.
|}]

type t : bits32 addressable = A of int32_u [@@unboxed]
[%%expect{|
type t = A of int32_u [@@unboxed]
|}]

type t : bits32 = A of int32_u [@@unboxed]
[%%expect{|
Line 1, characters 0-42:
1 | type t : bits32 = A of int32_u [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits32 addressable
         because it is the primitive type int32_u.
       But the layout of type "t" must be a sublayout of bits32
         because of the annotation on the declaration of the type t.
|}]

type t : bits32 addressable = A of { i : int32_u } [@@unboxed]
[%%expect{|
type t = A of { i : int32_u; } [@@unboxed]
|}]

type t : bits32 = A of { i : int32_u } [@@unboxed]
[%%expect{|
Line 1, characters 0-50:
1 | type t : bits32 = A of { i : int32_u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits32 addressable
         because it is the primitive type int32_u.
       But the layout of type "t" must be a sublayout of bits32
         because of the annotation on the declaration of the type t.
|}]

module M : sig
  type t : bits32
end = struct
  type t = A of int32_u [@@unboxed]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of int32_u [@@unboxed]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of int32_u [@@unboxed] end
       is not included in
         sig type t : bits32 end
       Type declarations do not match:
         type t = A of int32_u [@@unboxed]
       is not included in
         type t : bits32
       The layout of the first is bits32 addressable
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits32
         because of the definition of t at line 2, characters 2-17.
|}]

module M : sig
  type t : bits32 addressable
end = struct
  type t = A of int32_u [@@unboxed]
end
[%%expect{|
module M : sig type t : bits32 addressable end
|}]

