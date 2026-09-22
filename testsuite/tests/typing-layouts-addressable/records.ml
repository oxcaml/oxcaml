(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(**** Records make their fields addressable ****)

(* A record with one field has the kind of that field made addressable *)
type t : bits8 addressable = #{ i : int8# }
[%%expect{|
Line 1, characters 0-43:
1 | type t : bits8 addressable = #{ i : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : bits8 = #{ i : int8# }
[%%expect{|
type t = #{ i : int8#; }
|}]

type r : bits8 addressable box = { i : int8# }
[%%expect{|
Line 1, characters 0-46:
1 | type r : bits8 addressable box = { i : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r" is bits8 box
         because it's a boxed record type.
       But the layout of type "r" must be a sublayout of bits8 addressable box
         because of the annotation on the declaration of the type r.
|}]

type r : bits8 box = { i : int8# }
[%%expect{|
type r = { i : int8#; }
|}]

type r = { i : int8# }
type t : bits8 addressable = r#
[%%expect{|
type r = { i : int8#; }
Line 2, characters 0-31:
2 | type t : bits8 addressable = r#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r#" is bits8
         because it is an unboxed record.
       But the layout of type "r#" must be a sublayout of bits8 addressable
         because of the definition of t at line 2, characters 0-31.
|}]

type t : bits8 = r#
[%%expect{|
type t = r#
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
Line 1, characters 10-12:
1 | type ok = t8 req8a
              ^^
Error: This type "t8" should be an instance of type "('a : bits8 addressable)"
       The layout of t8 is bits8
         because of the definition of t8 at line 1, characters 0-24.
       But the layout of t8 must be a sublayout of bits8 addressable
         because of the definition of req8a at line 2, characters 0-35.
|}]

type bad = t8 req8
[%%expect{|
type bad = t8 req8
|}]

type ok = r# req8a
[%%expect{|
Line 1, characters 10-12:
1 | type ok = r# req8a
              ^^
Error: This type "r#" should be an instance of type "('a : bits8 addressable)"
       The layout of r# is bits8
         because it is an unboxed record.
       But the layout of r# must be a sublayout of bits8 addressable
         because of the definition of req8a at line 2, characters 0-35.
|}]

type bad = r# req8
[%%expect{|
type bad = r# req8
|}]

(* A record whose field has kind [any] *)
type ('a : any) t = #{ a : 'a }
[%%expect{|
type ('a : any) t = #{ a : 'a; }
|}]

type ok = int8# t req8a
[%%expect{|
Line 1, characters 10-17:
1 | type ok = int8# t req8a
              ^^^^^^^
Error: This type "int8# t" should be an instance of type
         "('a : bits8 addressable)"
       The layout of int8# t is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# t must be a sublayout of bits8 addressable
         because of the definition of req8a at line 2, characters 0-35.
|}]

type bad = int8# t req8
[%%expect{|
type bad = int8# t req8
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
val f : ('b : bits8 addressable). 'b t -> 'b t req8a option = <fun>
|}]

(* A field whose kind is already made addressable *)
type ok = t8 t req8a
[%%expect{|
Line 1, characters 10-14:
1 | type ok = t8 t req8a
              ^^^^
Error: This type "t8 t" should be an instance of type "('a : bits8 addressable)"
       The layout of t8 t is bits8
         because of the definition of t8 at line 1, characters 0-24.
       But the layout of t8 t must be a sublayout of bits8 addressable
         because of the definition of req8a at line 2, characters 0-35.
|}]

type u : bits8 addressable
type ok = u t req8a
[%%expect{|
type u : bits8 addressable
type ok = u t req8a
|}]

let f (x : ('b : bits8 addressable) t) : 'b t req8a option = None
[%%expect{|
val f : ('b : bits8 addressable). 'b t -> 'b t req8a option = <fun>
|}]

(* Inclusion *)
module M : sig
  type t : bits8
end = struct
  type t = #{ i : int8# }
end
[%%expect{|
module M : sig type t : bits8 end
|}]

module M : sig
  type t : bits8 addressable
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
         sig type t : bits8 addressable end
       Type declarations do not match:
         type t = #{ i : int8#; }
       is not included in
         type t : bits8 addressable
       The layout of the first is bits8
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 2, characters 2-28.
|}]

module M : sig
  type t : bits8 addressable box
end = struct
  type t = { i : int8# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { i : int8# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { i : int8#; } end
       is not included in
         sig type t : bits8 addressable box end
       Type declarations do not match:
         type t = { i : int8#; }
       is not included in
         type t : bits8 addressable box
       The layout of the first is bits8 box
         because of the definition of t at line 4, characters 2-24.
       But the layout of the first must be a sublayout of
           bits8 addressable box
         because of the definition of t at line 2, characters 2-32.
|}]

(**** [@@unboxed] types make their contents addressable ****)

type t : float64 addressable = { f : float# } [@@unboxed]
[%%expect{|
Line 1, characters 0-57:
1 | type t : float64 addressable = { f : float# } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is float64
         because it is the unboxed version of the primitive type float.
       But the layout of type "t" must be a sublayout of float64 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : float64 = { f : float# } [@@unboxed]
[%%expect{|
type t = { f : float#; } [@@unboxed]
|}]

type t : bits32 addressable = A of int32_u [@@unboxed]
[%%expect{|
Line 1, characters 0-54:
1 | type t : bits32 addressable = A of int32_u [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits32
         because it is the primitive type int32_u.
       But the layout of type "t" must be a sublayout of bits32 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : bits32 = A of int32_u [@@unboxed]
[%%expect{|
type t = A of int32_u [@@unboxed]
|}]

type t : bits32 addressable = A of { i : int32_u } [@@unboxed]
[%%expect{|
Line 1, characters 0-62:
1 | type t : bits32 addressable = A of { i : int32_u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits32
         because it is the primitive type int32_u.
       But the layout of type "t" must be a sublayout of bits32 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : bits32 = A of { i : int32_u } [@@unboxed]
[%%expect{|
type t = A of { i : int32_u; } [@@unboxed]
|}]

module M : sig
  type t : bits32
end = struct
  type t = A of int32_u [@@unboxed]
end
[%%expect{|
module M : sig type t : bits32 end
|}]

module M : sig
  type t : bits32 addressable
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
         sig type t : bits32 addressable end
       Type declarations do not match:
         type t = A of int32_u [@@unboxed]
       is not included in
         type t : bits32 addressable
       The layout of the first is bits32
         because it is the expansion of a type abbreviation.
       But the layout of the first must be a sublayout of bits32 addressable
         because of the definition of t at line 2, characters 2-29.
|}]

