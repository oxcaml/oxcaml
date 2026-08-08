(* TEST
 flags = "-extension layouts_alpha -extension layout_poly_alpha";
 expect;
*)

(**** Acceptance and printing ****)

type t : bits8 box
[%%expect{|
type t : bits8 box
|}]

type t : void box
[%%expect{|
type t : void box
|}]

type t : any box
[%%expect{|
type t : any box
|}]

type t : value box
[%%expect{|
type t : value box
|}]

type t : (bits64 & float64) box
[%%expect{|
type t : (bits64 & float64) box
|}]

type t : bits8 box box
[%%expect{|
type t : bits8 box box
|}]

type t : (any box non_null) box
[%%expect{|
type t : any box non_null box
|}]

type t : bits8 box & bits16
[%%expect{|
type t : bits8 box & bits16
|}]

type t : bits64 addressable box
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits64 addressable box
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type t : bits64 box
|}]

type t : bits8 addressable box
[%%expect{|
type t : bits8 addressable box
|}]

(**** Subkinding: the boxed form of each layout determines the scannable
      axes of its box kind ****)

type ('a : value non_pointer) np_req
type ('a : value non_pointer64) np64_req
type ('a : value non_float) nf_req
type ('a : value) v_req
type ('a : value_or_null) von_req
[%%expect{|
type ('a : value non_pointer) np_req
type ('a : value non_pointer64) np64_req
type ('a : value non_float) nf_req
type 'a v_req
type ('a : value_or_null) von_req
|}]

(* [void box], [bits8 box], [bits16 box], and [untagged_immediate box] are
   boxed as tagged immediates *)
type t : void box
type ok = t np_req
[%%expect{|
type t : void box
type ok = t np_req
|}]

type t : bits8 box
type ok = t np_req
[%%expect{|
type t : bits8 box
type ok = t np_req
|}]

type t : bits16 box
type ok = t np_req
[%%expect{|
type t : bits16 box
type ok = t np_req
|}]

type t : untagged_immediate box
type ok = t np_req
[%%expect{|
type t : untagged_immediate box
type ok = t np_req
|}]

(* [bits32 box] and [float32 box] are boxed as tagged immediates only on
   64-bit architectures *)
type t : bits32 box
type ok = t np64_req
type bad = t np_req
[%%expect{|
type t : bits32 box
type ok = t np64_req
Line 3, characters 11-12:
3 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is value non_pointer64
         because of the definition of t at line 1, characters 0-19.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
       Note: The layout of immediate64 is value non_pointer64.
|}]

type t : float32 box
type ok = t np64_req
type bad = t np_req
[%%expect{|
type t : float32 box
type ok = t np64_req
Line 3, characters 11-12:
3 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is value non_pointer64
         because of the definition of t at line 1, characters 0-20.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
       Note: The layout of immediate64 is value non_pointer64.
|}]

(* Other concrete layouts are boxed as non-float blocks, except [float64],
   which is boxed as a float block *)
type t : float64 box
type ok = t v_req
type bad = t nf_req
[%%expect{|
type t : float64 box
type ok = t v_req
Line 3, characters 11-12:
3 | type bad = t nf_req
               ^
Error: This type "t" should be an instance of type "('a : value non_float)"
       The layout of t is value
         because of the definition of t at line 1, characters 0-20.
       But the layout of t must be a sublayout of value non_float
         because of the definition of nf_req at line 3, characters 0-34.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

type t : bits64 box
type ok = t nf_req
[%%expect{|
type t : bits64 box
type ok = t nf_req
|}]

type t : value box
type ok = t nf_req
[%%expect{|
type t : value box
type ok = t nf_req
|}]

(* A product boxes as a non-float block even when a component is [float64] *)
type t : (bits64 & float64) box
type ok = t nf_req
[%%expect{|
type t : (bits64 & float64) box
type ok = t nf_req
|}]

(* A concrete made-addressable payload boxes as a value, but with no
   further promises: not non-pointer (it is a block, so [bits8 addressable
   box] is weaker than [bits8 box]) and not non-float (once [float64] is
   addressable, its box is a float block) *)
type t : bits8 addressable box
type ok = t v_req
type bad = t nf_req
[%%expect{|
type t : bits8 addressable box
type ok = t v_req
Line 3, characters 11-12:
3 | type bad = t nf_req
               ^
Error: This type "t" should be an instance of type "('a : value non_float)"
       The layout of t is value
         because of the definition of t at line 1, characters 0-30.
       But the layout of t must be a sublayout of value non_float
         because of the definition of nf_req at line 3, characters 0-34.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

type t : bits8 addressable box
type bad = t np_req
[%%expect{|
type t : bits8 addressable box
Line 2, characters 11-12:
2 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is value
         because of the definition of t at line 1, characters 0-30.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
|}]

(* A concrete made-addressable product still boxes as a non-float block:
   a block of at least two fields is never a float, unlike the box of a
   made-addressable base *)
type t : (bits8 & bits8) addressable box
type ok = t nf_req
type bad = t np_req
[%%expect{|
type t : (bits8 & bits8) addressable box
type ok = t nf_req
Line 3, characters 11-12:
3 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is value non_float
         because of the definition of t at line 1, characters 0-40.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

type t : (float64 & float64) addressable box
type ok = t nf_req
[%%expect{|
type t : (float64 & float64) addressable box
type ok = t nf_req
|}]

(* No axes are implied for an unknown payload, even a surely-addressable
   one *)
type t : any addressable box
type ok = t von_req
type bad = t v_req
[%%expect{|
type t : any addressable box
type ok = t von_req
Line 3, characters 11-12:
3 | type bad = t v_req
               ^
Error: This type "t" should be an instance of type "('a : value)"
       The layout of t is value_or_null
         because of the definition of t at line 1, characters 0-28.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

(* [any box] is only a subkind of [scannable] *)
type t : any box
type ok = t von_req
type bad = t v_req
[%%expect{|
type t : any box
type ok = t von_req
Line 3, characters 11-12:
3 | type bad = t v_req
               ^
Error: This type "t" should be an instance of type "('a : value)"
       The layout of t is value_or_null
         because of the definition of t at line 1, characters 0-16.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

(* A product boxes as a block only when it is concrete: an [any] (or layout
   variable) component could be filled in with a layout whose box is not a
   value *)
type t : (any & value) box
type ok = t von_req
type bad = t v_req
[%%expect{|
type t : (any & value) box
type ok = t von_req
Line 3, characters 11-12:
3 | type bad = t v_req
               ^
Error: This type "t" should be an instance of type "('a : value)"
       The layout of t is value_or_null
         because of the definition of t at line 1, characters 0-26.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

(* [box] is monotonic... *)
type ('a : any box) any_box_req
type t : bits8 box
type ok = t any_box_req
[%%expect{|
type ('a : any box) any_box_req
type t : bits8 box
type ok = t any_box_req
|}]

(* ...and free: no box kind is below a box kind with an incomparable
   payload... *)
type ('a : bits16 box) b16_box_req
type t : bits8 box
type bad = t b16_box_req
[%%expect{|
type ('a : bits16 box) b16_box_req
type t : bits8 box
Line 3, characters 11-12:
3 | type bad = t b16_box_req
               ^
Error: This type "t" should be an instance of type "('a : bits16 box)"
       The layout of t is bits8 box
         because of the definition of t at line 2, characters 0-18.
       But the layout of t must be a sublayout of bits16 box
         because of the definition of b16_box_req at line 1, characters 0-34.
|}]

(* ...and nothing that isn't box-headed is below a box kind *)
type ('a : value box) value_box_req
type bad = string value_box_req
[%%expect{|
type ('a : value box) value_box_req
Line 2, characters 11-17:
2 | type bad = string value_box_req
               ^^^^^^
Error: This type "string" should be an instance of type "('a : value box)"
       The layout of string is value non_float
         because it is the primitive type string.
       But the layout of string must be a sublayout of value box
         because of the definition of value_box_req at line 1, characters 0-35.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* A type variable can take a box kind and still be used as a function
   argument (its sort is the value sort) *)
let f (x : 'a) (_ : 'a value_box_req) = x
[%%expect{|
val f : ('a : value box). 'a -> 'a value_box_req -> 'a = <fun>
|}]

(**** Scannable axes on box ****)

type t : any box non_null
[%%expect{|
type t : any box non_null
|}]

(* Redundant axes on box warn *)
type t : void box non_null
[%%expect{|
Line 1, characters 18-26:
1 | type t : void box non_null
                      ^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "void box".

type t : void box
|}]

(* Axes after [box] apply to the box, so they are not ignored; axes before
   [box] apply to the payload, and warn if the payload ignores them *)
type t : void non_null box
[%%expect{|
Line 1, characters 9-26:
1 | type t : void non_null box
             ^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_null" have no effect on the kind "void".

type t : void box
|}]

(**** Mode crossing: [k box] crosses like [mutable_data] joined with
      [k] (see kind_crossing.ml) ****)

(* In particular, [immediate box] does not cross externality *)
type t : immediate box
type ('a : value mod external_) ext_req
type bad = t ext_req
[%%expect{|
type t : immediate box
type ('a : value mod external_) ext_req
Line 3, characters 11-12:
3 | type bad = t ext_req
               ^
Error: This type "t" should be an instance of type "('a : value mod external_)"
       The kind of t is immediate box
         because of the definition of t at line 1, characters 0-22.
       But the kind of t must be a subkind of value mod external_
         because of the definition of ext_req at line 2, characters 0-39.
|}]

(**** Interplay with [addressable] ****)

(* Box kinds are addressable *)
type ('a : any addressable) addr_req
type t : bits8 box
type ok = t addr_req
[%%expect{|
type ('a : any addressable) addr_req
type t : bits8 box
type ok = t addr_req
|}]

type t : bits8 box addressable
[%%expect{|
Line 1, characters 19-30:
1 | type t : bits8 box addressable
                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 box".

type t : bits8 box
|}]

(**** Kind aliases expand under [box], but truly-abstract kinds and layout
      variables are rejected ****)

kind_ kb = bits8 box
type t : kb
type t2 : kb box
[%%expect{|
kind_ kb = bits8 box
type t : bits8 box
type t2 : bits8 box box
|}]

kind_ k
type t : k box
[%%expect{|
kind_ k
Line 2, characters 11-14:
2 | type t : k box
               ^^^
Error: The kind constructor box cannot yet be applied to the abstract kind k.
|}]

module type S = sig
  val f : layout_ x. ('a : x box). 'a -> unit
end
[%%expect{|
Line 2, characters 29-32:
2 |   val f : layout_ x. ('a : x box). 'a -> unit
                                 ^^^
Error: The kind constructor box cannot yet be applied to the abstract kind x.
|}]

(* [@layout_poly] does not operate at box kinds *)
external id : ('a : any box). 'a -> 'a = "%identity" [@@layout_poly]
[%%expect{|
Line 1, characters 14-38:
1 | external id : ('a : any box). 'a -> 'a = "%identity" [@@layout_poly]
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@layout_poly]" on this external declaration has no
       effect. Consider removing it or adding a type
       variable for it to operate on.
|}]

(**** Module inclusion ****)

module M : sig
  type t : bits8 box
end = struct
  type t : bits8 box
end
[%%expect{|
module M : sig type t : bits8 box end
|}]

module M : sig
  type t : bits8 box
end = struct
  type t : value
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : bits8 box end
       Type declarations do not match:
         type t
       is not included in
         type t : bits8 box
       The layout of the first is value
         because of the definition of t at line 4, characters 2-16.
       But the layout of the first must be a sublayout of bits8 box
         because of the definition of t at line 2, characters 2-20.
|}]

module M : sig
  type t : bits8 box
end = struct
  type t : bits8
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 end
       is not included in
         sig type t : bits8 box end
       Type declarations do not match:
         type t : bits8
       is not included in
         type t : bits8 box
       The layout of the first is bits8
         because of the definition of t at line 4, characters 2-16.
       But the layout of the first must be a value layout
         because of the definition of t at line 2, characters 2-20.
|}]

(* The other direction: a box kind can be hidden at its scannable axes *)
module M : sig
  type t : value non_pointer
end = struct
  type t : bits8 box
end
[%%expect{|
module M : sig type t : value non_pointer end
|}]

(**** [or_null] of a box kind is not a box kind ****)

type t : value box
type n = t or_null
type ok = n von_req
type bad = n v_req
[%%expect{|
type t : value box
type n = t or_null
type ok = n von_req
Line 4, characters 11-12:
4 | type bad = n v_req
               ^
Error: This type "n" = "t or_null" should be an instance of type "('a : value)"
       The layout of n is value_or_null
         because it is the primitive type or_null.
       But the layout of n must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

type t : value box
type n = t or_null
type bad = n value_box_req
[%%expect{|
type t : value box
type n = t or_null
Line 3, characters 11-12:
3 | type bad = n value_box_req
               ^
Error: This type "n" = "t or_null" should be an instance of type "('a : value box)"
       The layout of n is value_or_null
         because it is the primitive type or_null.
       But the layout of n must be a sublayout of value box
         because of the definition of value_box_req at line 1, characters 0-35.
|}]

(* The kind of [_ or_null] is never box-headed, even below box kinds whose
   axes permit [maybe_null] *)
type u : any box non_null
type bad = u or_null any_box_req
[%%expect{|
type u : any box non_null
Line 2, characters 11-20:
2 | type bad = u or_null any_box_req
               ^^^^^^^^^
Error: This type "u or_null" should be an instance of type "('a : any box)"
       The layout of u or_null is value_or_null
         because it is the primitive type or_null.
       But the layout of u or_null must be a sublayout of any box
         because of the definition of any_box_req at line 1, characters 0-31.
|}]

type u : value
type bad = u or_null any_box_req
[%%expect{|
type u
Line 2, characters 11-20:
2 | type bad = u or_null any_box_req
               ^^^^^^^^^
Error: This type "u or_null" should be an instance of type "('a : any box)"
       The layout of u or_null is value_or_null
         because it is the primitive type or_null.
       But the layout of u or_null must be a sublayout of any box
         because of the definition of any_box_req at line 1, characters 0-31.
|}]

(* No kind for ['a] can give ['a or_null] a box kind *)
type 'a w = 'a or_null any_box_req
[%%expect{|
Line 1, characters 12-22:
1 | type 'a w = 'a or_null any_box_req
                ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type "('b : any box)"
       The layout of 'a or_null is value_or_null
         because it is the primitive type or_null.
       But the layout of 'a or_null must be a sublayout of any box
         because of the definition of any_box_req at line 1, characters 0-31.
|}]
