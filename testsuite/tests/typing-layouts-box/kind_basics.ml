(* TEST
 {
   flags = "-extension layouts_alpha -extension layout_poly_alpha";
   expect;
 }{
   flags = "-extension layouts_alpha -extension layout_poly_alpha -no-ikinds";
   expect;
 }
*)

(**** Basic box kinds and printing ****)

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

type t : any box non_null box
[%%expect{|
type t : any box non_null box
|}]

type t : bits8 box & bits16
[%%expect{|
type t : bits8 box & bits16
|}]

type t : bits8 addressable box
[%%expect{|
type t : bits8 addressable box
|}]

(**** Scannable axes of box kinds are implied by their contents ****)

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
   [value non_pointer] *)
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

(* [bits32 box] and [float32 box] are [value non_pointer64] *)
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
       The layout of t is bits32 box
         because of the definition of t at line 1, characters 0-19.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
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
       The layout of t is float32 box
         because of the definition of t at line 1, characters 0-20.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
|}]

(* [float64 box] is [value] *)
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
       The layout of t is float64 box
         because of the definition of t at line 1, characters 0-20.
       But the layout of t must be a sublayout of value non_float
         because of the definition of nf_req at line 3, characters 0-34.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* Other concrete layouts are [value non_float] when boxed *)

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

type t : (bits64 & float64) box
type ok = t nf_req
[%%expect{|
type t : (bits64 & float64) box
type ok = t nf_req
|}]

type t : bits8 addressable box
type ok = t nf_req
type bad = t np_req
[%%expect{|
type t : bits8 addressable box
type ok = t nf_req
Line 3, characters 11-12:
3 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is bits8 addressable box
         because of the definition of t at line 1, characters 0-30.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
|}]

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
       The layout of t is (bits8 & bits8) addressable box
         because of the definition of t at line 1, characters 0-40.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
|}]

(* Unlike [float64 box], [float64 addressable box] is a non-float block *)
type t : float64 addressable box
type ok = t nf_req
[%%expect{|
type t : float64 addressable box
type ok = t nf_req
|}]

type t : (float64 & float64) addressable box
type ok = t nf_req
[%%expect{|
type t : (float64 & float64) addressable box
type ok = t nf_req
|}]

type t : word box
type ok = t nf_req
[%%expect{|
type t : word box
type ok = t nf_req
|}]

type t : vec128 box
type ok = t nf_req
[%%expect{|
type t : vec128 box
type ok = t nf_req
|}]

(* Including box layouts: the outer box is a pointer even when the inner box
   is not *)
type t : bits8 box box
type ok = t nf_req
type bad = t np_req
[%%expect{|
type t : bits8 box box
type ok = t nf_req
Line 3, characters 11-12:
3 | type bad = t np_req
               ^
Error: This type "t" should be an instance of type "('a : value non_pointer)"
       The layout of t is bits8 box box
         because of the definition of t at line 1, characters 0-22.
       But the layout of t must be a sublayout of value non_pointer
         because of the definition of np_req at line 1, characters 0-36.
       Note: The layout of immediate is value non_pointer.
|}]

(* No axes are implied for payloads with unknown representations *)

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
       The layout of t is any box
         because of the definition of t at line 1, characters 0-16.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

type t : any addressable box
type bad = t v_req
[%%expect{|
type t : any addressable box
Line 2, characters 11-12:
2 | type bad = t v_req
               ^
Error: This type "t" should be an instance of type "('a : value)"
       The layout of t is any addressable box
         because of the definition of t at line 1, characters 0-28.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]

type t : (any & value) box
type bad = t v_req
[%%expect{|
type t : (any & value) box
Line 2, characters 11-12:
2 | type bad = t v_req
               ^
Error: This type "t" should be an instance of type "('a : value)"
       The layout of t is (any & value) box
         because of the definition of t at line 1, characters 0-26.
       But the layout of t must be a sublayout of value
         because of the definition of v_req at line 4, characters 0-23.
|}]


(* [box] is monotonic *)
type ('a : any box) any_box_req
type t : bits8 box
type ok = t any_box_req
[%%expect{|
type ('a : any box) any_box_req
type t : bits8 box
type ok = t any_box_req
|}]

(* Not all [value]s are box *)
type bad = string any_box_req
[%%expect{|
Line 1, characters 11-17:
1 | type bad = string any_box_req
               ^^^^^^
Error: This type "string" should be an instance of type "('a : any box)"
       The layout of string is value non_float
         because it is the primitive type string.
       But the layout of string must be a sublayout of any box
         because of the definition of any_box_req at line 1, characters 0-31.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* [_ box] is representable (as it's < scannable) *)
let f (x : 'a) (_ : 'a any_box_req) = x
[%%expect{|
val f : ('a : any box). 'a -> 'a any_box_req -> 'a = <fun>
|}]

(* [@layout_poly] leaves box-kinded variables alone (they are already
   representable), so [%identity] cannot mix them with instantiated ones *)
external magic : ('a : any) ('b : any box). 'a -> 'b = "%identity"
[@@layout_poly]
[%%expect{|
Line 1, characters 17-52:
1 | external magic : ('a : any) ('b : any box). 'a -> 'b = "%identity"
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%identity] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
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

(**** Mod bounds print after the box operator ****)

(* Written outside the operator *)
type t : value box mod portable
[%%expect{|
type t : value box mod portable
|}]

(* Written on the payload (kind_crossing.ml tests the resulting crossing) *)
type t : (value mod portable) box
[%%expect{|
type t : value box mod portable
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

(* Kind aliases involving box match in inclusion up to implied axes *)
module M : sig
  kind_ k = any box
end = struct
  kind_ k = any box
end
[%%expect{|
module M : sig kind_ k = any box end
|}]

module M : sig
  kind_ k = any box
end = struct
  kind_ k = any box non_null
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = any box non_null
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = any box non_null end
       is not included in
         sig kind_ k = any box end
       Kind declarations do not match:
         kind_ k = any box non_null
       is not included in
         kind_ k = any box
       Their definitions are not equal.
|}]

(* [kind_of_] cannot be boxed (it is unimplemented altogether) *)
type ('a : value) t : kind_of_ 'a box
[%%expect{|
Line 1, characters 22-37:
1 | type ('a : value) t : kind_of_ 'a box
                          ^^^^^^^^^^^^^^^
Error: Unimplemented kind syntax
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

(* Also through a functor's abstract kind *)
module Wrap (M : sig
    kind_ k

    type t : k
  end) =
struct
  kind_ k = M.k box

  type t : k
end
[%%expect{|
Line 7, characters 16-19:
7 |   kind_ k = M.k box
                    ^^^
Error: The kind constructor box cannot yet be applied to the abstract kind M.k.
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
  type t : value non_pointer
end = struct
  type t : bits8 box
end
[%%expect{|
module M : sig type t : value non_pointer end
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

(* Box kinds survive functor application *)
module F (X : sig
    type t : bits8 box
  end) =
struct
  type ok = X.t np_req
end

module A = F (struct
    type t : bits8 box
  end)
[%%expect{|
module F :
  functor (X : sig type t : bits8 box end) -> sig type ok = X.t np_req end
module A : sig type ok end
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
type bad = n any_box_req
[%%expect{|
type t : value box
type n = t or_null
Line 3, characters 11-12:
3 | type bad = n any_box_req
               ^
Error: This type "n" = "t or_null" should be an instance of type "('a : any box)"
       The layout of n is value_or_null
         because it is the primitive type or_null.
       But the layout of n must be a sublayout of any box
         because of the definition of any_box_req at line 1, characters 0-31.
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

(**** Unboxed records are never below box kinds, even when their declared
      kind is approximate ****)

type i8 : bits8
type ('a : any) prod = #{ a : 'a; b : string }
type bad = i8 prod any_box_req
[%%expect{|
type i8 : bits8
type ('a : any) prod = #{ a : 'a; b : string; }
Line 3, characters 11-18:
3 | type bad = i8 prod any_box_req
               ^^^^^^^
Error: This type "i8 prod" should be an instance of type "('a : any box)"
       The layout of i8 prod is any & value non_float
         because of the definition of prod at line 2, characters 0-46.
       But the layout of i8 prod must be a value layout
         because of the definition of any_box_req at line 1, characters 0-31.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(**** Test [Jkind.equate] by unifying univars ****)

type pa = { f : ('a : bits8 box). 'a -> 'a }
type pb = { f : ('b : bits8 box). 'b -> 'b }
let ok (x : pa) : pb = { f = x.f }
[%%expect{|
type pa = { f : ('a : bits8 box). 'a -> 'a; }
type pb = { f : ('b : bits8 box). 'b -> 'b; }
val ok : pa -> pb = <fun>
|}]

type pc = { f : ('c : bits16 box). 'c -> 'c }
let bad (x : pa) : pc = { f = x.f }
[%%expect{|
type pc = { f : ('c : bits16 box). 'c -> 'c; }
Line 2, characters 30-33:
2 | let bad (x : pa) : pc = { f = x.f }
                                  ^^^
Error: The field access "x.f" has type "'a -> 'a"
       but an expression was expected of type "'b -> 'b"
       The layout of 'a is bits16 box
         because of the definition of pc at line 1, characters 0-45.
       But the layout of 'a must overlap with bits8 box
         because of the definition of pa at line 1, characters 0-44.
|}]

type pd = { f : ('d : bits8). 'd -> 'd }
let bad (x : pa) : pd = { f = x.f }
[%%expect{|
type pd = { f : ('d : bits8). 'd -> 'd; }
Line 2, characters 30-33:
2 | let bad (x : pa) : pd = { f = x.f }
                                  ^^^
Error: The field access "x.f" has type "'a -> 'a"
       but an expression was expected of type "'b -> 'b"
       The layout of 'a is bits8
         because of the definition of pd at line 1, characters 0-40.
       But the layout of 'a must be a value layout
         because of the definition of pa at line 1, characters 0-44.
|}]

let bad (x : pd) : pa = { f = x.f }
[%%expect{|
Line 1, characters 30-33:
1 | let bad (x : pd) : pa = { f = x.f }
                                  ^^^
Error: The field access "x.f" has type "'a -> 'a"
       but an expression was expected of type "'b -> 'b"
       The layout of 'a is bits8 box
         because of the definition of pa at line 1, characters 0-44.
       But the layout of 'a must overlap with bits8
         because of the definition of pd at line 1, characters 0-40.
|}]

(* Boxes whose axes differ only by axes the contents imply are equal *)
type pe = { f : ('e : bits8 box non_null). 'e -> 'e }
let ok (x : pa) : pe = { f = x.f }
[%%expect{|
Line 1, characters 32-40:
1 | type pe = { f : ('e : bits8 box non_null). 'e -> 'e }
                                    ^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 box".

type pe = { f : ('e : bits8 box). 'e -> 'e; }
val ok : pa -> pe = <fun>
|}]

(* Boxes whose axes differ meaningfully are unequal, even when comparable *)
let bad (x : < m : ('f : any box). 'f -> 'f >)
    : < m : ('g : any box non_null). 'g -> 'g > = x
[%%expect{|
Line 2, characters 50-51:
2 |     : < m : ('g : any box non_null). 'g -> 'g > = x
                                                      ^
Error: The value "x" has type "< m : ('f : any box). 'f -> 'f >"
       but an expression was expected of type
         "< m : ('g : any box non_null). 'g -> 'g >"
       The method "m" has type "('f : any box). 'f -> 'f",
       but the expected method type was "('g : any box non_null). 'g -> 'g"
|}]

let bad (x : < m : ('g : any box non_null). 'g -> 'g >)
    : < m : ('f : any box). 'f -> 'f > = x
[%%expect{|
Line 2, characters 41-42:
2 |     : < m : ('f : any box). 'f -> 'f > = x
                                             ^
Error: The value "x" has type "< m : ('g : any box non_null). 'g -> 'g >"
       but an expression was expected of type
         "< m : ('f : any box). 'f -> 'f >"
       The method "m" has type "('g : any box non_null). 'g -> 'g",
       but the expected method type was "('f : any box). 'f -> 'f"
|}]

(* An explicitly-polymorphic variable can be inferred to need a box kind *)
type ('a : bits8 box) b8_box_req
module type S = sig
  val bad : 'a. 'a -> 'a b8_box_req
end
[%%expect{|
type ('a : bits8 box) b8_box_req
Line 3, characters 12-35:
3 |   val bad : 'a. 'a -> 'a b8_box_req
                ^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have kind value.
       But it was inferred to have kind bits8 box
         because of the definition of b8_box_req at line 1, characters 0-32.
|}]

(**** Test subkinding by generalizing field values ****)

(* A field value may be more general than its label, as the label's kind
   need only be a subkind of the value's *)
type pf = { f : ('f : any box). 'f -> 'f }
type pg = { f : ('g : any box non_null). 'g -> 'g }
let ok (x : pf) : pg = { f = x.f }
[%%expect{|
type pf = { f : ('f : any box). 'f -> 'f; }
type pg = { f : ('g : any box non_null). 'g -> 'g; }
val ok : pf -> pg = <fun>
|}]

let bad (x : pg) : pf = { f = x.f }
[%%expect{|
Line 1, characters 30-33:
1 | let bad (x : pg) : pf = { f = x.f }
                                  ^^^
Error: This field value has type "'a -> 'a" which is less general than
         "('f : any box). 'f -> 'f"
       The layout of 'a is any box
         because of the definition of pf at line 1, characters 0-42.
       But the layout of 'a must be a sublayout of any box non_null
         because of the definition of pg at line 2, characters 0-51.
|}]

(* Boxes with incomparable axes are accepted in neither direction *)
type ph = { f : ('h : any box non_float). 'h -> 'h }
let bad (x : pg) : ph = { f = x.f }
[%%expect{|
type ph = { f : ('h : any box non_float). 'h -> 'h; }
Line 2, characters 30-33:
2 | let bad (x : pg) : ph = { f = x.f }
                                  ^^^
Error: This field value has type "'a -> 'a" which is less general than
         "('h : any box non_float). 'h -> 'h"
       The layout of 'a is any box non_float
         because of the definition of ph at line 1, characters 0-52.
       But the layout of 'a must be a sublayout of any box non_float non_null
         because of the definition of pg at line 2, characters 0-51.
|}]

let bad (x : ph) : pg = { f = x.f }
[%%expect{|
Line 1, characters 30-33:
1 | let bad (x : ph) : pg = { f = x.f }
                                  ^^^
Error: This field value has type "'a -> 'a" which is less general than
         "('g : any box non_null). 'g -> 'g"
       The layout of 'a is any box non_null
         because of the definition of pg at line 2, characters 0-51.
       But the layout of 'a must be a sublayout of any box non_float non_null
         because of the definition of ph at line 1, characters 0-52.
|}]

(**** GADT refines a type to box ****)

type ('a : any) is_box = T : ('a : any box). 'a is_box

module M : sig
  type t
  val witness : t is_box
  val t : t
end = struct
  type t : float64 box
  let witness = T
  let t = Obj.magic 1.
end
[%%expect{|
type ('a : any) is_box = T : ('a : any box). 'a is_box
module M : sig type t val witness : t is_box val t : t end
|}]

let require_box (type a : any box) (_ : a) = ()

let ok () =
  let T = M.witness in
  require_box M.t
[%%expect{|
val require_box : ('a : any box). 'a -> unit = <fun>
val ok : unit -> unit = <fun>
|}]

let fail () = require_box M.t
[%%expect{|
Line 1, characters 26-29:
1 | let fail () = require_box M.t
                              ^^^
Error: The value "M.t" has type "M.t" but an expression was expected of type
         "('a : any box)"
       The layout of M.t is value
         because of the definition of t at line 4, characters 2-8.
       But the layout of M.t must be a sublayout of any box
         because of the definition of require_box at line 1, characters 16-47.
|}]

(**** Intersecting a box kind with a scannable sort ****)

(* The meet of [value non_pointer] and [any box] is
   [any box non_pointer non_null] *)
let ok (x : 'a) (_ : 'a np_req) (_ : 'a any_box_req) = x
[%%expect{|
val ok :
  ('a : any box non_pointer non_null).
    'a -> 'a np_req -> 'a any_box_req -> 'a =
  <fun>
|}]

(* And in the other order *)
let ok (x : 'a) (_ : 'a any_box_req) (_ : 'a np_req) = x
[%%expect{|
val ok :
  ('a : any box non_pointer non_null).
    'a -> 'a any_box_req -> 'a np_req -> 'a =
  <fun>
|}]

(* The meet of two box kinds meets the payloads and the applied axes *)
type ('a : any box non_null) box_nn_req
type ('a : any box non_float) box_nf_req
let ok (x : 'a) (_ : 'a box_nn_req) (_ : 'a box_nf_req) = x
[%%expect{|
type ('a : any box non_null) box_nn_req
type ('a : any box non_float) box_nf_req
val ok :
  ('a : any box non_float non_null).
    'a -> 'a box_nn_req -> 'a box_nf_req -> 'a =
  <fun>
|}]

(* Box kinds with incompatible payloads do not overlap *)
type ('a : bits16 box) b16_box_req
let bad (x : 'a) (_ : 'a b8_box_req) (_ : 'a b16_box_req) = x
[%%expect{|
type ('a : bits16 box) b16_box_req
Line 2, characters 42-44:
2 | let bad (x : 'a) (_ : 'a b8_box_req) (_ : 'a b16_box_req) = x
                                              ^^
Error: This type "('a : bits16 box)" should be an instance of type
         "('a0 : bits8 box)"
       The layout of 'a is bits8 box
         because of the definition of b8_box_req at line 1, characters 0-32.
       But the layout of 'a must overlap with bits16 box
         because of the definition of b16_box_req at line 1, characters 0-34.
|}]

(* Meeting with [any] applies its axes to the box *)
type ('a : any non_null) any_nn_req
let ok (x : 'a) (_ : 'a any_nn_req) (_ : 'a any_box_req) = x
[%%expect{|
type ('a : any non_null) any_nn_req
val ok : ('a : any box non_null). 'a -> 'a any_nn_req -> 'a any_box_req -> 'a =
  <fun>
|}]

(**** Inclusion between boxes with different payloads ****)

module M : sig
  type t : any box
end = struct
  type t : bits8 box
end
[%%expect{|
module M : sig type t : any box end
|}]

module M : sig
  type t : bits8 box
end = struct
  type t : bits16 box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits16 box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits16 box end
       is not included in
         sig type t : bits8 box end
       Type declarations do not match:
         type t : bits16 box
       is not included in
         type t : bits8 box
       The layout of the first is bits16 box
         because of the definition of t at line 4, characters 2-21.
       But the layout of the first must be a sublayout of bits8 box
         because of the definition of t at line 2, characters 2-20.
|}]

(**** Boxes inside products ****)

module M : sig
  type t : bits8 box & bits16
end = struct
  type t : bits8 box & bits16
end
[%%expect{|
module M : sig type t : bits8 box & bits16 end
|}]

module M : sig
  type t : bits8 box & bits16
end = struct
  type t : value & bits16
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value & bits16
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value & bits16 end
       is not included in
         sig type t : bits8 box & bits16 end
       Type declarations do not match:
         type t : value & bits16
       is not included in
         type t : bits8 box & bits16
       The layout of the first is value & bits16
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of bits8 box & bits16
         because of the definition of t at line 2, characters 2-29.
|}]

(* Box kinds and product kinds do not overlap *)
type ('a : bits8 & bits8) prod_req
let bad (x : 'a) (_ : 'a b8_box_req) (_ : 'a prod_req) = x
[%%expect{|
type ('a : bits8 & bits8) prod_req
Line 2, characters 42-44:
2 | let bad (x : 'a) (_ : 'a b8_box_req) (_ : 'a prod_req) = x
                                              ^^
Error: This type "('a : bits8 & bits8)" should be an instance of type
         "('a0 : bits8 box)"
       The layout of 'a is bits8 box
         because of the definition of b8_box_req at line 1, characters 0-32.
       But the layout of 'a must overlap with bits8 & bits8
         because of the definition of prod_req at line 1, characters 0-34.
|}]
