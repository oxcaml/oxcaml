(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* [@layout_poly] accepts variables bounded at [any addressable]: each use
   site gets the shared fresh sort, additionally constrained to be
   addressable. *)

external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
[%%expect{|
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
|}]

type b8a : bits8 addressable

type b8 : bits8
[%%expect{|
type b8a : bits8 addressable
type b8 : bits8
|}]

(* Usable at addressable kinds *)
let f (x : string) = id_addressable x

let g (x : int64#) = id_addressable x

let h (x : b8a) = id_addressable x

let i (x : #(int64# * string)) = id_addressable x
[%%expect{|
val f : string -> string = <fun>
val g : int64# -> int64# = <fun>
val h : b8a -> b8a = <fun>
val i : #(int64# * string) -> #(int64# * string) = <fun>
|}]

(* Rejected at unaddressable kinds *)
let bad (x : b8) = id_addressable x
[%%expect{|
Line 1, characters 34-35:
1 | let bad (x : b8) = id_addressable x
                                      ^
Error: The value "x" has type "b8" but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of b8 is bits8
         because of the definition of b8 at line 3, characters 0-15.
       But the layout of b8 must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let bad (x : float#) = id_addressable x
[%%expect{|
Line 1, characters 38-39:
1 | let bad (x : float#) = id_addressable x
                                          ^
Error: The value "x" has type "float#" but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* ...including at an unboxed product with an unaddressable component *)
let bad (x : #(float# * string)) = id_addressable x
[%%expect{|
Line 1, characters 50-51:
1 | let bad (x : #(float# * string)) = id_addressable x
                                                      ^
Error: The value "x" has type "#(float# * string)"
       but an expression was expected of type
         "('a : '_representable_layout_3 addressable)"
       The layout of #(float# * string) is float64 & value non_float
         because it is an unboxed tuple.
       But the layout of #(float# * string) must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* We can constrain an unfilled variable to always be addressable *)
let ok64 x =
  let _ = id_addressable x in
  (x : int64#)
[%%expect{|
val ok64 : int64# -> int64# = <fun>
|}]

let bad x =
  let _ = id_addressable x in
  (x : float#)
[%%expect{|
Line 3, characters 3-4:
3 |   (x : float#)
       ^
Error: The value "x" has type "('a : '_representable_layout_4 addressable)"
       but an expression was expected of type "float#"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be addressable
         because it's the type of a variable bound by a `let`.
|}]

let nested (x : b8a) = id_addressable (id_addressable x)
[%%expect{|
val nested : b8a -> b8a = <fun>
|}]

(* Variables at [any] and [any addressable] share the same sort, but only
   the latter is constrained to be addressable *)
external magic_to_addressable :
  ('a : any) ('b : any addressable). 'a -> 'b = "%identity"
  [@@layout_poly]
[%%expect{|
external magic_to_addressable : ('a : any) ('b : any addressable). 'a -> 'b
  = "%identity" [@@layout_poly]
|}]

let ok (x : int64#) : int64# = magic_to_addressable x
[%%expect{|
val ok : int64# -> int64# = <fun>
|}]

(* The shared sort is [bits8]; [b8a]'s kind [bits8 addressable] satisfies the
   result's bound... *)
let ok8 (x : b8) : b8a = magic_to_addressable x
[%%expect{|
val ok8 : b8 -> b8a = <fun>
|}]

(* ...but [b8]'s kind [bits8] does not *)
let bad (x : b8) : b8 = magic_to_addressable x
[%%expect{|
Line 1, characters 24-46:
1 | let bad (x : b8) : b8 = magic_to_addressable x
                            ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "('a : bits8 addressable)"
       but an expression was expected of type "b8"
       The layout of b8 is bits8
         because of the definition of b8 at line 3, characters 0-15.
       But the layout of b8 must be a sublayout of bits8 addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* [@layout_poly] still requires a variable at layout [any] (possibly made
   addressable) *)
external bad_ext : ('a : value addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
[%%expect{|
Line 1, characters 31-42:
1 | external bad_ext : ('a : value addressable). 'a -> 'a = "%identity"
                                   ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

Line 1, characters 19-53:
1 | external bad_ext : ('a : value addressable). 'a -> 'a = "%identity"
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@layout_poly]" on this external declaration has no
       effect. Consider removing it or adding a type
       variable for it to operate on.
|}]

(* Regression test: Intersecting [value & value] with [any addressable] wraps
   the internal representation of the kind redundantly (a product of values is
   already addressable); [f] must print and behave like [f_plain]. *)
type ('a : any, 'b : any) r = #{ a : 'a; b : 'b }

let f_plain (x : ('a : value & value)) = x

let g_plain (y : (string, string) r) = f_plain y
[%%expect{|
type ('a : any, 'b : any) r = #{ a : 'a; b : 'b; }
val f_plain : ('a : value & value). 'a -> 'a = <fun>
val g_plain : (string, string) r -> (string, string) r = <fun>
|}]

let f (x : ('a : value & value)) = id_addressable x

let g (y : (string, string) r) = f y
[%%expect{|
val f : ('a : value & value). 'a -> 'a = <fun>
val g : (string, string) r -> (string, string) r = <fun>
|}]

(* [any & any] also meets [any addressable], even though [any] itself is not
   addressable. *)
let f_any (x : ('a : any & any)) = id_addressable x

let g_any (y : (string, string) r) = f_any y
[%%expect{|
val f_any : ('a : value_or_null & value_or_null). 'a -> 'a = <fun>
val g_any : (string, string) r -> (string, string) r = <fun>
|}]

(* Applying [id_addressable] to the record directly: the component sorts are
   not yet known addressable when the product is decomposed, and are
   constrained only when the fields are checked. *)
let d (y : (string, string) r) = id_addressable y
[%%expect{|
val d : (string, string) r -> (string, string) r = <fun>
|}]
