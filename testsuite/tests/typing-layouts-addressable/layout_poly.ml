(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* [@layout_poly] accepts variables bounded at [any addressable]: each use
   site gets the shared fresh sort, additionally constrained to be
   addressable. *)

external id : ('a : any addressable). 'a -> 'a = "%identity" [@@layout_poly]
[%%expect{|
external id : ('a : any addressable). 'a -> 'a = "%identity" [@@layout_poly]
|}]

type t8 : bits8 addressable

type tb8 : bits8
[%%expect{|
type t8 : bits8 addressable
type tb8 : bits8
|}]

(* Usable at addressable kinds *)
let f (x : string) = id x

let g (x : int64#) = id x

let h (x : t8) = id x

let i (x : #(int64# * string)) = id x
[%%expect{|
val f : string -> string = <fun>
val g : int64# -> int64# = <fun>
val h : t8 -> t8 = <fun>
val i : #(int64# * string) -> #(int64# * string) = <fun>
|}]

(* Rejected at unaddressable kinds *)
let bad (x : tb8) = id x
[%%expect{|
Line 1, characters 23-24:
1 | let bad (x : tb8) = id x
                           ^
Error: The value "x" has type "tb8" but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 3, characters 0-16.
       But the layout of tb8 must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let bad (x : float#) = id x
[%%expect{|
Line 1, characters 26-27:
1 | let bad (x : float#) = id x
                              ^
Error: The value "x" has type "float#" but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* Variables at [any] and [any addressable] share the same sort, but only
   the latter is constrained to be addressable *)
external magic : ('a : any) ('b : any addressable). 'a -> 'b = "%identity"
  [@@layout_poly]
[%%expect{|
external magic : ('a : any) ('b : any addressable). 'a -> 'b = "%identity"
  [@@layout_poly]
|}]

let ok (x : int64#) : int64# = magic x
[%%expect{|
val ok : int64# -> int64# = <fun>
|}]

(* The shared sort is [bits8]; [t8]'s kind [bits8 addressable] satisfies the
   result's bound... *)
let ok8 (x : tb8) : t8 = magic x
[%%expect{|
val ok8 : tb8 -> t8 = <fun>
|}]

(* ...but [tb8]'s kind [bits8] does not *)
let bad (x : tb8) : tb8 = magic x
[%%expect{|
Line 1, characters 26-33:
1 | let bad (x : tb8) : tb8 = magic x
                              ^^^^^^^
Error: This expression has type "('a : bits8 addressable)"
       but an expression was expected of type "tb8"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 3, characters 0-16.
       But the layout of tb8 must be a sublayout of bits8 addressable
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
