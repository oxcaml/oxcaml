(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 expect;
*)

(* Regression tests for the [addressable] kind operator. *)

(**********************************************************************)
(* Resolving a sort variable must not pin addressability: sorts do not
   carry addressability, so knowing that a kind's sort is [bits8] leaves
   both [bits8] and [bits8 addressable] possible.

   Here [magic]'s ['a] and ['b] share one sort variable (layout-poly
   externals use a single sort for all their variables), which typing
   the argument resolves to [bits8] before the result's addressability
   is constrained. The result's kind should be [bits8 addressable]. *)

external[@layout_poly] magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"

type t8_plain : bits8

let f (x : t8_plain) =
  let r = magic x in
  (r : ('b : any addressable))

[%%expect{|
external magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"
  [@@layout_poly]
type t8_plain : bits8
Line 7, characters 3-4:
7 |   (r : ('b : any addressable))
       ^
Error: The value "r" has type "('a : bits8)"
       but an expression was expected of type "('b : any addressable)"
       The layout of 'b is any addressable
         because of the annotation on the type variable 'b.
       But the layout of 'b must overlap with bits8
         because of the definition of r at line 6, characters 10-17.
|}]

(* The same bug through expected-type propagation: the argument is still
   typed (resolving the shared sort variable) before the result is
   unified with the annotated type variable. *)

let g (x : t8_plain) =
  let r = (magic x : ('b : any addressable)) in
  r

[%%expect{|
Line 2, characters 11-18:
2 |   let r = (magic x : ('b : any addressable)) in
               ^^^^^^^
Error: This expression has type "('a : bits8)"
       but an expression was expected of type
         "('b : '_representable_layout_1 addressable)"
       The layout of 'b is '_representable_layout_1 addressable
         because of the annotation on the type variable 'b.
       But the layout of 'b must overlap with bits8
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(**********************************************************************)
(* The product-of-[any]s layout given to an unboxed record with an [any]
   layout annotation during initialization must keep the annotation's
   [addressable] mark. [bits8 & bits16] is not addressable, so these
   declarations must be rejected rather than silently dropping the
   requirement. *)

type bad : any addressable = #{ x : int8#; y : int16# }

[%%expect{|
type bad = #{ x : int8#; y : int16#; }
|}]

type ('a : any addressable) reqa
type inconsistent = bad reqa

[%%expect{|
type ('a : any addressable) reqa
Line 2, characters 20-23:
2 | type inconsistent = bad reqa
                        ^^^
Error: This type "bad" should be an instance of type "('a : any addressable)"
       The layout of bad is bits8 & bits16
         because of the definition of bad at line 1, characters 0-55.
       But the layout of bad must be a sublayout of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

type bad_single : any addressable = #{ x : int8# }

[%%expect{|
type bad_single = #{ x : int8#; }
|}]

(* A product of addressable kinds is addressable, so this is fine. *)

type ok : any addressable = #{ x : int64#; y : string }
type ok' = ok reqa

[%%expect{|
type ok = #{ x : int64#; y : string; }
type ok' = ok reqa
|}]
