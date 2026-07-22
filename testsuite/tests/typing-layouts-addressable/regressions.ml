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
val f : ('b : bits8 addressable). t8_plain -> 'b = <fun>
|}]

(* The same bug through expected-type propagation: the argument is still
   typed (resolving the shared sort variable) before the result is
   unified with the annotated type variable. *)

let g (x : t8_plain) =
  let r = (magic x : ('b : any addressable)) in
  r

[%%expect{|
val g : ('b : bits8 addressable). t8_plain -> 'b = <fun>
|}]

(**********************************************************************)
(* The product-of-[any]s layout given to an unboxed record with an [any]
   layout annotation during initialization must keep the annotation's
   [addressable] mark. [bits8 & bits16] is not addressable, so these
   declarations must be rejected rather than silently dropping the
   requirement. *)

type bad : any addressable = #{ x : int8#; y : int16# }

[%%expect{|
Line 1, characters 0-55:
1 | type bad : any addressable = #{ x : int8#; y : int16# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is bits8 & bits16
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of
           (any & any) addressable
         because of the annotation on the declaration of the type bad.
|}]

type ('a : any addressable) reqa
type inconsistent = bad reqa

[%%expect{|
type ('a : any addressable) reqa
Line 2, characters 20-23:
2 | type inconsistent = bad reqa
                        ^^^
Error: Unbound type constructor "bad"
|}]

type bad_single : any addressable = #{ x : int8# }

[%%expect{|
Line 1, characters 0-50:
1 | type bad_single : any addressable = #{ x : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad_single" is bits8
         because it is an unboxed record.
       But the layout of type "bad_single" must be a sublayout of
           any addressable
         because of the annotation on the declaration of the type bad_single.
|}]

(* A product of addressable kinds is addressable, so this is fine. *)

type ok : any addressable = #{ x : int64#; y : string }
type ok' = ok reqa

[%%expect{|
type ok = #{ x : int64#; y : string; }
type ok' = ok reqa
|}]
