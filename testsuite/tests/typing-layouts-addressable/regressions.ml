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

(**********************************************************************)
(* Constraining a kind-undetermined variable with [any addressable] COMMITS
   it to exactly the marked kind of whatever sort it resolves to (like sort
   defaulting; every question the checker asks is about an exact kind). At a
   product sort that means only the whole-marked kind satisfies it: the
   component-marked kind is a different, incomparable addressable kind and
   is (deliberately, incompletely) rejected. *)

type t_cm : bits8 addressable & bits16 addressable
type t_wm : (bits8 & bits16) addressable

let gm (x : t_cm) =
  let r = magic x in
  (r : ('c : any addressable))

[%%expect{|
type t_cm : bits8 addressable & bits16 addressable
type t_wm : (bits8 & bits16) addressable
val gm : ('c : (bits8 & bits16) addressable). t_cm -> 'c = <fun>
|}]

let use_wm (x : t_cm) : t_wm = gm x

[%%expect{|
val use_wm : t_cm -> t_wm = <fun>
|}]

let use_cm (x : t_cm) : t_cm = gm x

[%%expect{|
Line 1, characters 31-35:
1 | let use_cm (x : t_cm) : t_cm = gm x
                                   ^^^^
Error: This expression has type "('a : (bits8 & bits16) addressable)"
       but an expression was expected of type "t_cm"
       The layout of t_cm is bits8 addressable & bits16 addressable
         because of the definition of t_cm at line 1, characters 0-50.
       But the layout of t_cm must be a sublayout of
           (bits8 & bits16) addressable
         because of the definition of gm at lines 4-6, characters 7-30.
|}]

(**********************************************************************)
(* The same commitment applies to a layout-poly external bounded by
   [any addressable]: instantiating it at a product sort demands the
   whole-marked kind, so a component-marked product is (deliberately,
   incompletely) rejected even though it is addressable. Recovering this
   would take a fourth addressability slot value distinguishing the
   constraint from the mark; see [Jkind_types.Layout]. *)

external[@layout_poly] id_addressable :
  ('a : any addressable). 'a -> 'a = "%identity"

let poly_component_marked (x : t_cm) = id_addressable x

[%%expect{|
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
Line 4, characters 54-55:
4 | let poly_component_marked (x : t_cm) = id_addressable x
                                                          ^
Error: The value "x" has type "t_cm" but an expression was expected of type
         "('a : (bits8 & bits16) addressable)"
       The layout of t_cm is bits8 addressable & bits16 addressable
         because of the definition of t_cm at line 1, characters 0-50.
       But the layout of t_cm must be a sublayout of
           (bits8 & bits16) addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(**********************************************************************)
(* An [addressable] constraint on a variable must stay visible in
   signature mismatch errors, even though unconstrained sort-variable
   kinds are elided from printed types. *)

module M_elide : sig
  val h : unit -> unit
end = struct
  let h () (x : ('x : any addressable)) = ()
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let h () (x : ('x : any addressable)) = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val h :
             ('x : '_representable_layout_1 addressable). unit -> 'x -> unit
         end
       is not included in
         sig val h : unit -> unit end
       Values do not match:
         val h :
           ('x : '_representable_layout_1 addressable). unit -> 'x -> unit
       is not included in
         val h : unit -> unit
       The type "unit -> 'a -> unit" is not compatible with the type
         "unit -> unit"
       Hint: Did you forget to provide "()" as argument?
|}]
