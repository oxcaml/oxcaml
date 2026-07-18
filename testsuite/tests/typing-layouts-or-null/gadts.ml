(* TEST
 flags = "-w -181";
 expect;
*)

(* Basic GADT [@@or_null]: the null constructor and the (unboxed) payload
   constructor may carry return-type indices. *)
type _ t =
  | Null : int t
  | This : string -> string t
[@@or_null]

[%%expect{|
type _ t = Null : int t | This : string -> string t [@@or_null]
|}]

(* Construction and matching, with GADT refinement. *)
let n : int t = Null
let s : string t = This "hi"
let get_str (x : string t) = match x with This s -> s

[%%expect{|
val n : int t = Null
val s : string t = This "hi"
val get_str : string t -> string = <fun>
|}]

(* Refinement is sound: [Null : int t] cannot have type [string t]. *)
let bad (x : string t) = match x with Null -> assert false

[%%expect{|
Line 1, characters 38-42:
1 | let bad (x : string t) = match x with Null -> assert false
                                          ^^^^
Error: This pattern matches values of type "int t"
       but a pattern was expected which matches values of type "string t"
       Type "int" is not compatible with type "string"
|}]

(* Declaration-jkind precision: a GADT [@@or_null] payload is projected onto
   the declaration parameters, so a ground payload gets the same precise
   declaration jkind as its non-GADT twin. A ground [int] payload gives
   [immediate_or_null] (matching [type t = Null | This of int [@@or_null]]),
   not the conservative [value_or_null]. *)
module Ground_precise : sig type t : immediate_or_null end = struct
  type t = GN : t | GT : int -> t [@@or_null]
end

[%%expect{|
module Ground_precise : sig type t : immediate_or_null end
|}]

(* But a ground [float] payload is unboxed, so the type is nullable-float: its
   declaration jkind is [value_or_null], not [immediate_or_null]. *)
module Float_conservative : sig type t : immediate_or_null end = struct
  type t = FN : t | FT : float -> t [@@or_null]
end

[%%expect{|
Lines 1-3, characters 65-3:
1 | .................................................................struct
2 |   type t = FN : t | FT : float -> t [@@or_null]
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = FN : t | FT : float -> t [@@or_null] end
       is not included in
         sig type t : immediate_or_null end
       Type declarations do not match:
         type t = FN : t | FT : float -> t [@@or_null]
       is not included in
         type t : immediate_or_null
       The layout of the first is value_or_null
         because of the definition of t at line 2, characters 2-47.
       But the layout of the first must be a sublayout of
           value_or_null non_pointer
         because of the definition of t at line 1, characters 32-58.
|}]

module Float_ok : sig type t : value_or_null end = struct
  type t = FN : t | FT : float -> t [@@or_null]
end

[%%expect{|
module Float_ok : sig type t : value_or_null end
|}]

(* A polymorphic GADT covers indices whose payload could be [float], so its
   declaration jkind is not [immediate_or_null]. *)
module Poly_conservative : sig type _ f : immediate_or_null end = struct
  type _ f = PN : 'a f | PT : 'a -> 'a f [@@or_null]
end

[%%expect{|
Lines 1-3, characters 66-3:
1 | ..................................................................struct
2 |   type _ f = PN : 'a f | PT : 'a -> 'a f [@@or_null]
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type _ f = PN : 'a f | PT : 'a -> 'a f [@@or_null] end
       is not included in
         sig type _ f : immediate_or_null end
       Type declarations do not match:
         type _ f = PN : 'a f | PT : 'a -> 'a f [@@or_null]
       is not included in
         type _ f : immediate_or_null
       The layout of the first is value_or_null
         because of the definition of f at line 2, characters 2-52.
       But the layout of the first must be a sublayout of
           value_or_null non_pointer
         because of the definition of f at line 1, characters 31-59.
|}]

(* (a) The declaration's jkind must remain honest: it is [value_or_null],
   never [value]. *)
type _ a1 : value = Null : int a1 | This : string -> string a1 [@@or_null]

[%%expect{|
Line 1, characters 0-74:
1 | type _ a1 : value = Null : int a1 | This : string -> string a1 [@@or_null]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "a1" is value_or_null non_float
         because an [@@or_null] type gets its layout by applying or_null to its
         payload layout.
       But the layout of type "a1" must be a sublayout of value
         because of the annotation on the declaration of the type a1.
|}]

type _ a2 : value_or_null = Null : int a2 | This : string -> string a2
[@@or_null]

[%%expect{|
type _ a2 = Null : int a2 | This : string -> string a2 [@@or_null]
|}]

(* (b) A GADT whose payload could be [float] must not become a flat float
   array element. The payload is unboxed and a NULL is not a float, so the
   separability is read (per instantiation) from the unwrapped payload:
   [float f] is [maybe_separable] (a NULL could not sit in a flat float array),
   whereas [int f] is [non_float]. Thus [float f array] is a type error while
   [int f array] is allowed -- a NULL can never reach a flat float array. *)
type _ f = FNull : 'a f | FThis : 'a -> 'a f [@@or_null]

[%%expect{|
type _ f = FNull : 'a f | FThis : 'a -> 'a f [@@or_null]
|}]

let mk_float_arr (x : float f) = [| x |]

[%%expect{|
Line 1, characters 36-37:
1 | let mk_float_arr (x : float f) = [| x |]
                                        ^
Error: The value "x" has type "float f" but an expression was expected of type
         "('a : value_maybe_null)"
       The layout of float f is value_or_null
         because of the definition of f at line 1, characters 0-56.
       But the layout of float f must be a sublayout of value_maybe_null
         because it's the type of an array element.
|}]

let mk_int_arr (x : int f) = [| x |]

[%%expect{|
val mk_int_arr : int f -> int f array = <fun>
|}]

(* A widened [('a : any)] parameter is not narrowed by the GADT payload (the
   payload variable is constructor-local), so it reaches the declaration-jkind
   computation with a non-[value] layout that [apply_or_null] cannot process.
   This is handled by falling back to the conservative [value_or_null] jkind
   rather than crashing. *)
type ('a : any) widened_any_gadt =
  | Any_null : 'a widened_any_gadt
  | Any_this : 'a -> 'a widened_any_gadt
[@@or_null]

[%%expect{|
type ('a : any) widened_any_gadt =
    Any_null : 'a widened_any_gadt
  | Any_this : 'a -> 'a widened_any_gadt [@@or_null]
|}]

(* The payload of an [@@or_null] constructor must still be a [value]: a
   [float64] parameter is rejected. *)
type ('a : float64) widened_float64_gadt =
  | F_null : 'a widened_float64_gadt
  | F_this : 'a -> 'a widened_float64_gadt
[@@or_null]

[%%expect{|
Line 3, characters 13-15:
3 |   | F_this : 'a -> 'a widened_float64_gadt
                 ^^
Error: The layout of type "'a" is float64
         because of the annotation on 'a in the declaration of the type
                                      widened_float64_gadt.
       But the layout of type "'a" must be a value layout
         because the payload of widened_float64_gadt has layout value.
|}]

(* (c) Exhaustiveness stays sound: omitting a reachable constructor warns. *)
type _ u = UNull : 'a u | UThis : 'a -> 'a u [@@or_null]
let full (type a) (x : a u) = match x with UNull -> None | UThis v -> Some v

[%%expect{|
type _ u = UNull : 'a u | UThis : 'a -> 'a u [@@or_null]
val full : 'a u -> 'a option = <fun>
|}]

(* Omitting a reachable constructor is correctly flagged as non-exhaustive.
   (The named counter-example can be imprecise for [Variant_with_null] -- here
   [UThis] is the missing case but [UNull] is reported -- but the match is
   always soundly flagged; this is a pre-existing quirk of the null-tag
   counter-example generation, not a soundness gap.) *)
let missing_this (type a) (x : a u) = match x with UNull -> ()

[%%expect{|
Line 1, characters 38-62:
1 | let missing_this (type a) (x : a u) = match x with UNull -> ()
                                          ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "UNull"

val missing_this : 'a u -> unit = <fun>
|}]

(* (d) Abstraction: a [value_or_null] signature accepts the GADT impl; a
   [value] (non-null) signature is rejected. *)
module Ok : sig type _ t : value_or_null end = struct
  type _ t = Null : int t | This : string -> string t [@@or_null]
end

[%%expect{|
module Ok : sig type _ t : value_or_null end
|}]

module Bad : sig type _ t : value end = struct
  type _ t = Null : int t | This : string -> string t [@@or_null]
end

[%%expect{|
Lines 1-3, characters 40-3:
1 | ........................................struct
2 |   type _ t = Null : int t | This : string -> string t [@@or_null]
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type _ t = Null : int t | This : string -> string t [@@or_null]
         end
       is not included in
         sig type _ t end
       Type declarations do not match:
         type _ t = Null : int t | This : string -> string t [@@or_null]
       is not included in
         type _ t
       The layout of the first is value_or_null non_float
         because of the definition of t at line 2, characters 2-65.
       But the layout of the first must be a sublayout of value
         because of the definition of t at line 1, characters 17-33.
|}]

(* (e) Nested [@@or_null] stays blocked: the payload must be non-null. *)
type _ nested =
  | NNull : int nested
  | NThis : string or_null -> string nested
[@@or_null]

[%%expect{|
Line 3, characters 12-26:
3 |   | NThis : string or_null -> string nested
                ^^^^^^^^^^^^^^
Error: The layout of type "string or_null" is value_or_null
         because it is the primitive type or_null.
       But the layout of type "string or_null" must be a sublayout of
           value_maybe_separable
         because the payload of nested has layout value.
|}]

(* Existentials in the payload are allowed; when the payload's jkind is read
   (at use sites) the existentials are projected away, so they do not leak into
   the type's kind. *)
type _ e =
  | ENull : int e
  | EThis : ('b * ('b -> 'a)) -> 'a e
[@@or_null]

[%%expect{|
type _ e = ENull : int e | EThis : ('b * ('b -> 'a)) -> 'a e [@@or_null]
|}]

(* The [@@or_null] shape check is retained under GADT syntax: exactly one
   nullary and one unary constructor, both tuples (no inline records). *)
type _ inline =
  | INull : int inline
  | IThis : { x : string } -> string inline
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type _ inline =
2 |   | INull : int inline
3 |   | IThis : { x : string } -> string inline
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one nullary constructor and one unary constructor.
|}]

type _ two_args =
  | TNull : int two_args
  | TThis : string * int -> string two_args
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type _ two_args =
2 |   | TNull : int two_args
3 |   | TThis : string * int -> string two_args
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one nullary constructor and one unary constructor.
|}]
