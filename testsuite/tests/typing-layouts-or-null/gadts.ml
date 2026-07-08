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
         because an [@@or_null] type gets the layout of or_null
         applied to its payload type.
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

(* The [@@or_null] shape check is retained under GADT syntax: each
   constructor must be nullary or unary, and a tuple (no inline records). *)
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
       each constructor must be nullary or unary.
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
       each constructor must be nullary or unary.
|}]

(* (f) GADT with a unary all-void null constructor: newly expressible now
   that the GADT gate is gone and the shape check is per-constructor. The
   all-void argument classifies the constructor as the null case, exactly as
   for non-GADT custom or_nulls. *)
type void : void mod everything
external void : unit -> void = "%unbox_unit"

type _ vn =
  | VNull : void -> int vn
  | VThis : 'a -> 'a vn
[@@or_null]

[%%expect{|
type void : void mod everything
external void : unit -> void = "%unbox_unit"
type _ vn = VNull : void -> int vn | VThis : 'a -> 'a vn [@@or_null]
|}]

(* Constructor order does not matter. *)
type _ vn_flipped =
  | WThis : 'a -> 'a vn_flipped
  | WNull : void -> int vn_flipped
[@@or_null]

[%%expect{|
type _ vn_flipped =
    WThis : 'a -> 'a vn_flipped
  | WNull : void -> int vn_flipped [@@or_null]
|}]

(* The null constructor's void payload enters the declaration's kind as a
   with-bound (projected with the null constructor's own result type);
   [void mod everything] crosses everything, so a ground [int] payload still
   yields [immediate_or_null]. *)
module Void_null_precise : sig type t : immediate_or_null end = struct
  type t = VN : void -> t | VT : int -> t [@@or_null]
end

[%%expect{|
module Void_null_precise : sig type t : immediate_or_null end
|}]

(* The projected null with-bound is observable: a null payload of plain
   kind [void] does not cross portability, so it blocks a [mod portable]
   signature that the [void mod everything] payload above satisfies. *)
type nonportable_void : void
module Void_null_bound_blocks : sig
  type t : value_or_null mod portable
end = struct
  type t = BN : nonportable_void -> t | BT : int -> t [@@or_null]
end

[%%expect{|
type nonportable_void : void
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t = BN : nonportable_void -> t | BT : int -> t [@@or_null]
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = BN : nonportable_void -> t | BT : int -> t [@@or_null]
         end
       is not included in
         sig type t : value_or_null mod portable end
       Type declarations do not match:
         type t = BN : nonportable_void -> t | BT : int -> t [@@or_null]
       is not included in
         type t : value_or_null mod portable
       The kind of the first is immediate_or_null with nonportable_void
         because of the definition of t at line 5, characters 2-65.
       But the kind of the first must be a subkind of
           value_or_null mod portable
         because of the definition of t at line 3, characters 2-37.
|}]

(* An existential void payload on the null constructor is projected too: the
   orphaned constructor-local variable becomes a [(type : ...)] bound rather
   than escaping its scope. *)
type _ evn =
  | EVNull : ('v : void). 'v -> int evn
  | EVThis : 'a -> 'a evn
[@@or_null]

[%%expect{|
type _ evn = EVNull : ('v : void). 'v -> int evn | EVThis : 'a -> 'a evn [@@or_null]
|}]

(* The projected existential bound is observable, exactly like the concrete
   [nonportable_void] bound above: plain [(type : void)] does not cross
   portability. *)
module Evn_bound_blocks : sig
  type _ t : value_or_null mod portable
end = struct
  type _ t =
    | EN : ('v : void). 'v -> int t
    | ET : int -> 'a t
  [@@or_null]
end

[%%expect{|
Lines 3-8, characters 6-3:
3 | ......struct
4 |   type _ t =
5 |     | EN : ('v : void). 'v -> int t
6 |     | ET : int -> 'a t
7 |   [@@or_null]
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type _ t = EN : ('v : void). 'v -> int t | ET : int -> 'a t [@@or_null]
         end
       is not included in
         sig type _ t : value_or_null mod portable end
       Type declarations do not match:
         type _ t = EN : ('v : void). 'v -> int t | ET : int -> 'a t [@@or_null]
       is not included in
         type _ t : value_or_null mod portable
       The kind of the first is value_or_null non_pointer mod external_
         because of the definition of t at lines 4-7, characters 2-13.
       But the kind of the first must be a subkind of
           value_or_null mod portable
         because of the definition of t at line 2, characters 2-39.
|}]

(* (g) Recursive groups: during the group's translation the GADT or_null has
   a conservative temporary jkind; the final jkinds are then recomputed in
   dependency order, so a sibling consuming the finished type still sees the
   precise kind. *)
type 'a rt = RN : 'b rt | RT : 'c -> 'c rt [@@or_null]
and rbox = RB of int rt

[%%expect{|
type 'a rt = RN : 'b rt | RT : 'c -> 'c rt [@@or_null]
and rbox = RB of int rt
|}]

module Group_precise : sig
  type t : immediate_or_null
  type box = B of t
end = struct
  type t = GN : t | GT : int -> t [@@or_null]
  and box = B of t
end

[%%expect{|
module Group_precise : sig type t : immediate_or_null type box = B of t end
|}]

(* A GADT or_null and a void-null or_null in one recursive group, both with
   payloads from the group. *)
type ga = GAN : ga | GAT : grec -> ga [@@or_null]
and grec = { g : int }
and gvn = GVN : void -> gvn | GVT : grec -> gvn [@@or_null]

[%%expect{|
type ga = GAN : ga | GAT : grec -> ga [@@or_null]
and grec = { g : int; }
and gvn = GVN : void -> gvn | GVT : grec -> gvn [@@or_null]
|}]

(* (h) Mode crossing flows through the projected payload with-bound: an
   [int] payload crosses portability and contention... *)
module Crossing_ok : sig
  type t : value_or_null mod portable contended
end = struct
  type t = PN : t | PT : int -> t [@@or_null]
end

[%%expect{|
module Crossing_ok : sig type t : value_or_null mod portable contended end
|}]

(* ... while a function payload does not cross portability. *)
module Crossing_bad : sig
  type t : value_or_null mod portable
end = struct
  type t = QN : t | QT : (int -> int) -> t [@@or_null]
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = QN : t | QT : (int -> int) -> t [@@or_null]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = QN : t | QT : (int -> int) -> t [@@or_null] end
       is not included in
         sig type t : value_or_null mod portable end
       Type declarations do not match:
         type t = QN : t | QT : (int -> int) -> t [@@or_null]
       is not included in
         type t : value_or_null mod portable
       The kind of the first is value_or_null non_float mod aliased immutable
         because of the definition of t at line 4, characters 2-54.
       But the kind of the first must be a subkind of
           value_or_null mod portable
         because of the definition of t at line 2, characters 2-37.
|}]

(* An existential payload constrained to a crossing kind keeps the crossing
   through the [(type : ...)] projection. *)
module Crossing_existential : sig
  type t : value_or_null mod portable
end = struct
  type t =
    | XN : t
    | XT : ('b : value mod portable). 'b -> t
  [@@or_null]
end

[%%expect{|
module Crossing_existential : sig type t : value_or_null mod portable end
|}]

(* (i) Module inclusion compares GADT or_null constructors including their
   result types. *)
module Incl_ok : sig
  type _ t = Null : int t | This : string -> string t [@@or_null]
end = struct
  type _ t = Null : int t | This : string -> string t [@@or_null]
end

[%%expect{|
module Incl_ok :
  sig type _ t = Null : int t | This : string -> string t [@@or_null] end
|}]

module Incl_bad : sig
  type _ t = Null : int t | This : string -> string t [@@or_null]
end = struct
  type _ t = Null : bool t | This : string -> string t [@@or_null]
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type _ t = Null : bool t | This : string -> string t [@@or_null]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type _ t = Null : bool t | This : string -> string t [@@or_null]
         end
       is not included in
         sig
           type _ t = Null : int t | This : string -> string t [@@or_null]
         end
       Type declarations do not match:
         type _ t = Null : bool t | This : string -> string t [@@or_null]
       is not included in
         type _ t = Null : int t | This : string -> string t [@@or_null]
       Constructors do not match:
         "Null : bool t"
       is not the same as:
         "Null : int t"
       The type "bool t" is not equal to the type "int t"
       Type "bool" is not equal to type "int"
|}]

(* (j) The unboxed-recursion check sees through GADT payload variables. A
   widened declaration parameter lets the or_null type be indexed by a
   maybe-null type (only the payload constructor's local variable is
   constrained to [value]), so an unboxed cycle can re-enter through the
   payload variable. The B1-B4 projection in [contained_without_boxing]
   makes that edge visible, and the cycle is rejected exactly like its
   ordinary unboxed-GADT counterpart. Without the projection this
   declaration was accepted (the payload edge was invisible). *)
type w = W of w t [@@unboxed]
and ('a : value_or_null) t =
  | N : 'b t
  | T : ('c : value). 'c -> 'c t
[@@or_null]

[%%expect{|
Line 1, characters 0-29:
1 | type w = W of w t [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "w" is recursive without boxing:
         "w" contains "w t",
         "w t" contains "w"
|}]

(* The same or_null declaration without the cycle stays accepted. *)
type ('a : value_or_null) widened_ok =
  | WON : 'b widened_ok
  | WOT : ('c : value). 'c -> 'c widened_ok
[@@or_null]

[%%expect{|
type ('a : value_or_null) widened_ok =
    WON : 'b widened_ok
  | WOT : 'c -> 'c widened_ok [@@or_null]
|}]

(* A repeated result variable keeps the first-occurrence projection (BW1 of
   Note [With-bounds for GADTs]): at [(w, int) t] the payload ['c] projects
   to [w] even though the second index would force [w = int], so the cycle
   check rejects this declaration although [T] is uninhabitable at that
   index and [W N] would be a value of [w]. This is the checker's
   established index-insensitive conservatism, not something specific to
   [@@or_null]: the ordinary unboxed GADT below is rejected identically by
   the pre-existing projection in [unbox_once]. *)
type w2 = W2 of (w2, int) t2 [@@unboxed]
and ('a : value_or_null, 'b : value_or_null) t2 =
  | N2 : ('d : value_or_null). ('d, int) t2
  | T2 : ('c : value). 'c -> ('c, 'c) t2
[@@or_null]

[%%expect{|
Line 1, characters 0-40:
1 | type w2 = W2 of (w2, int) t2 [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "w2" is recursive without boxing:
         "w2" contains "(w2, int) t2",
         "(w2, int) t2" contains "w2"
|}]

type v = V of (v, int) u [@@unboxed]
and ('a, 'b) u = MkU : 'c -> ('c, 'c) u [@@unboxed]

[%%expect{|
Line 1, characters 0-36:
1 | type v = V of (v, int) u [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "v" is recursive without boxing:
         "v" contains "(v, int) u",
         "(v, int) u" contains "v"
|}]
