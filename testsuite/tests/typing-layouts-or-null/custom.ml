(* TEST
 flags = "-w -181";
 expect;
*)

type ('a : value) t : value_or_null =
  | Nope
  | Yep of 'a
[@@or_null]

[%%expect{|
type 'a t = Nope | Yep of 'a [@@or_null]
|}]

type ('a : value) both_attrs : value_or_null =
  Nope | Yep of 'a [@@or_null] [@@or_null_reexport]

[%%expect{|
Lines 1-2, characters 0-51:
1 | type ('a : value) both_attrs : value_or_null =
2 |   Nope | Yep of 'a [@@or_null] [@@or_null_reexport]
Error: Invalid [@or_null] declaration:
       it cannot be both [@@or_null] and [@@or_null_reexport].
|}]

let to_option = function
  | Nope -> None
  | Yep x -> Some x

[%%expect{|
val to_option : 'a t -> 'a option = <fun>
|}]

(* Missing-case witnesses must work from either constructor. *)
let missing_payload : int t -> unit = function Nope -> ()

[%%expect{|
Line 1, characters 38-57:
1 | let missing_payload : int t -> unit = function Nope -> ()
                                          ^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Yep _"

val missing_payload : int t -> unit = <fun>
|}]

let missing_null : int t -> unit = function Yep _ -> ()

[%%expect{|
Line 1, characters 35-55:
1 | let missing_null : int t -> unit = function Yep _ -> ()
                                       ^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Nope"

val missing_null : int t -> unit = <fun>
|}]

let of_option = function
  | None -> Nope
  | Some x -> Yep x

[%%expect{|
val of_option : 'a option -> 'a t = <fun>
|}]

type ('a : value) flipped : value_or_null =
  | Yep_first of 'a
  | Nope_last
[@@or_null]

[%%expect{|
type 'a flipped = Yep_first of 'a | Nope_last [@@or_null]
|}]

let is_nope = function
  | Nope_last -> true
  | Yep_first _ -> false

[%%expect{|
val is_nope : 'a flipped -> bool = <fun>
|}]

let none = Nope_last
let some = Yep_first 5

[%%expect{|
val none : 'a flipped = Nope_last
val some : int flipped = Yep_first 5
|}]

let bad = Yep (Yep 5)

[%%expect{|
Line 1, characters 14-21:
1 | let bad = Yep (Yep 5)
                  ^^^^^^^
Error: This constructor has type "'a t" but an expression was expected of type
         "('b : value)"
       The layout of 'a t is value_or_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the layout of 'a t must be a sublayout of value
         because of the definition of t at lines 1-4, characters 0-11.
|}]

type t_non_float : value mod non_float
type ('a : any mod separable) accepts_sep
type ('a : value_or_null mod non_float) accepts_nonfloat

type succeeds = t_non_float t accepts_sep
type succeeds = t_non_float t accepts_nonfloat

[%%expect{|
type t_non_float : value non_float
type ('a : any separable) accepts_sep
type ('a : value_or_null non_float) accepts_nonfloat
type succeeds = t_non_float t accepts_sep
type succeeds = t_non_float t accepts_nonfloat
|}]

type fails = float t accepts_sep

[%%expect{|
Line 1, characters 13-20:
1 | type fails = float t accepts_sep
                 ^^^^^^^
Error: This type "float t" should be an instance of type "('a : any separable)"
       The layout of float t is value_or_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the layout of float t must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = float t accepts_nonfloat

[%%expect{|
Line 1, characters 13-20:
1 | type fails = float t accepts_nonfloat
                 ^^^^^^^
Error: This type "float t" should be an instance of type
         "('a : value_or_null non_float)"
       The layout of float t is value_or_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the layout of float t must be a sublayout of
           value_or_null non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-56.
|}]

type int_t = int t

module type S = sig
  type t : any mod separable
end

[%%expect{|
type int_t = int t
module type S = sig type t : any separable end
|}]

module type S' = S with type t = int_t

[%%expect{|
module type S' = sig type t = int_t end
|}]

type 'a too_many =
  | A
  | B of 'a
  | C
[@@or_null]

[%%expect{|
Lines 1-5, characters 0-11:
1 | type 'a too_many =
2 |   | A
3 |   | B of 'a
4 |   | C
5 | [@@or_null]
Error: Invalid [@or_null] declaration: it must have exactly two constructors.
|}]

type multi_arg_payload =
  | A
  | B of int * int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type multi_arg_payload =
2 |   | A
3 |   | B of int * int
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       each constructor must be nullary or unary.
|}]

(* Non-parameterized custom [@@or_null] types. *)

type no_param =
  | A
  | B of int
[@@or_null]

[%%expect{|
type no_param = A | B of int [@@or_null]
|}]

type no_param_nonfloat =
  | A_nonfloat
  | B_nonfloat of t_non_float
[@@or_null]

[%%expect{|
type no_param_nonfloat = A_nonfloat | B_nonfloat of t_non_float [@@or_null]
|}]

type succeeds_sep = no_param_nonfloat accepts_sep
type succeeds_nonfloat = no_param_nonfloat accepts_nonfloat

[%%expect{|
type succeeds_sep = no_param_nonfloat accepts_sep
type succeeds_nonfloat = no_param_nonfloat accepts_nonfloat
|}]

type float_payload =
  | Nope_float
  | Yep_float of float
[@@or_null]

[%%expect{|
type float_payload = Nope_float | Yep_float of float [@@or_null]
|}]

type float_payload_fails_sep = float_payload accepts_sep

[%%expect{|
Line 1, characters 31-44:
1 | type float_payload_fails_sep = float_payload accepts_sep
                                   ^^^^^^^^^^^^^
Error: This type "float_payload" should be an instance of type
         "('a : any separable)"
       The layout of float_payload is value_or_null
         because of the definition of float_payload at lines 1-4, characters 0-11.
       But the layout of float_payload must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type float_payload_fails_nonfloat = float_payload accepts_nonfloat

[%%expect{|
Line 1, characters 36-49:
1 | type float_payload_fails_nonfloat = float_payload accepts_nonfloat
                                        ^^^^^^^^^^^^^
Error: This type "float_payload" should be an instance of type
         "('a : value_or_null non_float)"
       The layout of float_payload is value_or_null
         because of the definition of float_payload at lines 1-4, characters 0-11.
       But the layout of float_payload must be a sublayout of
           value_or_null non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-56.
|}]

type void : void mod everything
external void : unit -> void = "%unbox_unit"

[%%expect{|
type void : void mod everything
external void : unit -> void = "%unbox_unit"
|}]

type void_null =
  | Null_void of void
  | This_void of int
[@@or_null]

[%%expect{|
type void_null = Null_void of void | This_void of int [@@or_null]
|}]

type void_null_flipped =
  | This_void_flipped of int
  | Null_void_flipped of #(void * void)
[@@or_null]

[%%expect{|
type void_null_flipped =
    This_void_flipped of int
  | Null_void_flipped of #(void * void) [@@or_null]
|}]

type void_null_succeeds_sep = void_null accepts_sep
type void_null_succeeds_nonfloat = void_null accepts_nonfloat

[%%expect{|
type void_null_succeeds_sep = void_null accepts_sep
type void_null_succeeds_nonfloat = void_null accepts_nonfloat
|}]

(* A void null constructor with a [float] payload is maybe-separable (like
   [float or_null]), unlike the [int] payload above. Separability is a jkind
   axis computed structurally from the payload and does not depend on the
   flat-float-array optimization, so this is rejected identically under both
   the flat-float-array and no-flat-float-array configurations. *)

type void_null_float =
  | Null_void_float of void
  | This_void_float of float
[@@or_null]

[%%expect{|
type void_null_float = Null_void_float of void | This_void_float of float [@@or_null]
|}]

type void_null_float_fails_sep = void_null_float accepts_sep

[%%expect{|
Line 1, characters 33-48:
1 | type void_null_float_fails_sep = void_null_float accepts_sep
                                     ^^^^^^^^^^^^^^^
Error: This type "void_null_float" should be an instance of type
         "('a : any separable)"
       The layout of void_null_float is value_or_null
         because of the definition of void_null_float at lines 1-4, characters 0-11.
       But the layout of void_null_float must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type void_null_float_fails_nonfloat = void_null_float accepts_nonfloat

[%%expect{|
Line 1, characters 38-53:
1 | type void_null_float_fails_nonfloat = void_null_float accepts_nonfloat
                                          ^^^^^^^^^^^^^^^
Error: This type "void_null_float" should be an instance of type
         "('a : value_or_null non_float)"
       The layout of void_null_float is value_or_null
         because of the definition of void_null_float at lines 1-4, characters 0-11.
       But the layout of void_null_float must be a sublayout of
           value_or_null non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-56.
|}]

type void_alias = void
type 'a void_param : void

[%%expect{|
type void_alias = void
type 'a void_param : void
|}]

type void_alias_null =
  | Null_void_alias of void_alias
  | This_void_alias of string
[@@or_null]

type 'a void_param_null =
  | Null_void_param of 'a void_param
  | This_void_param of int
[@@or_null]

[%%expect{|
type void_alias_null =
    Null_void_alias of void_alias
  | This_void_alias of string [@@or_null]
type 'a void_param_null =
    Null_void_param of 'a void_param
  | This_void_param of int [@@or_null]
|}]

type 'a constrained_void_null =
  | Null_constrained of 'a
  | This_constrained of int
  constraint 'a = void
[@@or_null]

[%%expect{|
type 'a constrained_void_null =
    Null_constrained of 'a
  | This_constrained of int constraint 'a = void [@@or_null]
|}]

type recursive_void_null =
  | Null_recursive_void of recursive_void
  | This_recursive_void of int
[@@or_null]
and recursive_void : void mod everything

type recursive_void_alias = void
and recursive_void_alias_null =
  | Null_recursive_void_alias of recursive_void_alias
  | This_recursive_void_alias of int
[@@or_null]

type recursive_payload = int
and recursive_payload_null =
  | Null_recursive_payload
  | This_recursive_payload of recursive_payload
[@@or_null]

[%%expect{|
type recursive_void_null =
    Null_recursive_void of recursive_void
  | This_recursive_void of int [@@or_null]
and recursive_void : void mod everything
type recursive_void_alias = void
and recursive_void_alias_null =
    Null_recursive_void_alias of recursive_void_alias
  | This_recursive_void_alias of int [@@or_null]
type recursive_payload = int
and recursive_payload_null =
    Null_recursive_payload
  | This_recursive_payload of recursive_payload [@@or_null]
|}]

type no_nonvoid_payload =
  | A_no_nonvoid
  | B_no_nonvoid of void
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type no_nonvoid_payload =
2 |   | A_no_nonvoid
3 |   | B_no_nonvoid of void
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one null constructor and one payload constructor.
|}]

type two_nonvoid_payloads =
  | A_nonvoid of int
  | B_nonvoid of string
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type two_nonvoid_payloads =
2 |   | A_nonvoid of int
3 |   | B_nonvoid of string
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one null constructor and one payload constructor.
|}]

(* The null constructor's void argument counts towards the mod-bounds of the
   type: matching on the null constructor synthesizes a value of the argument
   type. *)

type nonportable_void : void

type crossing_void_null : value_or_null mod portable =
  | Null_crossing_void of void
  | This_crossing_void of int
[@@or_null]

type noncrossing_void_null : value_or_null mod portable =
  | Null_noncrossing_void of nonportable_void
  | This_noncrossing_void of int
[@@or_null]

[%%expect{|
type nonportable_void : void
type crossing_void_null =
    Null_crossing_void of void
  | This_crossing_void of int [@@or_null]
Lines 8-11, characters 0-11:
 8 | type noncrossing_void_null : value_or_null mod portable =
 9 |   | Null_noncrossing_void of nonportable_void
10 |   | This_noncrossing_void of int
11 | [@@or_null]
Error: This type definition does not satisfy its kind annotation
         value_or_null mod portable,
       because nonportable_void is not mod portable.
|}]

type two_nullary =
  | A_nullary
  | B_nullary
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type two_nullary =
2 |   | A_nullary
3 |   | B_nullary
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one null constructor and one payload constructor.
|}]

(* Both arguments are aliases from the same recursive group, so neither can
   be classified before the whole group is translated. *)

type both_aliased_void = void
and both_aliased_int = int
and both_aliased_null =
  | Null_both_aliased of both_aliased_void
  | This_both_aliased of both_aliased_int
[@@or_null]

[%%expect{|
type both_aliased_void = void
and both_aliased_int = int
and both_aliased_null =
    Null_both_aliased of both_aliased_void
  | This_both_aliased of both_aliased_int [@@or_null]
|}]

(* Null-first declarations exercise [Ctype.contained_without_boxing] (via the
   unboxed-recursion check and the jkind update order) before argument sorts
   are known, when the null constructor cannot yet be told apart from the
   payload constructor. *)

type null_first_rec =
  | Null_first of void
  | This_first of first_wrapper
[@@or_null]
and first_wrapper = { fw : int } [@@unboxed]

[%%expect{|
type null_first_rec = Null_first of void | This_first of first_wrapper [@@or_null]
and first_wrapper = { fw : int; } [@@unboxed]
|}]

type bad_rec =
  | Null_bad of void
  | This_bad of bad_wrapper
[@@or_null]
and bad_wrapper = { bw : bad_rec } [@@unboxed]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type bad_rec =
2 |   | Null_bad of void
3 |   | This_bad of bad_wrapper
4 | [@@or_null]
Error: The definition of "bad_rec" is recursive without boxing:
         "bad_rec" contains "bad_wrapper",
         "bad_wrapper" contains "bad_rec"
|}]

type portable_payload : value_or_null mod portable =
  | Portable_none
  | Portable_some of (unit -> unit) @@ portable
[@@or_null]

[%%expect{|
type portable_payload =
    Portable_none
  | Portable_some of (unit -> unit) @@ portable [@@or_null]
|}]

type nonportable_payload : value_or_null mod portable =
  | Nonportable_none
  | Nonportable_some of (unit -> unit)
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type nonportable_payload : value_or_null mod portable =
2 |   | Nonportable_none
3 |   | Nonportable_some of (unit -> unit)
4 | [@@or_null]
Error: The kind of type "nonportable_payload" is
           value_or_null non_float mod aliased immutable
         because an [@@or_null] type gets the kind of or_null
         applied to its payload type.
       But the kind of type "nonportable_payload" must be a subkind of
           value_or_null mod portable
         because of the annotation on the declaration of the type nonportable_payload.
|}]

(* A custom [@@or_null] type gets the same kind as the builtin ['a or_null]:
   it crosses everything modulo a with-bound on its payload. *)

type 'a crosses_like_or_null
  : value_or_null mod many forkable portable contended unyielding with 'a =
  | Cross_some of 'a
  | Cross_none
[@@or_null]

[%%expect{|
type 'a crosses_like_or_null = Cross_some of 'a | Cross_none [@@or_null]
|}]

(* [int crosses_like_or_null] crosses contention, like [int or_null]. *)

let cross_contention (x : int crosses_like_or_null @ contended) =
  (x : _ @ uncontended)

[%%expect{|
val cross_contention :
  int crosses_like_or_null @ contended -> int crosses_like_or_null = <fun>
|}]

(* But the crossing depends on the payload: [int ref crosses_like_or_null]
   does not cross contention. *)

let bad_cross_contention (x : int ref crosses_like_or_null @ contended) =
  (x : _ @ uncontended)

[%%expect{|
Line 2, characters 3-4:
2 |   (x : _ @ uncontended)
       ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* A payload under a modality contributes its with-bound under that modality:
   ['a @@ contended] makes the type cross contention regardless of 'a. *)

type 'a contended_payload =
  | Contended_none
  | Contended_some of 'a @@ contended
[@@or_null]

[%%expect{|
type 'a contended_payload =
    Contended_none
  | Contended_some of 'a @@ contended [@@or_null]
|}]

let cross_contention_modality (x : int ref contended_payload @ contended) =
  (x : _ @ uncontended)

[%%expect{|
val cross_contention_modality :
  int ref contended_payload @ contended -> int ref contended_payload = <fun>
|}]

(* The precise kind is available for signature inclusion. *)

module M_crosses : sig
  type 'a t : value_or_null mod many portable contended with 'a
end = struct
  type 'a t = Mk_some of 'a | Mk_none [@@or_null]
end

[%%expect{|
module M_crosses :
  sig type 'a t : value_or_null mod many portable contended with 'a end
|}]

(* The kind is a subkind of weaker kinds. The declaration below carries no
   kind annotation, so the equations check the computed kind. ['a] staying
   unconstrained in the printed outputs is part of the test: the checker
   could otherwise satisfy the annotations by strengthening the parameter's
   kind. *)

type 'a plain = Plain_some of 'a | Plain_none [@@or_null]

type 'a sub_top : value_or_null = 'a plain

type 'a sub_portable : value_or_null mod portable with 'a = 'a plain

[%%expect{|
type 'a plain = Plain_some of 'a | Plain_none [@@or_null]
type 'a sub_top = 'a plain
type 'a sub_portable = 'a plain
|}]

(* ... but not of non-null kinds. *)

type 'a sub_value : value = 'a plain

[%%expect{|
Line 1, characters 0-36:
1 | type 'a sub_value : value = 'a plain
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "'a plain" is value_or_null
         because of the definition of plain at line 1, characters 0-57.
       But the layout of type "'a plain" must be a sublayout of value
         because of the definition of sub_value at line 1, characters 0-36.
|}]

(* A kind constraint on the type parameter strengthens the with-bound, so
   the declaration's kind can cross an axis without mentioning ['a]. *)

type ('a : value mod portable) constrained_param : value_or_null mod portable =
  | Constrained_some of 'a
  | Constrained_none
[@@or_null]

[%%expect{|
type ('a : value mod portable) constrained_param =
    Constrained_some of 'a
  | Constrained_none [@@or_null]
|}]

(* Custom [@@or_null] variants without a type parameter cross according to
   their payload. *)

type mono = Mono_none | Mono_some of int [@@or_null]

let cross_mono (x : mono @ contended) = (x : _ @ uncontended)

[%%expect{|
type mono = Mono_none | Mono_some of int [@@or_null]
val cross_mono : mono @ contended -> mono = <fun>
|}]

(* ... and a monomorphic payload that doesn't cross blocks the crossing. *)

type mono_ref = Mono_ref_none | Mono_ref_some of int ref [@@or_null]

let bad_cross_mono (x : mono_ref @ contended) = (x : _ @ uncontended)

[%%expect{|
type mono_ref = Mono_ref_none | Mono_ref_some of int ref [@@or_null]
Line 3, characters 49-50:
3 | let bad_cross_mono (x : mono_ref @ contended) = (x : _ @ uncontended)
                                                     ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* An [@@unboxed] wrapper carrying a modality as the payload: the wrapper's
   modality provides the crossing. *)

type portable_box = { portable_field : (unit -> unit) @@ portable }
[@@unboxed]

type fn_or_null = Fn_none | Fn_some of portable_box [@@or_null]

let cross_fn (x : fn_or_null @ nonportable) = (x : _ @ portable)

[%%expect{|
type portable_box = { portable_field : unit -> unit @@ portable; } [@@unboxed]
type fn_or_null = Fn_none | Fn_some of portable_box [@@or_null]
val cross_fn : fn_or_null -> fn_or_null = <fun>
|}]

(* ... and the reverse nesting: a custom [@@or_null] as the field of an
   [@@unboxed] wrapper carrying a modality. *)

type fn_or_null_plain =
  | Plain_fn_none
  | Plain_fn_some of (unit -> unit)
[@@or_null]

type wrapped_or_null = { wrapped_field : fn_or_null_plain @@ portable }
[@@unboxed]

let cross_wrapped (x : wrapped_or_null @ nonportable) = (x : _ @ portable)

[%%expect{|
type fn_or_null_plain = Plain_fn_none | Plain_fn_some of (unit -> unit) [@@or_null]
type wrapped_or_null = { wrapped_field : fn_or_null_plain @@ portable; } [@@unboxed]
val cross_wrapped : wrapped_or_null -> wrapped_or_null = <fun>
|}]

type probe_result : value =
  | Probe_none
  | Probe_some of int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type probe_result : value =
2 |   | Probe_none
3 |   | Probe_some of int
4 | [@@or_null]
Error: The layout of type "probe_result" is value_or_null non_pointer
         because an [@@or_null] type gets the layout of or_null
         applied to its payload type.
       But the layout of type "probe_result" must be a sublayout of value
         because of the annotation on the declaration of the type probe_result.
|}]

(* Custom [@@or_null] types with unused type parameters. *)

type 'a unused_param =
  | A
  | B of int
[@@or_null]

[%%expect{|
type 'a unused_param = A | B of int [@@or_null]
|}]

type ('a, 'b) multi_param =
  | Nope_multi
  | Yep_multi of ('a list * 'b)
[@@or_null]

[%%expect{|
type ('a, 'b) multi_param = Nope_multi | Yep_multi of ('a list * 'b) [@@or_null]
|}]

type ('a, 'b) multi_param_succeeds_sep = ('a, 'b) multi_param accepts_sep

[%%expect{|
type ('a, 'b) multi_param_succeeds_sep = ('a, 'b) multi_param accepts_sep
|}]

type ('a, 'b) multi_param_succeeds_nonfloat =
  ('a, 'b) multi_param accepts_nonfloat

[%%expect{|
type ('a, 'b) multi_param_succeeds_nonfloat =
    ('a, 'b) multi_param accepts_nonfloat
|}]

type ('a, 'b) eq = unit constraint 'a = 'b

[%%expect{|
type ('b, 'a) eq = unit constraint 'a = 'b
|}]

type ('a, 'b) inferred_constraint =
  | Nope_inferred_constraint
  | Yep_inferred_constraint of ('a, 'b) eq
[@@or_null]

[%%expect{|
type ('a, 'b) inferred_constraint =
    Nope_inferred_constraint
  | Yep_inferred_constraint of ('a, 'a) eq constraint 'b = 'a [@@or_null]
|}]

type ('a, 'b) second_param =
  | Nope_second
  | Yep_second of 'b
[@@or_null]

[%%expect{|
type ('a, 'b) second_param = Nope_second | Yep_second of 'b [@@or_null]
|}]

type ('a, 'b) swapped = ('b, 'a) second_param

[%%expect{|
type ('a, 'b) swapped = ('b, 'a) second_param
|}]

type second_param_succeeds_sep =
  (float, t_non_float) second_param accepts_sep

type second_param_succeeds_nonfloat =
  (float, t_non_float) second_param accepts_nonfloat

[%%expect{|
type second_param_succeeds_sep =
    (float, t_non_float) second_param accepts_sep
type second_param_succeeds_nonfloat =
    (float, t_non_float) second_param accepts_nonfloat
|}]

type second_param_fails_nonfloat =
  (t_non_float, float) second_param accepts_nonfloat

[%%expect{|
Line 2, characters 2-35:
2 |   (t_non_float, float) second_param accepts_nonfloat
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "(t_non_float, float) second_param"
       should be an instance of type "('a : value_or_null non_float)"
       The layout of (t_non_float, float) second_param is value_or_null
         because of the definition of second_param at lines 1-4, characters 0-11.
       But the layout of (t_non_float, float) second_param must be a sublayout of
         value_or_null non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-56.
|}]

type swapped_succeeds_nonfloat =
  (t_non_float, float) swapped accepts_nonfloat

[%%expect{|
type swapped_succeeds_nonfloat =
    (t_non_float, float) swapped accepts_nonfloat
|}]

type swapped_fails_nonfloat =
  (float, t_non_float) swapped accepts_nonfloat

[%%expect{|
Line 2, characters 2-30:
2 |   (float, t_non_float) swapped accepts_nonfloat
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type
         "(float, t_non_float) swapped" = "(t_non_float, float) second_param"
       should be an instance of type "('a : value_or_null non_float)"
       The layout of (float, t_non_float) swapped is value_or_null
         because of the definition of second_param at lines 1-4, characters 0-11.
       But the layout of (float, t_non_float) swapped must be a sublayout of
           value_or_null non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-56.
|}]

type bad_payload =
  | Nope_bad
  | Yep_bad of int t
[@@or_null]

[%%expect{|
Line 3, characters 15-20:
3 |   | Yep_bad of int t
                   ^^^^^
Error: The layout of type "int t" is value_or_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the layout of type "int t" must be a sublayout of
           value_maybe_separable
         because the payload of bad_payload has layout value.
|}]


type 'a gadt =
  | A : 'a gadt
  | B : 'a -> 'a gadt
[@@or_null]

[%%expect{|
type 'a gadt = A : 'a gadt | B : 'a -> 'a gadt [@@or_null]
|}]

type 'a concrete_gadt =
  | Null : int concrete_gadt
  | This : string -> bool concrete_gadt
[@@or_null]

[%%expect{|
type 'a concrete_gadt =
    Null : int concrete_gadt
  | This : string -> bool concrete_gadt [@@or_null]
|}]

type ('a : any) widened_bad_jkind =
  | A
  | B of 'a
[@@or_null]
[%%expect{|
type ('a : value_maybe_separable) widened_bad_jkind = A | B of 'a [@@or_null]
|}]

type ('a : value_or_null) widened_bad_jkind =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
type ('a : value_maybe_separable) widened_bad_jkind = A | B of 'a [@@or_null]
|}]

type ('a : any) widened_any : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
type ('a : value_maybe_separable) widened_any = A | B of 'a [@@or_null]
|}]

type ('a : value_or_null) widened_nullable : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
type ('a : value_maybe_separable) widened_nullable = A | B of 'a [@@or_null]
|}]

type ('a : immediate) widened_immediate : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
type ('a : immediate) widened_immediate = A | B of 'a [@@or_null]
|}]

type ('a : immediate_or_null) widened_immediate_or_null : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
type ('a : immediate) widened_immediate_or_null = A | B of 'a [@@or_null]
|}]

type ('a : value) wrong_result_kind : value =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type ('a : value) wrong_result_kind : value =
2 |   | A
3 |   | B of 'a
4 | [@@or_null]
Error: The layout of type "wrong_result_kind" is value_or_null
         because an [@@or_null] type gets the layout of or_null
         applied to its payload type.
       But the layout of type "wrong_result_kind" must be a sublayout of value
         because of the annotation on the declaration of the type wrong_result_kind.
|}]

type ('a : float64) wrong_payload_kind : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
Line 3, characters 9-11:
3 |   | B of 'a
             ^^
Error: The layout of type "'a" is float64
         because of the annotation on 'a in the declaration of the type
                                      wrong_payload_kind.
       But the layout of type "'a" must be a value layout
         because the payload of wrong_payload_kind has layout value.
|}]

module M : sig
  type 'a t
end = struct
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end

[%%expect{|
Lines 3-8, characters 6-3:
3 | ......struct
4 |   type ('a : value) t : value_or_null =
5 |     | Nope
6 |     | Yep of 'a
7 |   [@@or_null]
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Nope | Yep of 'a [@@or_null] end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type 'a t = Nope | Yep of 'a [@@or_null]
       is not included in
         type 'a t
       The layout of the first is value_or_null
         because of the definition of t at lines 4-7, characters 2-13.
       But the layout of the first must be a sublayout of value
         because of the definition of t at line 2, characters 2-11.
|}]

module M : sig
  type ('a : value) t : value_or_null
end = struct
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end

[%%expect{|
module M : sig type 'a t : value_or_null end
|}]

module M : sig
  type ('a : value) t =
    | Nope
    | Yep of 'a
end = struct
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end

[%%expect{|
Lines 5-10, characters 6-3:
 5 | ......struct
 6 |   type ('a : value) t : value_or_null =
 7 |     | Nope
 8 |     | Yep of 'a
 9 |   [@@or_null]
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Nope | Yep of 'a [@@or_null] end
       is not included in
         sig type 'a t = Nope | Yep of 'a end
       Type declarations do not match:
         type 'a t = Nope | Yep of 'a [@@or_null]
       is not included in
         type 'a t = Nope | Yep of 'a
       Their internal representations differ:
       the first declaration has a constructor represented as a null pointer.
       Hint: add [@@or_null] or [@@or_null_reexport].
|}]

module M : sig
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end = struct
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end

[%%expect{|
module M : sig type 'a t = Nope | Yep of 'a [@@or_null] end
|}]

module New_shape_inclusion : sig
  type t : value_or_null mod non_float =
    | New_shape_null
    | New_shape_payload of t_non_float
  [@@or_null]

  type ('a, 'b) multi : value_or_null mod non_float =
    | New_shape_multi_null
    | New_shape_multi_payload of ('a list * 'b)
  [@@or_null]
end = struct
  type t : value_or_null mod non_float =
    | New_shape_null
    | New_shape_payload of t_non_float
  [@@or_null]

  type ('a, 'b) multi : value_or_null mod non_float =
    | New_shape_multi_null
    | New_shape_multi_payload of ('a list * 'b)
  [@@or_null]
end

[%%expect{|
module New_shape_inclusion :
  sig
    type t = New_shape_null | New_shape_payload of t_non_float [@@or_null]
    type ('a, 'b) multi =
        New_shape_multi_null
      | New_shape_multi_payload of ('a list * 'b) [@@or_null]
  end
|}]

module New_shape_abstract_inclusion : sig
  type t : value_or_null mod non_float

  type 'a unused : value_or_null mod non_float

  type ('a, 'b) multi : value_or_null mod non_float
end = struct
  type t =
    | New_shape_abstract_null
    | New_shape_abstract_payload of t_non_float
  [@@or_null]

  type 'a unused =
    | New_shape_abstract_unused_null
    | New_shape_abstract_unused_payload of int
  [@@or_null]

  type ('a, 'b) multi =
    | New_shape_abstract_multi_null
    | New_shape_abstract_multi_payload of ('a list * 'b)
  [@@or_null]
end

[%%expect{|
module New_shape_abstract_inclusion :
  sig
    type t : value_or_null non_float
    type 'a unused : value_or_null non_float
    type ('a, 'b) multi : value_or_null non_float
  end
|}]

module New_shape_second_param_inclusion : sig
  type ('a, 'b : value mod non_float) second :
    value_or_null mod non_float
end = struct
  type ('a, 'b : value mod non_float) second =
    | New_shape_second_null
    | New_shape_second_payload of 'b
  [@@or_null]
end

[%%expect{|
module New_shape_second_param_inclusion :
  sig type ('a, 'b : value non_float) second : value_or_null non_float end
|}]

module Bad_second_param_inclusion : sig
  type ('a, 'b) second : value_or_null mod non_float
end = struct
  type ('a, 'b) second =
    | Bad_null
    | Bad_payload of 'b
  [@@or_null]
end

[%%expect{|
Lines 3-8, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) second =
5 |     | Bad_null
6 |     | Bad_payload of 'b
7 |   [@@or_null]
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a, 'b) second = Bad_null | Bad_payload of 'b [@@or_null]
         end
       is not included in
         sig type ('a, 'b) second : value_or_null non_float end
       Type declarations do not match:
         type ('a, 'b) second = Bad_null | Bad_payload of 'b [@@or_null]
       is not included in
         type ('a, 'b) second : value_or_null non_float
       The layout of the first is value_or_null
         because of the definition of second at lines 4-7, characters 2-13.
       But the layout of the first must be a sublayout of
           value_or_null non_float
         because of the definition of second at line 2, characters 2-52.
|}]

module M : sig
  type ('a : value) t : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]
end = struct
  type ('a : value) t =
    | Nope
    | Yep of 'a
end

[%%expect{|
Lines 6-10, characters 6-3:
 6 | ......struct
 7 |   type ('a : value) t =
 8 |     | Nope
 9 |     | Yep of 'a
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Nope | Yep of 'a end
       is not included in
         sig type 'a t = Nope | Yep of 'a [@@or_null] end
       Type declarations do not match:
         type 'a t = Nope | Yep of 'a
       is not included in
         type 'a t = Nope | Yep of 'a [@@or_null]
       Their internal representations differ:
       the second declaration has a constructor represented as a null pointer.
       Hint: add [@@or_null] or [@@or_null_reexport].
|}]

module M : sig
  type ('a : value) t1 : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]

  type ('a : value) t2 = 'a t1 =
    | Nope
    | Yep of 'a
  [@@or_null]
end = struct
  type ('a : value) t1 : value_or_null =
    | Nope
    | Yep of 'a
  [@@or_null]

  type ('a : value) t2 = 'a t1 =
    | Nope
    | Yep of 'a
  [@@or_null]
end

[%%expect{|
module M :
  sig
    type 'a t1 = Nope | Yep of 'a [@@or_null]
    type 'a t2 = 'a t1 = Nope | Yep of 'a [@@or_null]
  end
|}]

let x : int M.t2 = M.Yep 3

[%%expect{|
val x : int M.t2 = M.Yep 3
|}]

type ('a : value) t1 : value_or_null =
  | Nope
  | Yep of 'a
[@@or_null]

type ('a : value) t2 = 'a t1

[%%expect{|
type 'a t1 = Nope | Yep of 'a [@@or_null]
type 'a t2 = 'a t1
|}]

type ('a : value) t1 : value_or_null =
  | Nope
  | Yep of 'a
[@@or_null]

type ('a : value) t2 = 'a t1 [@@or_null]

[%%expect{|
type 'a t1 = Nope | Yep of 'a [@@or_null]
type 'a t2 = 'a t1
|}]

type ('a : value) t1 : value_or_null =
  | Nope
  | Yep of 'a
[@@or_null]

type ('a : value) t2 = 'a t1 =
  | Nope
  | Yep of 'a
[@@or_null]

type ('a : value) t3 = 'a t2 =
  | Nope
  | Yep of 'a
[@@or_null]

[%%expect{|
type 'a t1 = Nope | Yep of 'a [@@or_null]
type 'a t2 = 'a t1 = Nope | Yep of 'a [@@or_null]
type 'a t3 = 'a t2 = Nope | Yep of 'a [@@or_null]
|}]

(* GADT tests. *)

type mixed =
  | Mixed_null
  | Mixed_this : 'a -> mixed
[@@or_null]

let mixed_is_null = function
  | Mixed_null -> true
  | Mixed_this _ -> false

let mixed_int = Mixed_this 42
let mixed_string = Mixed_this "hello"

[%%expect{|
type mixed = Mixed_null | Mixed_this : 'a -> mixed [@@or_null]
val mixed_is_null : mixed -> bool = <fun>
val mixed_int : mixed = Mixed_this <poly>
val mixed_string : mixed = Mixed_this <poly>
|}]

type 'a mixed_null_gadt =
  | Mixed_gadt_null : int mixed_null_gadt
  | Mixed_plain_this of 'a
[@@or_null]

let mixed_to_option : type a. a mixed_null_gadt -> a option = function
  | Mixed_gadt_null -> None
  | Mixed_plain_this x -> Some x

let mixed_string_payload (x : string mixed_null_gadt) =
  match x with Mixed_plain_this s -> s

[%%expect{|
type 'a mixed_null_gadt =
    Mixed_gadt_null : int mixed_null_gadt
  | Mixed_plain_this of 'a [@@or_null]
val mixed_to_option : 'a mixed_null_gadt -> 'a option = <fun>
val mixed_string_payload : string mixed_null_gadt -> string = <fun>
|}]

type _ indexed =
  | Indexed_null : int indexed
  | Indexed_this : string -> string indexed
[@@or_null]

let indexed_string (x : string indexed) =
  match x with Indexed_this s -> s
let indexed_null (x : int indexed) =
  match x with Indexed_null -> ()

[%%expect{|
type _ indexed =
    Indexed_null : int indexed
  | Indexed_this : string -> string indexed [@@or_null]
val indexed_string : string indexed -> string = <fun>
val indexed_null : int indexed -> unit = <fun>
|}]

let bad_index (x : string indexed) =
  match x with Indexed_null -> ()

[%%expect{|
Line 2, characters 15-27:
2 |   match x with Indexed_null -> ()
                   ^^^^^^^^^^^^
Error: This pattern matches values of type "int indexed"
       but a pattern was expected which matches values of type "string indexed"
       Type "int" is not compatible with type "string"
|}]

type 'a poly_gadt =
  | Poly_null : 'a poly_gadt
  | Poly_this : 'a -> 'a poly_gadt
[@@or_null]

let missing_gadt_payload : int poly_gadt -> unit =
  function Poly_null -> ()
let missing_gadt_null : int poly_gadt -> unit =
  function Poly_this _ -> ()

[%%expect{|
type 'a poly_gadt = Poly_null : 'a poly_gadt | Poly_this : 'a -> 'a poly_gadt [@@or_null]
Line 7, characters 2-26:
7 |   function Poly_null -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Poly_this _"

val missing_gadt_payload : int poly_gadt -> unit = <fun>
Line 9, characters 2-28:
9 |   function Poly_this _ -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Poly_null"

val missing_gadt_null : int poly_gadt -> unit = <fun>
|}]

let int_gadt_array (x : int poly_gadt) = [|x|]

[%%expect{|
val int_gadt_array : int poly_gadt -> int poly_gadt array = <fun>
|}]

let float_gadt_array (x : float poly_gadt) = [|x|]

[%%expect{|
Line 1, characters 47-48:
1 | let float_gadt_array (x : float poly_gadt) = [|x|]
                                                   ^
Error: The value "x" has type "float poly_gadt"
       but an expression was expected of type "('a : value_maybe_null)"
       The layout of float poly_gadt is value_or_null
         because of the definition of poly_gadt at lines 1-4, characters 0-11.
       But the layout of float poly_gadt must be a sublayout of
           value_maybe_null
         because it's the type of an array element.
|}]

module Gadt_ground : sig type t : immediate_or_null end = struct
  type t = N : t | P : int -> t [@@or_null]
end
module Gadt_float : sig type t : value_or_null end = struct
  type t = N : t | P : float -> t [@@or_null]
end
module Gadt_mixed : sig type 'a t : value_or_null end = struct
  type 'a t = N | P : 'a -> 'a t [@@or_null]
end

[%%expect{|
module Gadt_ground : sig type t : immediate_or_null end
module Gadt_float : sig type t : value_or_null end
module Gadt_mixed : sig type 'a t : value_or_null end
|}]

module Gadt_nonnull : sig type t : value end = struct
  type t = N : t | P : int -> t [@@or_null]
end

[%%expect{|
Lines 1-3, characters 47-3:
1 | ...............................................struct
2 |   type t = N : t | P : int -> t [@@or_null]
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = N : t | P : int -> t [@@or_null] end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = N : t | P : int -> t [@@or_null]
       is not included in
         type t
       The layout of the first is value_or_null non_pointer
         because of the definition of t at line 2, characters 2-43.
       But the layout of the first must be a sublayout of value
         because of the definition of t at line 1, characters 26-40.
|}]

(* The declaration parameter remains wider than the payload variable. *)
type ('a : any) wide_gadt =
  | Wide_null : ('a : any). 'a wide_gadt
  | Wide_this : 'a -> 'a wide_gadt
[@@or_null]
let wide_float64_null : float# wide_gadt = Wide_null

[%%expect{|
type ('a : any) wide_gadt =
    Wide_null : ('a : any). 'a wide_gadt
  | Wide_this : 'a -> 'a wide_gadt [@@or_null]
val wide_float64_null : float# wide_gadt = Wide_null
|}]

let wide_float64_payload = Wide_this #1.0

[%%expect{|
Line 1, characters 37-41:
1 | let wide_float64_payload = Wide_this #1.0
                                         ^^^^
Error: This constant has type "float#" but an expression was expected of type
         "('a : value)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because of the definition of wide_gadt at lines 1-4, characters 0-11.
|}]

module Gadt_wide_crossing : sig
  type ('a : any) t : value_or_null mod portable
end = struct
  type ('a : any) t =
    | N : 'a t
    | P : ('b : value mod portable). 'b -> 'b t
  [@@or_null]
end

module Gadt_wide_bound : sig
  type ('a : any) t : value_or_null mod portable with 'a
end = struct
  type ('a : any) t = N : 'b t | P : 'c -> 'c t [@@or_null]
end

module Gadt_modality : sig
  type ('a : any) t : value_or_null mod portable
end = struct
  type ('a : any) t =
    | N : 'a t
    | P : 'b @@ portable -> 'b t
  [@@or_null]
end

[%%expect{|
module Gadt_wide_crossing :
  sig type ('a : any) t : value_or_null mod portable end
module Gadt_wide_bound :
  sig type ('a : any) t : value_or_null mod portable with 'a end
module Gadt_modality : sig type ('a : any) t : value_or_null mod portable end
|}]

module Gadt_existential : sig
  type t : value_or_null mod portable
end = struct
  type t = N : t | P : ('a : value mod portable). 'a -> t [@@or_null]
end

type 'a compound_index =
  | Compound_null : 'a compound_index
  | Compound_this : 'b -> 'b list compound_index
[@@or_null]

type 'a existential_gadt =
  | Existential_null : 'a existential_gadt
  | Existential_this : ('b * ('b -> 'a)) -> 'a existential_gadt
[@@or_null]

[%%expect{|
module Gadt_existential : sig type t : value_or_null mod portable end
type 'a compound_index =
    Compound_null : 'a compound_index
  | Compound_this : 'b -> 'b list compound_index [@@or_null]
type 'a existential_gadt =
    Existential_null : 'a existential_gadt
  | Existential_this : ('b * ('b -> 'a)) -> 'a existential_gadt [@@or_null]
|}]

module Gadt_void : sig type t : immediate_or_null end = struct
  type t = N : void -> t | P : int -> t [@@or_null]
end
module Gadt_void_flipped : sig type t : immediate_or_null end = struct
  type t = P : int -> t | N : #(void * void) -> t [@@or_null]
end

[%%expect{|
module Gadt_void : sig type t : immediate_or_null end
module Gadt_void_flipped : sig type t : immediate_or_null end
|}]

(* N accepts a nonportable_void, so t cannot cross portability. *)
module Gadt_void_bound : sig
  type t : value_or_null mod portable
end = struct
  type t = N : nonportable_void -> t | P : int -> t [@@or_null]
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = N : nonportable_void -> t | P : int -> t [@@or_null]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = N : nonportable_void -> t | P : int -> t [@@or_null]
         end
       is not included in
         sig type t : value_or_null mod portable end
       Type declarations do not match:
         type t = N : nonportable_void -> t | P : int -> t [@@or_null]
       is not included in
         type t : value_or_null mod portable
       The kind of the first is immediate_or_null with nonportable_void
         because of the definition of t at line 4, characters 2-63.
       But the kind of the first must be a subkind of
           value_or_null mod portable
         because of the definition of t at line 2, characters 2-37.
|}]

type portable_void : void mod portable

module Gadt_portable_void_bound : sig
  type t : value_or_null mod portable
end = struct
  type t = N : portable_void -> t | P : int -> t [@@or_null]
end

[%%expect{|
type portable_void : void mod portable
module Gadt_portable_void_bound : sig type t : value_or_null mod portable end
|}]

(* The existential 'v is not known portable. Its bound still restricts t,
   even though P's payload is portable. *)
module Gadt_existential_void_bound : sig
  type ('a : any) t : value_or_null mod portable
end = struct
  type ('a : any) t =
    | N : ('v : void). 'v -> int t
    | P : ('b : value mod portable). 'b -> 'b t
  [@@or_null]
end

[%%expect{|
Lines 3-8, characters 6-3:
3 | ......struct
4 |   type ('a : any) t =
5 |     | N : ('v : void). 'v -> int t
6 |     | P : ('b : value mod portable). 'b -> 'b t
7 |   [@@or_null]
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : any) t =
               N : ('v : void). 'v -> int t
             | P : ('b : value mod portable). 'b -> 'b t [@@or_null]
         end
       is not included in
         sig type ('a : any) t : value_or_null mod portable end
       Type declarations do not match:
         type ('a : any) t =
             N : ('v : void). 'v -> int t
           | P : ('b : value mod portable). 'b -> 'b t [@@or_null]
       is not included in
         type ('a : any) t : value_or_null mod portable
       The kind of the first is value_or_null mod external_ with 'a
         because of the definition of t at lines 4-7, characters 2-13.
       But the kind of the first must be a subkind of
           value_or_null mod portable
         because of the definition of t at line 2, characters 2-48.
|}]

module Gadt_portable_existential_void_bound : sig
  type ('a : any) t : value_or_null mod portable
end = struct
  type ('a : any) t =
    | N : ('v : void mod portable). 'v -> int t
    | P : ('b : value mod portable). 'b -> 'b t
  [@@or_null]
end

[%%expect{|
module Gadt_portable_existential_void_bound :
  sig type ('a : any) t : value_or_null mod portable end
|}]

module Gadt_projected_void_bound : sig
  type ('a : any) t : value_or_null mod portable with 'a
end = struct
  type ('a : any) t =
    | N : ('v : void). 'v -> 'v t
    | P : int -> 'a t
  [@@or_null]
end

[%%expect{|
module Gadt_projected_void_bound :
  sig type ('a : any) t : value_or_null mod portable with 'a end
|}]

module Gadt_inclusion : sig
  type _ t = N : int t | P : string -> string t [@@or_null]
end = struct
  type _ t = N : int t | P : string -> string t [@@or_null]
end

[%%expect{|
module Gadt_inclusion :
  sig type _ t = N : int t | P : string -> string t [@@or_null] end
|}]

module Gadt_bad_inclusion : sig
  type _ t = N : int t | P : string -> string t [@@or_null]
end = struct
  type _ t = N : bool t | P : string -> string t [@@or_null]
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type _ t = N : bool t | P : string -> string t [@@or_null]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type _ t = N : bool t | P : string -> string t [@@or_null] end
       is not included in
         sig type _ t = N : int t | P : string -> string t [@@or_null] end
       Type declarations do not match:
         type _ t = N : bool t | P : string -> string t [@@or_null]
       is not included in
         type _ t = N : int t | P : string -> string t [@@or_null]
       Constructors do not match:
         "N : bool t"
       is not the same as:
         "N : int t"
       The type "bool t" is not equal to the type "int t"
       Type "bool" is not equal to type "int"
|}]

type _ nested_gadt =
  | Nested_null : int nested_gadt
  | Nested_this : string or_null -> string nested_gadt
[@@or_null]

[%%expect{|
Line 3, characters 18-32:
3 |   | Nested_this : string or_null -> string nested_gadt
                      ^^^^^^^^^^^^^^
Error: The layout of type "string or_null" is value_or_null
         because it is the primitive type or_null.
       But the layout of type "string or_null" must be a sublayout of
           value_maybe_separable
         because the payload of nested_gadt has layout value.
|}]

type _ unboxed_payload_gadt =
  | Unboxed_null : int unboxed_payload_gadt
  | Unboxed_this : float# -> float unboxed_payload_gadt
[@@or_null]

[%%expect{|
Line 3, characters 19-25:
3 |   | Unboxed_this : float# -> float unboxed_payload_gadt
                       ^^^^^^
Error: The layout of type "float#" is float64
         because it is the unboxed version of the primitive type float.
       But the layout of type "float#" must be a value layout
         because the payload of unboxed_payload_gadt has layout value.
|}]

type _ record_payload_gadt =
  | Record_null : int record_payload_gadt
  | Record_this : { x : string } -> string record_payload_gadt
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type _ record_payload_gadt =
2 |   | Record_null : int record_payload_gadt
3 |   | Record_this : { x : string } -> string record_payload_gadt
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       each constructor must be nullary or unary.
|}]

type _ multiple_payload_gadt =
  | Multiple_null : int multiple_payload_gadt
  | Multiple_this : string * int -> string multiple_payload_gadt
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type _ multiple_payload_gadt =
2 |   | Multiple_null : int multiple_payload_gadt
3 |   | Multiple_this : string * int -> string multiple_payload_gadt
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       each constructor must be nullary or unary.
|}]

module Gadt_group : sig
  type t : immediate_or_null
  type box = B of t
end = struct
  type t = N : t | P : int -> t [@@or_null]
  and box = B of t
end

type group_gadt = GN : group_gadt | GP : group_record -> group_gadt
[@@or_null]
and group_record = { next : group_gadt }
and group_void_gadt =
  | GVN : void -> group_void_gadt
  | GVP : group_record -> group_void_gadt
[@@or_null]

[%%expect{|
module Gadt_group : sig type t : immediate_or_null type box = B of t end
type group_gadt = GN : group_gadt | GP : group_record -> group_gadt [@@or_null]
and group_record = { next : group_gadt; }
and group_void_gadt =
    GVN : void -> group_void_gadt
  | GVP : group_record -> group_void_gadt [@@or_null]
|}]

(* The recursion check follows projected arguments before sorts are known. *)
type cycle = Cycle of cycle cycle_gadt [@@unboxed]
and ('a : value_or_null) cycle_gadt =
  | Cycle_null : 'b cycle_gadt
  | Cycle_this : ('c : value). 'c -> 'c cycle_gadt
[@@or_null]

[%%expect{|
Line 1, characters 0-50:
1 | type cycle = Cycle of cycle cycle_gadt [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "cycle" is recursive without boxing:
         "cycle" contains "cycle cycle_gadt",
         "cycle cycle_gadt" contains "cycle"
|}]

type cycle2 = Cycle2 of (cycle2, int) repeated_gadt [@@unboxed]
and ('a : value_or_null, 'b : value_or_null) repeated_gadt =
  | Repeated_null : ('d : value_or_null). ('d, int) repeated_gadt
  | Repeated_this : ('c : value). 'c -> ('c, 'c) repeated_gadt
[@@or_null]

[%%expect{|
Line 1, characters 0-63:
1 | type cycle2 = Cycle2 of (cycle2, int) repeated_gadt [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "cycle2" is recursive without boxing:
         "cycle2" contains "(cycle2, int) repeated_gadt",
         "(cycle2, int) repeated_gadt" contains "cycle2"
|}]

(* A payload modality must not erase the null argument's bound. *)
type gadt_null_bound =
  | Bound_null : nonportable_void -> gadt_null_bound
  | Bound_this : (unit -> unit) @@ portable -> gadt_null_bound
[@@or_null]
let require_portable_gadt (_ : gadt_null_bound @ portable) = ()

[%%expect{|
type gadt_null_bound =
    Bound_null : nonportable_void -> gadt_null_bound
  | Bound_this : (unit -> unit) @@ portable -> gadt_null_bound [@@or_null]
val require_portable_gadt : gadt_null_bound @ portable -> unit = <fun>
|}]

let cannot_cross_null_bound (x : gadt_null_bound @ nonportable) =
  require_portable_gadt x

[%%expect{|
Line 2, characters 24-25:
2 |   require_portable_gadt x
                            ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

type ordinary_null_bound =
  | Ordinary_bound_null of nonportable_void
  | Ordinary_bound_this of (unit -> unit) @@ portable
[@@or_null]
let require_portable_ordinary (_ : ordinary_null_bound @ portable) = ()

[%%expect{|
type ordinary_null_bound =
    Ordinary_bound_null of nonportable_void
  | Ordinary_bound_this of (unit -> unit) @@ portable [@@or_null]
val require_portable_ordinary : ordinary_null_bound @ portable -> unit =
  <fun>
|}]

let cannot_cross_ordinary_bound (x : ordinary_null_bound @ nonportable) =
  require_portable_ordinary x

[%%expect{|
Line 2, characters 28-29:
2 |   require_portable_ordinary x
                                ^
Error: This value is "nonportable" but is expected to be "portable".
|}]
