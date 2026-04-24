(* TEST
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
Error: This expression has type "'a t" but an expression was expected of type
         "('b : value)"
       The layout of 'a t is value maybe_separable maybe_null
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
       The layout of float t is value maybe_separable maybe_null
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
       The layout of float t is value maybe_separable maybe_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the layout of float t must be a sublayout of
           value non_float maybe_null
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

(* CR or-null: allow non-parameterized custom [@@or_null] types.
   Internal ticket 6853. *)

type no_param =
  | A
  | B of int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type no_param =
2 |   | A
3 |   | B of int
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one type parameter.
|}]

(* CR or-null: allow custom [@@or_null] types with unused type parameters.
   Internal ticket 6853. *)

type 'a unused_param =
  | A
  | B of int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type 'a unused_param =
2 |   | A
3 |   | B of int
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one nullary constructor and one unary constructor carrying the sole type parameter.
|}]

(* CR or-null: allow GADT custom [@@or_null] types.
   Internal ticket 6854. *)

type 'a gadt =
  | A : 'a gadt
  | B : 'a -> 'a gadt
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type 'a gadt =
2 |   | A : 'a gadt
3 |   | B : 'a -> 'a gadt
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       GADT constructors are not supported with [@@or_null].
|}]

(* CR or-null: allow GADT custom [@@or_null] types with concrete indices.
   Internal ticket 6854. *)

type 'a concrete_gadt =
  | Null : int concrete_gadt
  | This : string -> bool concrete_gadt
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type 'a concrete_gadt =
2 |   | Null : int concrete_gadt
3 |   | This : string -> bool concrete_gadt
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       GADT constructors are not supported with [@@or_null].
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
Error: The layout of type "wrong_result_kind" is
           value maybe_separable maybe_null
         because of the annotation on 'a in the declaration of the type
                                      wrong_result_kind.
       But the layout of type "wrong_result_kind" must be a sublayout of value
         because of the annotation on the declaration of the type wrong_result_kind.
|}]

type ('a : float64) wrong_payload_kind : value_or_null =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
Line 1, characters 6-18:
1 | type ('a : float64) wrong_payload_kind : value_or_null =
          ^^^^^^^^^^^^
Error: The layout of type "'a" is float64
         because of the annotation on 'a in the declaration of the type
                                      wrong_payload_kind.
       But the layout of type "'a" must be a value layout
         because the type argument of wrong_payload_kind has layout value.
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
       The layout of the first is value maybe_separable maybe_null
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
