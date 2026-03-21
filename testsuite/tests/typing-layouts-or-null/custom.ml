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
       The kind of 'a t is value_or_null
         because of the definition of t at lines 1-4, characters 0-11.
       But the kind of 'a t must be a subkind of value
         because of the definition of t at lines 1-4, characters 0-11.
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

type t_no_param =
  | A
  | B of int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type t_no_param =
2 |   | A
3 |   | B of int
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one type parameter.
|}]

type 'a wrong_payload =
  | A
  | B of int
[@@or_null]

[%%expect{|
Lines 1-4, characters 0-11:
1 | type 'a wrong_payload =
2 |   | A
3 |   | B of int
4 | [@@or_null]
Error: Invalid [@or_null] declaration:
       it must have exactly one nullary constructor and one unary constructor carrying the sole type parameter.
|}]

type ('a : value_or_null) bad_jkind =
  | A
  | B of 'a
[@@or_null]

[%%expect{|
Line 1, characters 6-24:
1 | type ('a : value_or_null) bad_jkind =
          ^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a" is value_or_null
         because of the annotation on 'a in the declaration of the type
                                      bad_jkind.
       But the kind of type "'a" must be a subkind of
           value_or_null mod non_null
         because the type argument of bad_jkind has kind value.
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
Error: The kind of type "wrong_result_kind" is value_or_null
         because of the annotation on 'a in the declaration of the type
                                      wrong_result_kind.
       But the kind of type "wrong_result_kind" must be a subkind of value
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
       But the layout of type "'a" must be a sublayout of value
         because the type argument of wrong_payload_kind has layout value.
|}]
