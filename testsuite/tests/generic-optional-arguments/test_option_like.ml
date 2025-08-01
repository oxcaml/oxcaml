(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Valid option-like type *)
type 'a t =
  | None
  | Some of 'a
[@@option_like]
[%%expect{|
type 'a t = None | Some of 'a
|}]
(* CR generic-optional: Print out [@@option_like] *)

(* Invalid: abstract type *)
type 'a abstract [@@option_like]
[%%expect{|
Line 1, characters 0-32:
1 | type 'a abstract [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because it is abstract.
|}]

(* Invalid: more than two constructors *)
type 'a many =
  | A
  | B of 'a
  | C
[@@option_like]
[%%expect{|
Lines 1-5, characters 0-15:
1 | type 'a many =
2 |   | A
3 |   | B of 'a
4 |   | C
5 | [@@option_like]
Error: This type cannot be marked as option-like because
       it has more than two constructors.
|}]

(* Invalid: two constructors with no arguments *)
type both_empty =
  | A
  | B
[@@option_like]
[%%expect{|
Lines 1-4, characters 0-15:
1 | type both_empty =
2 |   | A
3 |   | B
4 | [@@option_like]
Error: This type cannot be marked as option-like because
       it should have exactly one nullary constructor and one unary constructor.
|}]

(* Invalid: constructor with multiple arguments *)
type 'a multi_arg =
  | Empty
  | Many of 'a * 'a
[@@option_like]
[%%expect{|
Lines 1-4, characters 0-15:
1 | type 'a multi_arg =
2 |   | Empty
3 |   | Many of 'a * 'a
4 | [@@option_like]
Error: This type cannot be marked as option-like because
       the constructor with arguments has more than one argument.
|}]

(* Invalid: constructor with multiple arguments, with two type variables *)
type ('a, 'b) multi_arg =
  | Full of 'a * 'b
  | Empty
[@@option_like]
[%%expect {|
Lines 1-4, characters 0-15:
1 | type ('a, 'b) multi_arg =
2 |   | Full of 'a * 'b
3 |   | Empty
4 | [@@option_like]
Error: This type cannot be marked as option-like because
       the constructor with arguments has more than one argument.
|}]

type 'a unknown_arg =
  | Full of 'b
  | Empty
[@@option_like]
[%%expect {|
Line 2, characters 12-14:
2 |   | Full of 'b
                ^^
Error: The type variable "'b" is unbound in this type declaration.
|}]

(* Invalid: record type *)
type 'a record = { x : 'a } [@@option_like]
[%%expect{|
Line 1, characters 0-43:
1 | type 'a record = { x : 'a } [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because it is a record type.
|}]

(* Invalid: record type *)
type 'a unboxed_record = #{ x : 'a } [@@option_like]
[%%expect{|
Line 1, characters 0-52:
1 | type 'a unboxed_record = #{ x : 'a } [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because
       it is an unboxed record type.
|}]

(* Invalid: inline record *)
type 'a inline_rec =
  | Empty
  | Rec of { x : 'a }
[@@option_like]
[%%expect{|
Lines 1-4, characters 0-15:
1 | type 'a inline_rec =
2 |   | Empty
3 |   | Rec of { x : 'a }
4 | [@@option_like]
Error: This type cannot be marked as option-like because
       inline records are not supported.
|}]

(* Valid: swapped order *)
type 'a swapped =
  | Full of 'a
  | Empty
[@@option_like]
[%%expect{|
type 'a swapped = Full of 'a | Empty
|}]

(* CR generic-optional: Allow this *)
type 'a defn_option = 'a option [@@option_like]
[%%expect{|
Line 1, characters 0-47:
1 | type 'a defn_option = 'a option [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because it is abstract.
|}]

type 'a t = 'a option =
  | None
  | Some of 'a
[@@option_like]
[%%expect{|
type 'a t = 'a option = None | Some of 'a
|}]

(* Invalid: record *)
type ('a, 'b) record_args =
  | Full of {a : 'a; b : 'b}
  | Empty
[@@option_like]
[%%expect {|
Lines 1-4, characters 0-15:
1 | type ('a, 'b) record_args =
2 |   | Full of {a : 'a; b : 'b}
3 |   | Empty
4 | [@@option_like]
Error: This type cannot be marked as option-like because
       inline records are not supported.
|}]

(* Invalid: complex args *)
type 'a complex_args =
  | Full of 'a list
  | Empty
[@@option_like]
[%%expect {|
Line 2, characters 12-19:
2 |   | Full of 'a list
                ^^^^^^^
Error: This type cannot be marked as option-like because
       the constructor argument must be a type variable (e.g. 'a).
|}]

(* Invalid: constant args *)
type constant =
  | Full of int
  | Empty
[@@option_like]
[%%expect {|
Line 2, characters 12-15:
2 |   | Full of int
                ^^^
Error: This type cannot be marked as option-like because
       the constructor argument must be a type variable (e.g. 'a).
|}]

(* Valid: signature ascription*)
module M : sig
  type 'a t = Full of 'a | Empty [@@option_like]
end = struct
  type 'a t = Full of 'a | Empty [@@option_like]
end
[%%expect {|
module M : sig type 'a t = Full of 'a | Empty end @@ stateless
|}]

(* Invalid: need concrete definitions to apply [@@option_like] *)
module M : sig
  type 'a t [@@option_like]
end = struct
  type 'a t = Full of 'a | Empty [@@option_like]
end
[%%expect {|
Line 2, characters 2-27:
2 |   type 'a t [@@option_like]
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because it is abstract.
|}]

(* Okay:  [@@option_like] in signature but not in structure *)
module M : sig
  type 'a t = Full of 'a | Empty [@@option_like]
end = struct
  type 'a t = Full of 'a | Empty
end
[%%expect {|
module M : sig type 'a t = Full of 'a | Empty end @@ stateless
|}]

(* Okay:  [@@option_like] in structure but not in signature *)
module M : sig
  type 'a t = Full of 'a | Empty
end = struct
  type 'a t = Full of 'a | Empty [@@option_like]
end
[%%expect {|
module M : sig type 'a t = Full of 'a | Empty end @@ stateless
|}]

(* Valid: gadt *)
type 'a gadt =
  | None
  | Some : 'a -> 'a gadt
[@@option_like]
[%%expect {|
type 'a gadt = None | Some : 'a -> 'a gadt
|}]

(* Valid: gadt no type parameter *)
type _ gadt =
  | None
  | Some : 'a -> 'a gadt
[@@option_like]
[%%expect {|
type _ gadt = None | Some : 'a -> 'a gadt
|}]

(* Valid: phantom type parameters *)
type ('a, 'b) phantom =
  | None
  | Some of 'a
[@@option_like]
[%%expect {|
type ('a, 'b) phantom = None | Some of 'a
|}]

(* Invalid: extensible variants *)
type 'a ext = ..  [@@option_like]
[%%expect {|
Line 1, characters 0-33:
1 | type 'a ext = ..  [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because
       extensible variant types cannot be option-like.
|}]

type 'a option = float [@@option_like]

[%%expect{|
Line 1, characters 0-38:
1 | type 'a option = float [@@option_like]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be marked as option-like because it is abstract.
|}]


(* Some concrete type parameter *)
type (_, _) gadt =
  | None
  | Some : 'a -> (int, 'a) gadt
[@@option_like]
[%%expect {|
type (_, _) gadt = None | Some : 'a -> (int, 'a) gadt
|}]
