(* TEST
 flags = "-w -181";
 expect;
*)

(* [@repr null] on a nullary constructor coexisting with an ordinary
   non-constant (boxed) constructor. *)
type tree = Empty [@repr null] | Node of int * tree * tree

[%%expect{|
type tree = Empty | Node of int * tree * tree
|}]

(* [@repr null] coexisting with ordinary constant constructors. *)
type t_consts = Null_const [@repr null] | A | B

[%%expect{|
type t_consts = Null_const | A | B
|}]

(* [@repr value] + [@repr null]: exactly the [@@or_null] shape.  The payload of
   the value constructor is stored unboxed. *)
type ('a : value) t_val : value_or_null = N [@repr null] | V of 'a [@repr value]

let to_option = function N -> None | V x -> Some x

[%%expect{|
type 'a t_val = N | V of 'a
val to_option : 'a t_val -> 'a option = <fun>
|}]

(* [@repr unboxed]: a single unary constructor, whole-variant unboxed. *)
type u = K of string [@repr unboxed]

[%%expect{|
type u = K of string [@@unboxed]
|}]

(* Rejection: [@repr immediate] is not yet supported. *)
type b_imm = Small of int [@repr immediate]

[%%expect{|
Line 1, characters 13-43:
1 | type b_imm = Small of int [@repr immediate]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@repr immediate]" is not yet supported;
       only "[@repr null]", "[@repr value]" and "[@repr unboxed]" are currently implemented.
|}]

(* Rejection: [@repr pointer] is not yet supported. *)
type b_ptr = P of int [@repr pointer]

[%%expect{|
Line 1, characters 13-37:
1 | type b_ptr = P of int [@repr pointer]
                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@repr pointer]" is not yet supported;
       only "[@repr null]", "[@repr value]" and "[@repr unboxed]" are currently implemented.
|}]

(* Rejection: [@repr value] may only coexist with a single [@repr null]. *)
type 'a bad_val = N [@repr null] | V of 'a [@repr value] | X

[%%expect{|
Line 1, characters 0-60:
1 | type 'a bad_val = N [@repr null] | V of 'a [@repr value] | X
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr value] may only coexist with a single [@repr null] constructor.
|}]

(* Rejection: [@repr unboxed] requires a single unary constructor. *)
type bad_unboxed = A of int [@repr unboxed] | B of int

[%%expect{|
Line 1, characters 0-54:
1 | type bad_unboxed = A of int [@repr unboxed] | B of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr unboxed] requires a single unary constructor.
|}]

(* Rejection: [@repr null] requires a nullary constructor. *)
type bad_null = Bad of int [@repr null] | Other

[%%expect{|
Line 1, characters 0-47:
1 | type bad_null = Bad of int [@repr null] | Other
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr null] requires a nullary constructor.
|}]

(* Module inclusion: the representation is part of the signature, so a
   [@repr null] implementation does not match a plain-variant signature. *)
module M : sig
  type t = Empty | Node of int
end = struct
  type t = Empty [@repr null] | Node of int
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Empty [@repr null] | Node of int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Empty | Node of int end
       is not included in
         sig type t = Empty | Node of int end
       Type declarations do not match:
         type t = Empty | Node of int
       is not included in
         type t = Empty | Node of int
       Their internal representations differ:
       the first declaration has a constructor represented as a null pointer.
       Hint: add [@@or_null] or [@@or_null_reexport].
|}]
