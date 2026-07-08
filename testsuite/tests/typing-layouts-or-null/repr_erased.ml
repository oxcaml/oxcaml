(* TEST
 flags = "-w -181";
 expect;
*)

(* Tranche-2 constructor-level erased representations: [@repr immediate] and
   [@repr pointer], and their sound coexistence with [@repr null] and ordinary
   boxed constructors. *)

(* --- [@repr immediate] acceptances --- *)

(* An immediate payload alongside an ordinary boxed constructor. *)
type bignum = Small of int [@repr immediate] | Big of string

[%%expect{|
type bignum = Small of int | Big of string
|}]

(* [@repr immediate] alone. *)
type only_imm = I of int [@repr immediate]

[%%expect{|
type only_imm = I of int
|}]

(* [@repr immediate] coexisting with [@repr null]. *)
type imm_or_null = N [@repr null] | I of int [@repr immediate]

[%%expect{|
type imm_or_null = N | I of int
|}]

(* --- [@repr pointer] acceptances --- *)

(* immediate + pointer: the classic tagged-or-boxed encoding. *)
type int_or_err = I of int [@repr immediate] | E of string [@repr pointer]

[%%expect{|
type int_or_err = I of int | E of string
|}]

(* null + pointer with a boxed-record payload. *)
type box = { a : int; b : string }
type boxed_or_null = Null_ptr [@repr null] | Ptr of box [@repr pointer]

[%%expect{|
type box = { a : int; b : string; }
type boxed_or_null = Null_ptr | Ptr of box
|}]

(* null + immediate + pointer, all three erased kinds at once. *)
type three = N [@repr null] | I of int [@repr immediate] | P of string [@repr pointer]

[%%expect{|
type three = N | I of int | P of string
|}]

(* null + immediate + ordinary boxed (the tree shape). *)
type tree = Empty [@repr null] | Leaf of int [@repr immediate] | Branch of tree * tree

[%%expect{|
type tree = Empty | Leaf of int | Branch of tree * tree
|}]

(* --- [@repr immediate] rejections --- *)

(* Immediate collides with ordinary constant constructors. *)
type imm_const = K | I of int [@repr immediate]

[%%expect{|
Line 1, characters 0-47:
1 | type imm_const = K | I of int [@repr immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr immediate] must not coexist with ordinary constant constructors: both occupy the immediate space.
|}]

(* At most one immediate. *)
type two_imm = A of int [@repr immediate] | B of int [@repr immediate]

[%%expect{|
Line 1, characters 0-70:
1 | type two_imm = A of int [@repr immediate] | B of int [@repr immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       there may be at most one [@repr immediate] constructor.
|}]

(* The payload must be an immediate. *)
type imm_str = I of string [@repr immediate]

[%%expect{|
Line 1, characters 15-44:
1 | type imm_str = I of string [@repr immediate]
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr immediate] requires an immediate, non-null payload.
|}]

(* --- [@repr pointer] rejections --- *)

(* Pointer collides with an ordinary boxed (non-constant) constructor. *)
type ptr_boxed = P of string [@repr pointer] | B of int list

[%%expect{|
Line 1, characters 0-60:
1 | type ptr_boxed = P of string [@repr pointer] | B of int list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] may only coexist with [@repr null] and [@repr immediate] constructors: a raw pointer occupies the whole non-immediate space and would collide with ordinary constructors.
|}]

(* Pointer collides with an ordinary constant constructor. *)
type ptr_const = P of string [@repr pointer] | K

[%%expect{|
Line 1, characters 0-48:
1 | type ptr_const = P of string [@repr pointer] | K
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] may only coexist with [@repr null] and [@repr immediate] constructors: a raw pointer occupies the whole non-immediate space and would collide with ordinary constructors.
|}]

(* At most one pointer. *)
type two_ptr = A of string [@repr pointer] | B of bytes [@repr pointer]

[%%expect{|
Line 1, characters 0-71:
1 | type two_ptr = A of string [@repr pointer] | B of bytes [@repr pointer]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       there may be at most one [@repr pointer] constructor.
|}]

(* The payload must be a pointer, not an immediate. *)
type ptr_imm = P of int [@repr pointer]

[%%expect{|
Line 1, characters 15-39:
1 | type ptr_imm = P of int [@repr pointer]
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] requires a payload that is definitely a non-null pointer.
|}]

(* The payload must be DEFINITELY a pointer: an unconstrained type variable
   could be instantiated with an immediate. *)
type 'a ptr_tvar = P of 'a [@repr pointer]

[%%expect{|
Line 1, characters 19-42:
1 | type 'a ptr_tvar = P of 'a [@repr pointer]
                       ^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] requires a payload that is definitely a non-null pointer.
|}]

(* Float guard: a boxed float shares the flat-float-array representation. *)
type ptr_float = P of float [@repr pointer]

[%%expect{|
Line 1, characters 17-43:
1 | type ptr_float = P of float [@repr pointer]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] requires a payload that is definitely a non-null pointer.
|}]

(* Abstract payloads are rejected (cannot confirm pointer-ness). *)
module M : sig type t_abstract end = struct type t_abstract = int end
type ptr_abstract = P of M.t_abstract [@repr pointer]

[%%expect{|
module M : sig type t_abstract end
Line 2, characters 20-53:
2 | type ptr_abstract = P of M.t_abstract [@repr pointer]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] requires a payload that is definitely a non-null pointer.
|}]
