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

(* --- inline-record payload rejection --- *)

(* [@repr immediate] requires a unary tuple constructor, not an inline record. *)
type imm_inline = N [@repr null] | A of { x : int } [@repr immediate]

[%%expect{|
Line 1, characters 0-69:
1 | type imm_inline = N [@repr null] | A of { x : int } [@repr immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr immediate] requires a unary tuple constructor.
|}]

(* [@repr pointer] requires a unary tuple constructor, not an inline record. *)
type ptr_inline = N [@repr null] | A of { x : int } [@repr pointer]

[%%expect{|
Line 1, characters 0-67:
1 | type ptr_inline = N [@repr null] | A of { x : int } [@repr pointer]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [@repr] declaration:
       [@repr pointer] requires a unary tuple constructor.
|}]

(* --- [@repr] on extensible variants / exceptions is accepted and silently
   ignored: these never receive an erased representation, so there is no
   soundness concern.  (Batch compilation additionally emits Warning 53
   [misplaced-attribute]; the toplevel driving this test does not.) *)

exception Exn_repr of int [@repr immediate]

[%%expect{|
exception Exn_repr of int
|}]

type ext = ..
type ext += Ext_repr of int [@repr immediate]

[%%expect{|
type ext = ..
type ext += Ext_repr of int
|}]

(* --- GADT constructors may carry [@repr] --- *)
type _ gadt =
  | Code : int -> int gadt [@repr immediate]
  | Text : string -> string gadt [@repr pointer]

[%%expect{|
type _ gadt = Code : int -> int gadt | Text : string -> string gadt
|}]

(* --- module inclusion: [@repr] is part of the representation, so signature
   and implementation must agree on it --- *)

(* Exploit (soundness): a [@repr immediate] signature constructor is
   payload-unboxed, but a plain implementation constructor is a boxed block.
   Without comparing the erased representations this was accepted and a client
   read the block pointer as a tagged int. *)
module M_imm_boxed : sig
  type t = N [@repr null] | A of int [@repr immediate]
end = struct
  type t = N [@repr null] | A of int
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = N [@repr null] | A of int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = N | A of int end
       is not included in
         sig type t = N | A of int end
       Type declarations do not match:
         type t = N | A of int
       is not included in
         type t = N | A of int
       Their internal representations differ:
       constructor A uses the ordinary boxed representation in the first declaration but [@repr immediate] in the second declaration.
|}]

(* The other direction (signature boxed, implementation [@repr immediate]) is
   equally rejected. *)
module M_boxed_imm : sig
  type t = N [@repr null] | A of int
end = struct
  type t = N [@repr null] | A of int [@repr immediate]
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = N [@repr null] | A of int [@repr immediate]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = N | A of int end
       is not included in
         sig type t = N | A of int end
       Type declarations do not match:
         type t = N | A of int
       is not included in
         type t = N | A of int
       Their internal representations differ:
       constructor A uses [@repr immediate] in the first declaration but the ordinary boxed representation in the second declaration.
|}]

(* [@repr pointer] versus a plain boxed implementation is also rejected. *)
module M_ptr_boxed : sig
  type t = N [@repr null] | A of string [@repr pointer]
end = struct
  type t = N [@repr null] | A of string
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = N [@repr null] | A of string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = N | A of string end
       is not included in
         sig type t = N | A of string end
       Type declarations do not match:
         type t = N | A of string
       is not included in
         type t = N | A of string
       Their internal representations differ:
       constructor A uses the ordinary boxed representation in the first declaration but [@repr pointer] in the second declaration.
|}]

(* Positive: identical [@repr immediate] on both sides is accepted. *)
module M_imm_ok : sig
  type t = N [@repr null] | A of int [@repr immediate]
end = struct
  type t = N [@repr null] | A of int [@repr immediate]
end

[%%expect{|
module M_imm_ok : sig type t = N | A of int end
|}]

(* Positive: identical [@repr pointer] on both sides is accepted. *)
module M_ptr_ok : sig
  type t = N [@repr null] | A of string [@repr pointer]
end = struct
  type t = N [@repr null] | A of string [@repr pointer]
end

[%%expect{|
module M_ptr_ok : sig type t = N | A of string end
|}]
