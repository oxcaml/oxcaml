(* TEST
 expect;
*)

(* All-void constructors without [@immediate_all_void_constructor] are
   represented as blocks. *)

type t = A of unit#
[%%expect{|
type t = A of unit#
|}]

type t = A of #(unit# * unit#) | B of int
[%%expect{|
type t = A of #(unit# * unit#) | B of int
|}]

type t : immutable_data with unit# = A of unit#
[%%expect{|
type t = A of unit#
|}]

(* Without the attribute, the constructor is a block, so the variant is not
   immediate. *)

type t : immediate = A of unit#
[%%expect{|
Line 1, characters 0-31:
1 | type t : immediate = A of unit#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is value non_float
         because it's a boxed variant type.
       But the layout of type "t" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type t.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* Inclusion must not conflate immediate and block all-void constructors. *)

module M : sig
  type t = A of unit#
end = struct
  type t = A of unit# [@immediate_all_void_constructor]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of unit# [@immediate_all_void_constructor]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of unit# [@immediate_all_void_constructor] end
       is not included in
         sig type t = A of unit# end
       Type declarations do not match:
         type t = A of unit# [@immediate_all_void_constructor]
       is not included in
         type t = A of unit#
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       The first is annotated with "[@immediate_all_void_constructor]" and the second isn't.
|}]

module M : sig
  type t = A of unit# [@immediate_all_void_constructor]
end = struct
  type t = A of unit#
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of unit#
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of unit# end
       is not included in
         sig type t = A of unit# [@immediate_all_void_constructor] end
       Type declarations do not match:
         type t = A of unit#
       is not included in
         type t = A of unit# [@immediate_all_void_constructor]
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       The second is annotated with "[@immediate_all_void_constructor]" and the first isn't.
|}]

type u = A of unit# [@immediate_all_void_constructor]
type t = u = A of unit#
[%%expect{|
type u = A of unit# [@immediate_all_void_constructor]
Line 2, characters 0-23:
2 | type t = u = A of unit#
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "u"
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       The original is annotated with "[@immediate_all_void_constructor]" and this isn't.
|}]

type u = A of unit#
type t = u = A of unit# [@immediate_all_void_constructor]
[%%expect{|
type u = A of unit#
Line 2, characters 0-57:
2 | type t = u = A of unit# [@immediate_all_void_constructor]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "u"
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       This is annotated with "[@immediate_all_void_constructor]" and the original isn't.
|}]

(* Agreement on the attribute is fine. *)

module M : sig
  type t = A of unit# [@immediate_all_void_constructor]
end = struct
  type t = A of unit# [@immediate_all_void_constructor]
end
[%%expect{|
module M : sig type t = A of unit# [@immediate_all_void_constructor] end
|}]

module M : sig
  type t = A of unit#
end = struct
  type t = A of unit#
end
[%%expect{|
module M : sig type t = A of unit# end
|}]

type u = A of unit# [@immediate_all_void_constructor]
type t = u = A of unit# [@immediate_all_void_constructor]
[%%expect{|
type u = A of unit# [@immediate_all_void_constructor]
type t = u = A of unit# [@immediate_all_void_constructor]
|}]

(* Constructor shapes can mismatch before any other inclusion error when the
   layout-mismatched type comes later in a recursive group. *)

module M : sig
  type t = A of v
  and v : value
end = struct
  type t = A of v
  and v = unit#
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = A of v
6 |   and v = unit#
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of v and v = unit# end
       is not included in
         sig type t = A of v and v end
       Type declarations do not match:
         type t = A of v
       is not included in
         type t = A of v
       Constructors do not match:
         "A of v"
       is not the same as:
         "A of v"
       Their internal representations differ:
       This is likely caused by a layout mismatch in a later definition.
|}]

(* Arguments of all-void constructors are Guard in the recursive value check,
   with or without the attribute. *)

type imm = A of unit# [@immediate_all_void_constructor]
let rec x = A (let _ = ref x in #())
[%%expect{|
type imm = A of unit# [@immediate_all_void_constructor]
val x : imm = A <void>
|}]

type blk = A of unit#
let rec x = A (let _ = ref x in #())
[%%expect{|
type blk = A of unit#
val x : blk = A <void>
|}]

(* Mode-crossing *)

type imm : immediate = A of unit# [@immediate_all_void_constructor]
let crosses_local (x : imm @ local) = (x : imm @ global)
[%%expect{|
type imm = A of unit# [@immediate_all_void_constructor]
val crosses_local : imm @ local -> imm = <fun>
|}]

(* We could likely make this cross more if we choose to guarantee it's either
   immediate or statically-allocated. *)
type blk = A of unit#
let crosses_local (x : blk @ local) = (x : blk @ global)
[%%expect{|
type blk = A of unit#
Line 2, characters 39-40:
2 | let crosses_local (x : blk @ local) = (x : blk @ global)
                                           ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

type blk : immutable_data = A of unit#
[%%expect{|
type blk = A of unit#
|}]
