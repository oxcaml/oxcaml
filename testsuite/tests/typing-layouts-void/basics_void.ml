(* TEST
 expect;
*)

(* unit# can be ignored with [;] *)

external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|
external unbox_unit : unit -> unit# = "%unbox_unit"
|}]

let () =
  unbox_unit ();
  ()
[%%expect{|
|}]

type unit_u : void mod everything
[%%expect{|
type unit_u : void mod everything
|}]

(* Variants whose constructor arguments are all void are immediates *)

type v : immediate = A of unit_u [@immediate_all_void_constructor]
[%%expect{|
type v = A of unit_u [@immediate_all_void_constructor]
|}]

type v : immediate =
  | A of unit_u [@immediate_all_void_constructor]
  | B of #(unit_u * #(unit_u * unit_u)) [@immediate_all_void_constructor]
  | C
[%%expect{|
type v =
    A of unit_u [@immediate_all_void_constructor]
  | B of #(unit_u * #(unit_u * unit_u)) [@immediate_all_void_constructor]
  | C
|}]

type bad : immediate = A of unit_u [@immediate_all_void_constructor] | B of int
[%%expect{|
Line 1, characters 0-79:
1 | type bad : immediate = A of unit_u [@immediate_all_void_constructor] | B of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed variant type.
       But the layout of type "bad" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type bad.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* With-bounds for all-void variants *)

type key : void
type key_holder1 : immediate with key = A of key [@immediate_all_void_constructor]
type ('a : void) r = #{ a : 'a }
type key_holder2 : immediate with key =
  | A of #(unit_u * key r) [@immediate_all_void_constructor]
[%%expect{|
type key : void
type key_holder1 = A of key [@immediate_all_void_constructor]
type ('a : void) r = #{ a : 'a; }
type key_holder2 = A of #(unit_u * key r) [@immediate_all_void_constructor]
|}]

type bad : immediate = A of key [@immediate_all_void_constructor]
[%%expect{|
Line 1, characters 0-65:
1 | type bad : immediate = A of key [@immediate_all_void_constructor]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation immediate,
       because key is not mod global aliased many contended portable forkable
                 unyielding stateless immutable.
|}]
type bad : immediate = A of #(unit_u * key r) [@immediate_all_void_constructor]
[%%expect{|
Line 1, characters 0-79:
1 | type bad : immediate = A of #(unit_u * key r) [@immediate_all_void_constructor]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation immediate,
       because key is not mod global aliased many contended portable forkable
                 unyielding stateless immutable.
|}]


type void_mod_global : void mod global
type t : value mod global = A of void_mod_global [@immediate_all_void_constructor]
type t2 : immediate with void_mod_global =
  | A of void_mod_global [@immediate_all_void_constructor]
[%%expect{|
type void_mod_global : void mod global
type t = A of void_mod_global [@immediate_all_void_constructor]
type t2 = A of void_mod_global [@immediate_all_void_constructor]
|}]

type v1 : void
type v2 : void
type t : immediate with v1 with v2 =
  | A of v1 [@immediate_all_void_constructor]
  | B of #(unit_u * v2 r) [@immediate_all_void_constructor]
[%%expect{|
type v1 : void
type v2 : void
type t =
    A of v1 [@immediate_all_void_constructor]
  | B of #(unit_u * v2 r) [@immediate_all_void_constructor]
|}]

type bad : immediate with v1 =
  | A of v1 [@immediate_all_void_constructor]
  | B of #(unit_u * v2 r) [@immediate_all_void_constructor]
[%%expect{|
Lines 1-3, characters 0-59:
1 | type bad : immediate with v1 =
2 |   | A of v1 [@immediate_all_void_constructor]
3 |   | B of #(unit_u * v2 r) [@immediate_all_void_constructor]
Error: This type definition does not satisfy its kind annotation immediate with v1,
       because v2 is not mod global aliased many contended portable forkable
                 unyielding stateless immutable.
|}]

type vme : void
type t : value mod external_ = A of vme [@immediate_all_void_constructor]
[%%expect{|
type vme : void
type t = A of vme [@immediate_all_void_constructor]
|}]

(* All-void records are not allowed *)
type u1 = #{ a: unit_u }
type u2 = #{ a: unit_u; b: unit_u }
type u3 = { a : unit_u } [@@unboxed]
type u4 = #{ a: u2 }
type u5 = #{ a: u3 }
[%%expect{|
type u1 = #{ a : unit_u; }
type u2 = #{ a : unit_u; b : unit_u; }
type u3 = { a : unit_u; } [@@unboxed]
type u4 = #{ a : u2; }
type u5 = #{ a : u3; }
|}]

type bad = { a : unit_u }
[%%expect{|
Line 1, characters 0-25:
1 | type bad = { a : unit_u }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : #(unit_u * unit_u) }
[%%expect{|
Line 1, characters 0-37:
1 | type bad = { a : #(unit_u * unit_u) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u1 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u1 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u2 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u2 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u3 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u3 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u4 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u4 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u5 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u5 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

type bad = A of { a : unit_u }
[%%expect{|
Line 1, characters 11-30:
1 | type bad = A of { a : unit_u }
               ^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : #(unit_u * unit_u) }
[%%expect{|
Line 1, characters 11-42:
1 | type bad = A of { a : #(unit_u * unit_u) }
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : u1 }
[%%expect{|
Line 1, characters 11-26:
1 | type bad = A of { a : u1 }
               ^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : u2 }
[%%expect{|
Line 1, characters 11-26:
1 | type bad = A of { a : u2 }
               ^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : u3 }
[%%expect{|
Line 1, characters 11-26:
1 | type bad = A of { a : u3 }
               ^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : u4 }
[%%expect{|
Line 1, characters 11-26:
1 | type bad = A of { a : u4 }
               ^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = A of { a : u5 }
[%%expect{|
Line 1, characters 11-26:
1 | type bad = A of { a : u5 }
               ^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

(* [void] in arrays is not yet allowed *)

external length : ('a : any mod separable) . 'a array -> int = "%array_length"
[@@layout_poly]
external get : ('a : any mod separable). 'a array -> int -> 'a = "%array_safe_get"
[@@layout_poly]
[%%expect{|
external length : ('a : any separable). 'a array -> int = "%array_length"
  [@@layout_poly]
external get : ('a : any separable). 'a array -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
|}]

let f (a : unit_u array) = length a
[%%expect{|
Line 1, characters 34-35:
1 | let f (a : unit_u array) = length a
                                      ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : #(int * unit_u) array) = length a
[%%expect{|
Line 1, characters 43-44:
1 | let f (a : #(int * unit_u) array) = length a
                                               ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : unit_u array) i = get a i
[%%expect{|
Line 1, characters 33-34:
1 | let f (a : unit_u array) i = get a i
                                     ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : #(int * unit_u) array) i = get a i
[%%expect{|
Line 1, characters 42-43:
1 | let f (a : #(int * unit_u) array) i = get a i
                                              ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : u1 array) i = get a i
[%%expect{|
Line 1, characters 29-30:
1 | let f (a : u1 array) i = get a i
                                 ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : u2 array) i = get a i
[%%expect{|
Line 1, characters 29-30:
1 | let f (a : u2 array) i = get a i
                                 ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

let f (a : u3 array) i = get a i
[%%expect{|
Line 1, characters 29-30:
1 | let f (a : u3 array) i = get a i
                                 ^
Error: Types whose layout contains [void] are not yet supported in arrays.
|}]

(* [@immediate_all_void_constructor] is required on constructors whose arguments
   are all void, and optional elsewhere. *)

type t = A of unit_u [@immediate_all_void_constructor]
[%%expect{|
type t = A of unit_u [@immediate_all_void_constructor]
|}]

type t = A of unit_u [@immediate_all_void_constructor] | B of int | C
[%%expect{|
type t = A of unit_u [@immediate_all_void_constructor] | B of int | C
|}]

type t = A of unit_u * #(unit_u * unit_u) [@immediate_all_void_constructor]
[%%expect{|
type t = A of unit_u * #(unit_u * unit_u) [@immediate_all_void_constructor]
|}]

type t = A : unit_u -> t [@immediate_all_void_constructor]
[%%expect{|
type t = A : unit_u -> t [@immediate_all_void_constructor]
|}]

module type S = sig
  type t = A of unit_u [@immediate_all_void_constructor]
end
[%%expect{|
module type S =
  sig type t = A of unit_u [@immediate_all_void_constructor] end
|}]

(* Missing attribute *)

type t = A of unit_u
[%%expect{|
Line 1, characters 9-20:
1 | type t = A of unit_u
             ^^^^^^^^^^^
Error: All arguments of the constructor "A" are void, so it must be
       annotated with "[@immediate_all_void_constructor]".
|}]

type t = A of #(unit_u * unit_u) | B of int
[%%expect{|
Line 1, characters 9-32:
1 | type t = A of #(unit_u * unit_u) | B of int
             ^^^^^^^^^^^^^^^^^^^^^^^
Error: All arguments of the constructor "A" are void, so it must be
       annotated with "[@immediate_all_void_constructor]".
|}]

module type S = sig
  type t = A of unit_u
end
[%%expect{|
Line 2, characters 11-22:
2 |   type t = A of unit_u
               ^^^^^^^^^^^
Error: All arguments of the constructor "A" are void, so it must be
       annotated with "[@immediate_all_void_constructor]".
|}]

(* A misplaced attribute is a warning, not an error, so the type is still
   accepted. The warning (53) isn't reported here because expect tests run at
   toplevel; see [immediate_all_void_constructor_unused.ml]. *)

type t = A of int [@immediate_all_void_constructor]
[%%expect{|
type t = A of int
|}]

type t = A of unit_u [@@unboxed]
[%%expect{|
type t = A of unit_u [@@unboxed]
|}]
