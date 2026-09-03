(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(***************************)
(* Immediate layout errors *)

(* All-void boxed records are blocks, not immediates. *)
type bad : immediate = { x : unit# }
[%%expect{|
Line 1, characters 0-36:
1 | type bad : immediate = { x : unit# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed record type.
       But the layout of type "bad" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type bad.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* All-void inline records are blocks, not immediates. *)

type t : immediate = A of { x : unit# }
[%%expect{|
Line 1, characters 0-39:
1 | type t : immediate = A of { x : unit# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is value non_float
         because it's a boxed variant type.
       But the layout of type "t" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type t.
       Note: The layout of immediate is value non_pointer.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* Enumeration *)
type ('a: void) t = 'a
type v = unit
let f (x: v): 'a t = x
[%%expect{|
type ('a : void) t = 'a
type v = unit
Line 3, characters 21-22:
3 | let f (x: v): 'a t = x
                         ^
Error: The value "x" has type "v" = "unit" but an expression was expected of type
         "'a t" = "('a : void)"
       The layout of unit is value non_pointer
         because it is the primitive type unit.
       But the layout of unit must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
       Note: The layout of immediate is value non_pointer.
|}]

(* Primitive *)
type ('a: void) t = 'a
let f (x: int): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 23-24:
2 | let f (x: int): 'a t = x
                           ^
Error: The value "x" has type "int" but an expression was expected of type
         "'a t" = "('a : void)"
       The layout of int is value non_pointer
         because it is the primitive type int.
       But the layout of int must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
       Note: The layout of immediate is value non_pointer.
|}];;

(* Immediate_polymorphic_variant *)
type ('a: void) t = 'a
let f (x: [`A | `B]): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 29-30:
2 | let f (x: [`A | `B]): 'a t = x
                                 ^
Error: The value "x" has type "[ `A | `B ]"
       but an expression was expected of type "'a t" = "('a : void)"
       The layout of [ `A | `B ] is value non_pointer
         because it's an enumeration variant type (all constructors are constant).
       But the layout of [ `A | `B ] must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
       Note: The layout of immediate is value non_pointer.
|}]
