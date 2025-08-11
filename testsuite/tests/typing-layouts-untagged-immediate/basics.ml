(* TEST
 {
   flags = "-extension layouts_beta";
   expect;
 }
*)

(* This file contains typing tests for the layout [untagged_immediate].

   Runtime tests for the type [int#] can be found in the
   [unboxed_int], [alloc], and [test_int_u] tests in this
   directory.  The type [int#] here is used as a convenient example of a
   concrete [untagged_immediate] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

type t_untagged_immediate : untagged_immediate
type ('a : untagged_immediate) t_untagged_immediate_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_untagged_immediate) = x;;
let f1_2 (x : 'a t_untagged_immediate_id) = x;;
let f1_3 (x : int#) = x;;
[%%expect{|
type t_untagged_immediate : untagged_immediate mod non_float
type ('a : untagged_immediate mod non_float) t_untagged_immediate_id = 'a
Line 7, characters 38-39:
7 | let f1_1 (x : t_untagged_immediate) = x;;
                                          ^
Error: Non-value layout untagged_immediate detected as sort for type
       t_untagged_immediate,
       but this requires extension layouts, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_untagged_immediate) =
  let y = x in
  y;;

let f2_2 (x : 'a t_untagged_immediate_id) =
  let y = x in
  y;;

let f2_3 (x : int#) =
  let y = x in
  y;;
[%%expect{|
Lines 2-3, characters 2-3:
2 | ..let y = x in
3 |   y..
Error: Non-value layout untagged_immediate detected as sort for type
       t_untagged_immediate,
       but this requires extension layouts, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_untagged_immediate = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_untagged_immediate = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_1" has layout "untagged_immediate".
|}];;

let x3_2 : 'a t_untagged_immediate_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_untagged_immediate_id = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_2" has layout "untagged_immediate".
|}];;

let x3_3 : int# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : int# = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_3" has layout "untagged_immediate".
|}];;

module M3_4 = struct
  let x : t_untagged_immediate = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_untagged_immediate = assert false
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "untagged_immediate".
|}];;

module M3_5 = struct
  let f (x : int#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "y" has layout "untagged_immediate".
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_untagged_immediate) = x, false;;
[%%expect{|
Line 1, characters 38-39:
1 | let f4_1 (x : t_untagged_immediate) = x, false;;
                                          ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value
         because it's the type of a tuple element.
|}];;

let f4_2 (x : 'a t_untagged_immediate_id) = x, false;;
[%%expect{|
Line 1, characters 44-45:
1 | let f4_2 (x : 'a t_untagged_immediate_id) = x, false;;
                                                ^
Error: This expression has type
         "'a t_untagged_immediate_id" = "('a : untagged_immediate mod non_float)"
       but an expression was expected of type "('b : value_or_null)"
       The layout of 'a t_untagged_immediate_id is untagged_immediate
         because of the definition of t_untagged_immediate_id at line 2, characters 0-59.
       But the layout of 'a t_untagged_immediate_id must be a sublayout of
           value
         because it's the type of a tuple element.
|}];;

let f4_3 (x : int#) = x, false;;
[%%expect{|
Line 1, characters 22-23:
1 | let f4_3 (x : int#) = x, false;;
                          ^
Error: This expression has type "int#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_4 = t_untagged_immediate * string;;
[%%expect{|
Line 1, characters 12-32:
1 | type t4_4 = t_untagged_immediate * string;;
                ^^^^^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "t_untagged_immediate" is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of "t_untagged_immediate" must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_5 = int * int#;;
[%%expect{|
Line 1, characters 18-22:
1 | type t4_5 = int * int#;;
                      ^^^^
Error: Tuple element types must have layout value.
       The layout of "int#" is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of "int#" must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type ('a : untagged_immediate) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 38-40:
1 | type ('a : untagged_immediate) t4_6 = 'a * 'a
                                          ^^
Error: Tuple element types must have layout value.
       The layout of "'a" is untagged_immediate
         because of the annotation on 'a in the declaration of the type t4_6.
       But the layout of "'a" must overlap with value
         because it's the type of a tuple element.
|}];;

(* check for layout propagation *)
type ('a : untagged_immediate, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 57-59:
1 | type ('a : untagged_immediate, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                                             ^^
Error: Tuple element types must have layout value.
       The layout of "'a" is untagged_immediate
         because of the annotation on 'a in the declaration of the type t4_7.
       But the layout of "'a" must overlap with value
         because it's the type of a tuple element.
|}]

(****************************************************)
(* Test 5: Allowed in some structures in typedecls. *)

type t5_1 = { x : t_untagged_immediate };;
[%%expect{|
type t5_1 = { x : t_untagged_immediate; }
|}];;

type t5_2 = { y : int; x : t_untagged_immediate };;
[%%expect{|
type t5_2 = { y : int; x : t_untagged_immediate; }
|}];;

type t5_2' = { y : string; x : t_untagged_immediate };;
[%%expect{|
type t5_2' = { y : string; x : t_untagged_immediate; }
|}];;

type t5_3 = { x : t_untagged_immediate } [@@unboxed];;
[%%expect{|
type t5_3 = { x : t_untagged_immediate; } [@@unboxed]
|}];;

type t5_4 = A of t_untagged_immediate;;
[%%expect{|
type t5_4 = A of t_untagged_immediate
|}];;

type t5_5 = A of int * t_untagged_immediate;;
[%%expect{|
type t5_5 = A of int * t_untagged_immediate
|}];;

type ('a : untagged_immediate) t5_7 = A of int
type ('a : untagged_immediate) t5_8 = A of 'a;;
[%%expect{|
type ('a : untagged_immediate mod non_float) t5_7 = A of int
type ('a : untagged_immediate mod non_float) t5_8 = A of 'a
|}]

(* No mixed block restriction: the compiler reorders the block for you, moving
   the unboxed type to the flat suffix. *)
type 'a t_reordered = A of t_untagged_immediate * 'a

[%%expect{|
type 'a t_reordered = A of t_untagged_immediate * 'a
|}]

type t5_6 = A of t_untagged_immediate [@@unboxed];;
[%%expect{|
type t5_6 = A of t_untagged_immediate [@@unboxed]
|}];;

type t5_6_1 = A of { x : t_untagged_immediate } [@@unboxed];;
[%%expect{|
type t5_6_1 = A of { x : t_untagged_immediate; } [@@unboxed]
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_untagged_immediate end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-51:
1 | module type S6_1 = sig val x : t_untagged_immediate end
                                   ^^^^^^^^^^^^^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of type t_untagged_immediate must be a sublayout of
           value
         because it's the type of something stored in a module structure.
|}];;

module type S6_2 = sig val x : 'a t_untagged_immediate_id end
[%%expect{|
Line 1, characters 31-57:
1 | module type S6_2 = sig val x : 'a t_untagged_immediate_id end
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type 'a t_untagged_immediate_id is untagged_immediate
         because of the definition of t_untagged_immediate_id at line 2, characters 0-59.
       But the layout of type 'a t_untagged_immediate_id must be a sublayout of
         value
         because it's the type of something stored in a module structure.
|}];;

module type S6_3 = sig val x : int# end
[%%expect{|
Line 1, characters 31-35:
1 | module type S6_3 = sig val x : int# end
                                   ^^^^
Error: This type signature for "x" is not a value type.
       The layout of type int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of type int# must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_untagged_immediate) = `A x;;
[%%expect{|
Line 1, characters 41-42:
1 | let f7_1 (x : t_untagged_immediate) = `A x;;
                                             ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_2 (x : 'a t_untagged_immediate_id) = `A x;;
[%%expect{|
Line 1, characters 47-48:
1 | let f7_2 (x : 'a t_untagged_immediate_id) = `A x;;
                                                   ^
Error: This expression has type
         "'a t_untagged_immediate_id" = "('a : untagged_immediate mod non_float)"
       but an expression was expected of type "('b : value_or_null)"
       The layout of 'a t_untagged_immediate_id is untagged_immediate
         because of the definition of t_untagged_immediate_id at line 2, characters 0-59.
       But the layout of 'a t_untagged_immediate_id must be a sublayout of
           value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_3 (x : int#) = `A x;;
[%%expect{|
Line 1, characters 25-26:
1 | let f7_3 (x : int#) = `A x;;
                             ^
Error: This expression has type "int#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type f7_4 = [ `A of t_untagged_immediate ];;
[%%expect{|
Line 1, characters 20-40:
1 | type f7_4 = [ `A of t_untagged_immediate ];;
                        ^^^^^^^^^^^^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "t_untagged_immediate" is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of "t_untagged_immediate" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type ('a : untagged_immediate) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 46-48:
1 | type ('a : untagged_immediate) f7_5 = [ `A of 'a ];;
                                                  ^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "'a" is untagged_immediate
         because of the annotation on 'a in the declaration of the type f7_5.
       But the layout of "'a" must overlap with value
         because it's the type of the field of a polymorphic variant.
|}];;

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_untagged_immediate () : t_untagged_immediate = assert false
let make_t_untagged_immediate_id () : 'a t_untagged_immediate_id = assert false
let make_intu () : int# = assert false

let id_value x = x;;
[%%expect{|
Line 1, characters 58-70:
1 | let make_t_untagged_immediate () : t_untagged_immediate = assert false
                                                              ^^^^^^^^^^^^
Error: Non-value layout untagged_immediate detected as sort for type
       t_untagged_immediate,
       but this requires extension layouts, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}];;

let x8_1 = id_value (make_t_untagged_immediate ());;
[%%expect{|
Line 1, characters 11-19:
1 | let x8_1 = id_value (make_t_untagged_immediate ());;
               ^^^^^^^^
Error: Unbound value "id_value"
|}];;

let x8_2 = id_value (make_t_untagged_immediate_id ());;
[%%expect{|
Line 1, characters 11-19:
1 | let x8_2 = id_value (make_t_untagged_immediate_id ());;
               ^^^^^^^^
Error: Unbound value "id_value"
|}];;

let x8_3 = id_value (make_intu ());;
[%%expect{|
Line 1, characters 11-19:
1 | let x8_3 = id_value (make_intu ());;
               ^^^^^^^^
Error: Unbound value "id_value"
|}];;

(*************************************)
(* Test 9: But untagged_immediate functions do. *)

let twice f (x : 'a t_untagged_immediate_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_untagged_immediate ())
let f9_2 () = twice f1_2 (make_t_untagged_immediate_id ())
let f9_3 () = twice f1_3 (make_intu ());;
[%%expect{|
Line 1, characters 47-54:
1 | let twice f (x : 'a t_untagged_immediate_id) = f (f x)
                                                   ^^^^^^^
Error: Non-value layout untagged_immediate detected as sort for type
       'a t_untagged_immediate_id,
       but this requires extension layouts, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}];;

(**************************************************)
(* Test 10: Invalid uses of untagged_immediate and externals *)

(* Valid uses of untagged_immediate in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - [@unboxed] is allowed on unboxed types but has no effect. Same is not
     true for [@untagged].
*)

external f10_1 : int -> bool -> int# = "foo";;
[%%expect{|
Line 1, characters 0-44:
1 | external f10_1 : int -> bool -> int# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_untagged_immediate -> int = "foo";;
[%%expect{|
Line 1, characters 0-52:
1 | external f10_2 : t_untagged_immediate -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_6 : (int#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f10_6 : int# -> bool -> string = "foo" "bar"
|}];;

external f10_7 : string -> (int#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f10_7 : string -> int# = "foo" "bar"
|}];;

external f10_8 : int -> int#  = "foo" "bar" [@@unboxed];;
[%%expect{|
Line 1, characters 17-20:
1 | external f10_8 : int -> int#  = "foo" "bar" [@@unboxed];;
                     ^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}];;

external f10_9 : (int#[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-22:
1 | external f10_9 : (int#[@untagged]) -> bool -> string  = "foo" "bar";;
                      ^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

external f10_10 : string -> (int#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 29-33:
1 | external f10_10 : string -> (int#[@untagged])  = "foo" "bar";;
                                 ^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

(***********************************************)
(* Test 11: untagged_immediate banned in extensible variants *)

(* CR layouts v5.9: Actually allow mixed extensible variant blocks. *)

type t11_1 = ..

type t11_1 += A of t_untagged_immediate;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-39:
3 | type t11_1 += A of t_untagged_immediate;;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

type t11_1 += B of int#;;
[%%expect{|
Line 1, characters 14-23:
1 | type t11_1 += B of int#;;
                  ^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

type ('a : untagged_immediate) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : untagged_immediate mod non_float) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

(* not allowed: extensible variant with unboxed field *)
type 'a t11_2 += C : 'a * 'b -> 'a t11_2

[%%expect{|
Line 1, characters 17-40:
1 | type 'a t11_2 += C : 'a * 'b -> 'a t11_2
                     ^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

(***************************************)
(* Test 12: untagged_immediate in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_untagged_immediate >;;
[%%expect{|
Line 1, characters 15-39:
1 | type t12_1 = < x : t_untagged_immediate >;;
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "t_untagged_immediate" is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of "t_untagged_immediate" must be a sublayout of value
         because it's the type of an object field.
|}];;

type ('a : untagged_immediate) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 41-47:
1 | type ('a : untagged_immediate) t12_2 = < x : 'a >;;
                                             ^^^^^^
Error: Object field types must have layout value.
       The layout of "'a" is untagged_immediate
         because of the annotation on 'a in the declaration of the type t12_2.
       But the layout of "'a" must overlap with value
         because it's the type of an object field.
|}]

class c12_3 = object method x : t_untagged_immediate = assert false end;;
[%%expect{|
Line 1, characters 21-67:
1 | class c12_3 = object method x : t_untagged_immediate = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method "x" has type "t_untagged_immediate"
       but is expected to have type "('a : value)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value
         because it's the type of an object field.
|}];;

class ['a] c12_4 = object
  method x : 'a t_untagged_immediate_id -> 'a t_untagged_immediate_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_untagged_immediate_id -> 'a t_untagged_immediate_id = assert false
                 ^^
Error: This type "('a : value)" should be an instance of type
         "('b : untagged_immediate mod non_float)"
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with untagged_immediate
         because of the definition of t_untagged_immediate_id at line 2, characters 0-59.
|}];;

class c12_5 = object val x : t_untagged_immediate = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_untagged_immediate = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       The layout of x is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of x must be a sublayout of value
         because it's the type of a class field.
|}];;

class type c12_6 = object method x : int# end;;
[%%expect{|
Line 1, characters 26-41:
1 | class type c12_6 = object method x : int# end;;
                              ^^^^^^^^^^^^^^^
Error: The method "x" has type "int#" but is expected to have type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's the type of an object field.
|}];;

class type c12_7 = object val x : int# end
[%%expect{|
Line 1, characters 26-38:
1 | class type c12_7 = object val x : int# end
                              ^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       The layout of x is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of x must be a sublayout of value
         because it's the type of an instance variable.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_untagged_immediate_id -> 'a t_untagged_immediate_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_untagged_immediate_id -> 'a t_untagged_immediate_id
              ^^
Error: This type "('a : value)" should be an instance of type
         "('b : untagged_immediate mod non_float)"
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with untagged_immediate
         because of the definition of t_untagged_immediate_id at line 2, characters 0-59.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_untagged_immediate -> t_untagged_immediate >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_untagged_immediate) : t_untagged_immediate = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_untagged_immediate -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_untagged_immediate = assert false
end;;
[%%expect{|
type t12_8 = < f : t_untagged_immediate -> t_untagged_immediate >
Line 2, characters 26-31:
2 | let f12_9 (o : t12_8) x = o#f x
                              ^^^^^
Error: Non-value layout untagged_immediate detected as sort for type
       t_untagged_immediate,
       but this requires extension layouts, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}];;

(* Third, another disallowed use: capture in an object. *)
let f12_13 m1 m2 = object
  val f = fun () ->
    let _ = f1_1 m1 in
    let _ = f1_1 m2 in
    ()
end;;
[%%expect{|
Line 3, characters 12-16:
3 |     let _ = f1_1 m1 in
                ^^^^
Error: Unbound value "f1_1"
|}];;

let f12_14 (m1 : t_untagged_immediate) (m2 : t_untagged_immediate) = object
  val f = fun () ->
    let _ = f1_1 m1 in
    let _ = f1_1 m2 in
    ()
end;;
[%%expect{|
Line 3, characters 12-16:
3 |     let _ = f1_1 m1 in
                ^^^^
Error: Unbound value "f1_1"
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on untagged_immediate yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_untagged_immediate) = x = x;;
[%%expect{|
Line 1, characters 39-40:
1 | let f13_1 (x : t_untagged_immediate) = x = x;;
                                           ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value.
|}];;

let f13_2 (x : t_untagged_immediate) = compare x x;;
[%%expect{|
Line 1, characters 47-48:
1 | let f13_2 (x : t_untagged_immediate) = compare x x;;
                                                   ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value.
|}];;

let f13_3 (x : t_untagged_immediate) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 56-57:
1 | let f13_3 (x : t_untagged_immediate) = Marshal.to_bytes x;;
                                                            ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value.
|}];;

let f13_4 (x : t_untagged_immediate) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 52-53:
1 | let f13_4 (x : t_untagged_immediate) = Hashtbl.hash x;;
                                                        ^
Error: This expression has type "t_untagged_immediate"
       but an expression was expected of type "('a : value)"
       The layout of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the layout of t_untagged_immediate must be a sublayout of value.
|}];;
