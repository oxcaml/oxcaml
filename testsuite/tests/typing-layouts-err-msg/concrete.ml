(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(**************************)
(* Concrete layout errors *)

type t_any : any
type t_void : void

(* Match *)
let () = match (assert false : t_any) with _ -> ()

[%%expect{|
type t_any : any
type t_void : void
Line 5, characters 15-37:
5 | let () = match (assert false : t_any) with _ -> ()
                   ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_any" but an expression was expected of type
         "('a : '_representable_layout_1)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because a value of this type is matched against a pattern.
|}]

(* Constructor_arg_projection *)
type t = A of t_any
let f (A _) = ()
[%%expect {|
type t = A of t_any
Line 2, characters 9-10:
2 | let f (A _) = ()
             ^
Error: Constructor arguments being projected must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor argument being projected.
|}]

(* Constructor_arg_assignment *)
let _ = A (assert false)
[%%expect {|
Line 1, characters 10-24:
1 | let _ = A (assert false)
              ^^^^^^^^^^^^^^
Error: Constructor arguments must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

(* Unannotated_type_parameter *)
type 'a t = 'a
and t2 = t_any t

[%%expect{|
Line 2, characters 9-14:
2 | and t2 = t_any t
             ^^^^^
Error: This type "t_any" should be an instance of type
         "('a : '_representable_layout_2 separable non_null)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it instantiates an unannotated type parameter of t.
|}]

(* Record_projection *)
type t = {a: t_any}
let f (t: t) = t.a
[%%expect {|
type t = { a : t_any; }
Line 2, characters 15-18:
2 | let f (t: t) = t.a
                   ^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a field in a record being projected from.
|}]

(* Record_assignment *)
let _ = { a = assert false }
[%%expect {|
Line 1, characters 14-26:
1 | let _ = { a = assert false }
                  ^^^^^^^^^^^^
Error: Values of fields must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a field being assigned a value.
|}]

(* Let_binding *)
let x: t_any = assert false

[%%expect{|
Line 1, characters 4-5:
1 | let x: t_any = assert false
        ^
Error: This pattern matches values of type "t_any"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_3)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* Function_argument *)
let f (x: t_any) = ()

[%%expect{|
Line 1, characters 6-16:
1 | let f (x: t_any) = ()
          ^^^^^^^^^^
Error: This pattern matches values of type "t_any"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_4)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because we must know concretely how to pass a function argument.
|}]

(* Function_result *)
let f (): t_any = assert false

[%%expect{|
Line 1, characters 18-30:
1 | let f (): t_any = assert false
                      ^^^^^^^^^^^^
Error: This expression has type "t_any" but an expression was expected of type
         "('a : '_representable_layout_5)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because we must know concretely how to return a function result.
|}]

(* Structure_item_expression *)
(* See [concrete_struct_item_expr.ml] *)

(* V1_safety_check *)
(* See [concrete_v1_check.ml] *)

(* External_argument *)
external eq : t_any -> 'a -> bool = "%equal"
[%%expect{|
Line 1, characters 14-19:
1 | external eq : t_any -> 'a -> bool = "%equal"
                  ^^^^^
Error: Types in an external must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of an argument in an external declaration.
|}]
(* Shadowed by Function_argument *)

(* External_result *)
external eq : 'a -> 'a -> t_any = "%equal"
[%%expect{|
Line 1, characters 26-31:
1 | external eq : 'a -> 'a -> t_any = "%equal"
                              ^^^^^
Error: Types in an external must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of the result of an external declaration.
|}]
(* Shadowed by Function_result *)

(* Statement *)
let _ = (assert false : t_any); ()

[%%expect{|
Line 1, characters 8-30:
1 | let _ = (assert false : t_any); ()
            ^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 1, characters 9-21:
1 | let _ = (assert false : t_any); ()
             ^^^^^^^^^^^^
Error: This expression has type "t_any" but an expression was expected of type
         "('a : '_representable_layout_6)"
       because it is in the left-hand side of a sequence
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a statement.
|}]

(* Module_projection *)
type t_any : any
[%%expect{|
type t_any : any
|}]

module F(M : sig val x : t_any end) = struct
  let x = M.x
end
[%%expect{|
Line 2, characters 10-13:
2 |   let x = M.x
              ^^^
Error: This path projects from a module that contains unrepresentable items.
Line 1, characters 17-30:
1 | module F(M : sig val x : t_any end) = struct
                     ^^^^^^^^^^^^^
  "x" is not representable.
  The layout of t_any is any
    because of the definition of t_any at line 1, characters 0-16.
  But the layout of t_any must be representable
    because it's the type of an item in a module being projected from.
|}]

(* Module_coercion *)
module type S = sig
  val x : t_any
  val y : int
end

module type U = sig
  val y : int
end

module G (X : U) = struct type u = int end
[%%expect{|
module type S = sig val x : t_any val y : int end
module type U = sig val y : int end
module G : functor (X : U) -> sig type u = int end
|}]

module F (M : S) = struct
  (* This checks S <: U *)
  type v = G(M).u
end
[%%expect{|
Line 3, characters 11-15:
3 |   type v = G(M).u
               ^^^^
Error: Modules do not match: sig val x : t_any val y : int end
     is not included in U
     Signatures containing unrepresentable items cannot be
     coerced to other module types.
     "x" is not representable.
     The layout of t_any is any
       because of the definition of t_any at line 1, characters 0-16.
     But the layout of t_any must be representable
       because it's the type of an item in a module being coerced.
|}]

(* Module_inclusion *)
module F(M : sig val x : t_any end) = struct
  include M
end
[%%expect{|
Line 2, characters 2-11:
2 |   include M
      ^^^^^^^^^
Error: This module contains unrepresentable items, so it cannot be included.
Line 1, characters 17-30:
1 | module F(M : sig val x : t_any end) = struct
                     ^^^^^^^^^^^^^
  "x" is not representable.
  The layout of t_any is any
    because of the definition of t_any at line 1, characters 0-16.
  But the layout of t_any must be representable
    because it's the type of an item in a module being included.
|}]

(* Module_open *)
module F(M : sig val x : t_any end) = struct
  open (M : sig val x : t_any end)
end
[%%expect{|
Line 2, characters 2-34:
2 |   open (M : sig val x : t_any end)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module contains unrepresentable items, so it cannot be opened.
Line 2, characters 16-29:
2 |   open (M : sig val x : t_any end)
                    ^^^^^^^^^^^^^
  "x" is not representable.
  The layout of t_any is any
    because of the definition of t_any at line 1, characters 0-16.
  But the layout of t_any must be representable
    because it's the type of an item in a module being opened.
|}]
