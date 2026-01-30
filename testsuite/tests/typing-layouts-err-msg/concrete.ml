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

(* Constructor_declaration *)
type t = A of t_any

[%%expect{|
type t = A of t_any
|}]

(* Label_declaration *)
type t = {a: t_any}

[%%expect{|
type t = { a : t_any; }
|}]

(* Constructor_arg_projection *)
let f (A _) = ()
[%%expect {|
Line 1, characters 9-10:
1 | let f (A _) = ()
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
Line 1, characters 8-24:
1 | let _ = A (assert false)
            ^^^^^^^^^^^^^^^^
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
         "('a : '_representable_layout_2)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it instantiates an unannotated type parameter of t.
|}]

(* Record_projection *)
let f (t: t) = t.a
[%%expect {|
Line 1, characters 15-18:
1 | let f (t: t) = t.a
                   ^^^
Error: Fields being projected must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a field being projected.
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
