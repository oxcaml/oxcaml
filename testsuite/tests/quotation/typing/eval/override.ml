(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that having another [eval]-named type in scope is sane. *)

(* Predefined [eval] reduces as expected *)
let f (x : <[int]> eval) : int = x
[%%expect {|
val f : <[int]> eval -> int = <fun>
|}]

(* Define another type named [eval] *)
let f_predef (x : 'a eval) = x
type ('a : <[any]>) predef_eval = 'a eval
type ('a : <[any]>) eval
let f_override (x : 'a eval) = x
[%%expect {|
Line 1, characters 13-26:
1 | let f_predef (x : 'a eval) = x
                 ^^^^^^^^^^^^^
Error: This pattern matches values of type "'a eval"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_1)"
       The layout of 'a eval is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a eval must be representable
         because we must know concretely how to pass a function argument.
|}]

(* Type names are distinguished sensibly *)
let f_predef_and_override = f_predef, f_override
[%%expect {|
Line 1, characters 28-36:
1 | let f_predef_and_override = f_predef, f_override
                                ^^^^^^^^
Error: Unbound value "f_predef"
|}]

(* [predef_eval] still reduces *)
let f (x : <[int]> predef_eval) : int = x
[%%expect {|
Line 1, characters 19-30:
1 | let f (x : <[int]> predef_eval) : int = x
                       ^^^^^^^^^^^
Error: Unbound type constructor "predef_eval"
|}]
(* ...but the fake [eval] does not *)
let f (x : <[int]> eval) : int = x
[%%expect {|
val f : <[int]> eval -> int = <fun>
|}]
