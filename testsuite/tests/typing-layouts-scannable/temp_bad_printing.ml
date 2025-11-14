(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* CR layouts-scannable: float64 should not print with maybe_pointer when it
   is in a product layout. when it isn't, it prints out fine. *)
type t : float64
type t : float64 & value
[%%expect{|
type t : float64
type t : float64 & value
|}]

(* CR layouts-scannable: similarly, the printing in error messages contains
   extra [maybe_pointer] annotations. This is likely due to calling
   [Layout.format] somewhere, which will just print out all of the axes *)
type a : any
type b : float32 = a
[%%expect{|
type a : any
Line 2, characters 0-20:
2 | type b : float32 = a
    ^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of type "a" must be a sublayout of float32
         because of the definition of b at line 2, characters 0-20.
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 29-36:
1 | let f = fun (type (a : any)) (x : a) -> x
                                 ^^^^^^^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* there is also a question of when to print out abbreviations vs
   the scannable axes applied to scannable that they apply to. *)
