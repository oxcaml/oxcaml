(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Only the never-returning raise builtins may declare a result type of
   layout [any]. *)

external my_raise : ('a : any). exn -> 'a = "%reraise"
[%%expect{|
external my_raise : ('a : any). exn -> 'a = "%reraise"
|}]

let never_returns : type (a : any). unit -> a = fun () -> my_raise Exit
[%%expect{|
val never_returns : ('a : any). unit -> 'a = <fun>
|}]

let first_class : type (a : any). exn -> a = my_raise
[%%expect{|
val first_class : ('a : any). exn -> 'a = <fun>
|}]

external bad_c_stub : ('a : any). string -> 'a = "c_stub"
[%%expect{|
Line 1, characters 44-46:
1 | external bad_c_stub : ('a : any). string -> 'a = "c_stub"
                                                ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the result of an external declaration.
|}]

external bad_builtin : ('a : any). exn -> 'a = "%identity"
[%%expect{|
Line 1, characters 42-44:
1 | external bad_builtin : ('a : any). exn -> 'a = "%identity"
                                              ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the result of an external declaration.
|}]

(* The argument of a raise builtin must still be representable. *)
external bad_arg : ('a : any). 'a -> exn = "%reraise"
[%%expect{|
Line 1, characters 31-33:
1 | external bad_arg : ('a : any). 'a -> exn = "%reraise"
                                   ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an argument in an external declaration.
|}]

(* [@layout_poly] wins over the raise-builtin exemption and is then
   rejected by the builtin value-repr check. *)
external bad_poly : ('a : any). exn -> 'a = "%reraise" [@@layout_poly]
[%%expect{|
Line 1, characters 20-41:
1 | external bad_poly : ('a : any). exn -> 'a = "%reraise" [@@layout_poly]
                        ^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%reraise] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
Hint: This was expected to be a value-only primitive. You might've
      misspelled the primitive name.
|}]
