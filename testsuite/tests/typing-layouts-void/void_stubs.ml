(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Void is not yet allowed in C stubs. *)
(* CR layouts v5: allow some of these, improve the error message for others *)

type void : void
type r = #{ v1 : void; v2 : void }
[%%expect{|
type void : void
type r = #{ v1 : void; v2 : void; }
|}]


external ext_void_arg : void -> unit = "foo" "bar"
[%%expect{|
external ext_void_arg : void -> unit = "foo" "bar"
|}]

external ext_void_return : unit -> void = "foo" "bar"
[%%expect{|
external ext_void_return : unit -> void = "foo" "bar"
|}]

external ext_void_in_product_arg : #(string * void) -> unit = "foo" "bar"
[%%expect{|
Line 1, characters 35-59:
1 | external ext_void_in_product_arg : #(string * void) -> unit = "foo" "bar"
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
[%%expect{|
external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
|}]

external ext_all_void_arg : r -> unit = "foo" "bar"
[%%expect{|
Line 1, characters 28-37:
1 | external ext_all_void_arg : r -> unit = "foo" "bar"
                                ^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
external ext_all_void_return : unit -> r = "foo" "bar"
[%%expect{|
external ext_all_void_return : unit -> r = "foo" "bar"
|}]
