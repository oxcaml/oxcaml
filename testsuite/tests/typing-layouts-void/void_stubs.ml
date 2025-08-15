(* TEST
   expect;
*)

(* CR layouts v5: improve the error message for disallowed cases *)

(* no product args whatsoever. but we allow (? & ?) returns, including when one
   or both is void, as long as neither are products *)

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

external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
[%%expect{|
external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
|}]

external ext_void_void_return : unit -> r = "foo" "bar"
[%%expect{|
external ext_void_void_return : unit -> r = "foo" "bar"
|}]

external ext_void_in_product_arg : #(string * void) -> unit = "foo" "bar"
[%%expect{|
Line 1, characters 35-59:
1 | external ext_void_in_product_arg : #(string * void) -> unit = "foo" "bar"
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
external ext_void_void_arg : r -> unit = "foo" "bar"
[%%expect{|
Line 1, characters 29-38:
1 | external ext_void_void_arg : r -> unit = "foo" "bar"
                                 ^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_void_void_void_return : unit -> #(void * void * void) = "foo" "bar"
[%%expect{|
Line 1, characters 37-66:
1 | external ext_void_void_void_return : unit -> #(void * void * void) = "foo" "bar"
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
