(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* CR layouts v5: improve the error messages *)

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
external ext_void_in_product_arg : #(string * void) -> unit = "foo" "bar"
|}]

external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
[%%expect{|
external ext_void_in_product_return : unit -> #(string * void) = "foo" "bar"
|}]

external ext_all_void_arg : r -> unit = "foo" "bar"
[%%expect{|
external ext_all_void_arg : r -> unit = "foo" "bar"
|}]
external ext_all_void_return : unit -> r = "foo" "bar"
[%%expect{|
external ext_all_void_return : unit -> r = "foo" "bar"
|}]
