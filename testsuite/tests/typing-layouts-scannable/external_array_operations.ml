(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* There is a particularly tricky edge case that arises in Typeopt's
   [type_representable_layout]. More documentation is present there; this file
   tests the current behavior. *)

(* CR layouts-scannable: This signature seems needlessly restrictive
   (but this probably doesn't matter too much). *)
external weird_id : ('a : any mod separable). 'a array -> int = "%identity"
let weird_id' arr = weird_id arr
[%%expect{|
external weird_id : ('a : any separable). 'a array -> int = "%identity"
val weird_id' : ('a : value maybe_null). 'a array -> int = <fun>
|}]

(* This is not sound whenever 'a is not a value, but this is the kind of
   unsoundness that is generally permitted with [external] declarations *)
external weird_flarr_len : ('a : any mod separable). 'a array -> int = "%floatarray_length"
let weird_flarr_len' x = weird_flarr_len x
[%%expect{|
external weird_flarr_len : ('a : any separable). 'a array -> int
  = "%floatarray_length"
val weird_flarr_len' : ('a : value maybe_null). 'a array -> int = <fun>
|}]

external product_edge_case : ('a : any mod separable). #('a * 'a) array -> int = "%identity"
let product_edge_case x = product_edge_case x
[%%expect{|
external product_edge_case : ('a : any separable). #('a * 'a) array -> int
  = "%identity"
val product_edge_case : ('a : value maybe_null). #('a * 'a) array -> int =
  <fun>
|}]
