(* TEST
 expect;
*)

#syntax quotations on

(* Test that [expr] and [eval] are not added to the [Predef] environment
   if the [Runtime_metaprogramming] extension is not enabled.
   The other tests verify that it is accessible when the extension is
   turned on. *)

type 'a t = 'a expr
[%%expect {|
Line 1, characters 15-19:
1 | type 'a t = 'a expr
                   ^^^^
Error: Unbound type constructor "expr"
|}];;

type 'a t = 'a eval
[%%expect {|
Line 1, characters 15-19:
1 | type 'a t = 'a eval
                   ^^^^
Error: Unbound type constructor "eval"
|}];;

type t = <[int]> expr -> <[int]> eval
[%%expect {|
Line 1, characters 17-21:
1 | type t = <[int]> expr -> <[int]> eval
                     ^^^^
Error: Unbound type constructor "expr"
|}];;
