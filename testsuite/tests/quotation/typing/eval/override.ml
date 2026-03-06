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
val f_predef : 'a eval -> 'a eval = <fun>
type 'a predef_eval = 'a eval
type 'a eval
val f_override : 'a eval -> 'a eval = <fun>
|}]

(* Type names are distinguished sensibly *)
let f_predef_and_override = f_predef, f_override
[%%expect {|
val f_predef_and_override :
  ('a eval/2 -> 'a eval/2) * ('b eval/1 -> 'b eval/1) = (<fun>, <fun>)
|}]

(* [predef_eval] still reduces *)
let f (x : <[int]> predef_eval) : int = x
[%%expect {|
val f : <[int]> predef_eval -> int = <fun>
|}]
(* ...but the fake [eval] does not *)
let f (x : <[int]> eval) : int = x
[%%expect {|
Line 1, characters 33-34:
1 | let f (x : <[int]> eval) : int = x
                                     ^
Error: This expression has type "<[int]> eval"
       but an expression was expected of type "int"
|}]
