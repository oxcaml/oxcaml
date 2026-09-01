(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* Makes sure that top-level zapping is accurate *)

fun x -> x;;
[%%expect{|
- : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]
