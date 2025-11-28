(* TEST
   flags = "-extension layout_poly_alpha";
   expect;
*)

exception Force_type
;;

let const_one : (repr_ 'a). 'a -> int =
  let f x = 1 in f
;;
[%%expect {|
exception Force_type
Line 4, characters 4-13:
4 | let const_one : (repr_ 'a). 'a -> int =
        ^^^^^^^^^
Error: This pattern matches values of type "(repr_ 'a). 'a -> int"
       but a pattern was expected which matches values of type "'b"
       The universal variable "'a" would escape its scope
|}];;

let const_one_with_fun : (repr_ 'a). 'a -> int =
  fun x -> 1
;;
[%%expect {|
Line 1, characters 4-22:
1 | let const_one_with_fun : (repr_ 'a). 'a -> int =
        ^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "(repr_ 'a). 'a -> int"
       but a pattern was expected which matches values of type "'b"
       The universal variable "'a" would escape its scope
|}];;

let ident : (repr_ 'a). 'a -> 'a =
  fun x -> x
;;
[%%expect {|
Line 1, characters 4-9:
1 | let ident : (repr_ 'a). 'a -> 'a =
        ^^^^^
Error: This pattern matches values of type "(repr_ 'a). 'a -> 'a"
       but a pattern was expected which matches values of type "'b"
       The universal variable "'a" would escape its scope
|}];;

let forced : (repr_ 'a). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;

let forced2 : (repr_ 'a) (repr_ 'b). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;

let forced3 : (repr_ 'a) (repr_ 'b) (repr_ 'c). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;
