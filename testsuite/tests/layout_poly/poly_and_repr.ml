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
Line 5, characters 17-18:
5 |   let f x = 1 in f
                     ^
Error: This expression has type "'b -> int"
       but an expression was expected of type "(repr_ 'a). 'a -> int"
|}];;

let const_one_with_fun : (repr_ 'a). 'a -> int =
  fun (x : any) -> 1
;;
[%%expect {|
Line 2, characters 2-20:
2 |   fun (x : any) -> 1
      ^^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "(repr_ 'a). 'a -> int"
|}];;

let ident : (repr_ 'a). 'a -> 'a =
  fun x -> x
;;
[%%expect {|
Line 2, characters 2-12:
2 |   fun x -> x
      ^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "(repr_ 'a). 'a -> 'a"
|}];;

let const_fun_repr_arg : ((repr_ 'a). 'a -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val const_fun_repr_arg : ((repr_ 'a). 'a -> unit) -> unit = <fun>
|}]

let poly_and_repr_1 : 'b. ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_1 : ((repr_ 'a). 'a -> 'b -> unit) -> unit = <fun>
|}]

let poly_and_repr_2 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_2 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  <fun>
|}]

let poly_and_repr_3 : ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_3 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  <fun>
|}]

let poly_and_repr_4 : ('b : value). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_4 : ((repr_ 'a). 'a -> 'b -> unit) -> unit = <fun>
|}]

let forced : (repr_ 'a). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;

let poly_and_repr_1 : 'b. ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_1 : ((repr_ 'a). 'a -> 'b -> unit) -> unit = <fun>
|}]


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
