(* TEST
   flags = "-extension layout_poly_alpha";
   toplevel;
*)

exception Force_type
;;

let _ : (repr_ 'a : any). 'a -> 'b -> 'a * 'b = raise Force_type
;;

let _ : (repr_ 'a). (repr_ 'b). 'a -> 'b -> 'a * 'b = raise Force_type
;;

let _ : (repr_ 'a) 'b. 'a -> 'b -> 'a * 'b = raise Force_type
;;

let _ : 'a (repr_ 'b). 'a -> 'b -> 'a * 'b = raise Force_type
;;
