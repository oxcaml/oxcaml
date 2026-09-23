(* TEST
 {
   toplevel;
 }{
   toplevel.opt;
 }
*)

box;;
unbox;;
Stdlib.box;;
Stdlib.unbox;;

let _ = box;;
let _ = unbox;;
let b = box;;
b;;

(box : float# -> float);;
(unbox : float -> float#);;
let boxed = box #3.5;;
unbox boxed;;
unbox (box #("product", #4.5));;

external opaque : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly];;
opaque;;
opaque #("opaque", #5L);;
