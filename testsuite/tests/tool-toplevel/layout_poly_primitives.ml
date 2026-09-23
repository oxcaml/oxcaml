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

let (_ : (_ : float64) -> _) = box;;
let f (x : (_ : float64)) = x;;
(box : (_ : float64) -> _);;
let (_ : (_ : float64) box -> _) = unbox;;
let (_ : (_ : value & float64) -> _) = box;;
let (_ : (_ : float64) -> _ -> _) = fun _ x -> x;;
let _ = fun x -> x;;
