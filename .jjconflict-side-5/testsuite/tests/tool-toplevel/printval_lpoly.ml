(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Layout-poly values should print as <lpoly> in the toplevel. *)
let poly_ id x = x
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]
