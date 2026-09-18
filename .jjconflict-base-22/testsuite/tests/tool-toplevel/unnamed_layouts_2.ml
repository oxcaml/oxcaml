(* TEST
 {
   toplevel;
 }{
   toplevel.opt;
 }
*)

(* We allow unnamed non-values at the toplevel, but they are always printed
   as "<abstr>" *)

let _ = #3.14;;
