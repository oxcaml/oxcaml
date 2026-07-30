(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Layout-poly values should print as <lpoly> in the toplevel. *)
let poly_ id x = x
[%%expect{|
>> Fatal error: No compilation unit set
Uncaught exception: Misc.Fatal_error

|}]
