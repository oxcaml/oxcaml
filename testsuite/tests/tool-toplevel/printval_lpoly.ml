(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Layout-poly values should print as <lpoly> in the toplevel.

   CR-soon zqian: let poly_ currently crashes at code generation, so this
   test captures the current fatal error. Once let poly_ translation is
   implemented, this should instead show:
     val id : layout_ l. ('a : l). 'a -> 'a = <lpoly>
*)
let poly_ id x = x
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]
