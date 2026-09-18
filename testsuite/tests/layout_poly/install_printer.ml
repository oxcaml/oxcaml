(* TEST
 flags = "-extension layout_poly_alpha";
 ocamlrunparam += ",b=0";
 ocaml_exit_status = "2";
 toplevel;
*)

type ('a : any) t = Foo;;

let poly_ print_nothing out x = print_string "~Foo";;

(* Currently, this does not work because we use [Ctype.is_moregeneral] when
   managing printers, which loses track of generic sort variables.
   This is fine as the toplevel does not really support layout polymorphism
   anyway. Tracked by ticket 7837. *)
#install_printer print_nothing;;
Foo;;
