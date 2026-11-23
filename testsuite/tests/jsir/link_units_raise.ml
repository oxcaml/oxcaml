(* TEST
 modules = "link_units_raise_a.ml";
 flambda2;
 exit_status = "2";
 {
   native;
 }{
   jsir;
 }{
   ocamlopt_flags = "-link-jsir";
   jsir;
 }
*)

(* The toplevel code of Link_units_raise_a never exits normally, so with
   -link-jsir this unit is unreachable in the linked JSIR program (see
   Jsir_linker), and the exception escapes. *)

let () = print_endline "init main"

let () = Printf.printf "value: %d\n" Link_units_raise_a.value
