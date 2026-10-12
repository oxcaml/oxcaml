(* TEST
 flags = "-no-syntax-quotations";
 ocaml_script_as_argument = "true";
 toplevel;
*)

let ( $ ) f x = f x
let () = print_endline $ "ok"
