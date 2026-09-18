(* TEST
 set OCAMLPARAM = "_,syntax-quotations=0";
*)

let ( $ ) f x = f x
let () = print_endline $ "ok"
