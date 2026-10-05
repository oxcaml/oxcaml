(* TEST
 set OCAMLPARAM = "syntax-quotations=1,_";
 flags = "-no-syntax-quotations";
*)

(* A setting placed before [_] in OCAMLPARAM is a default that the command
   line overrides. *)
let ( $ ) f x = f x
let () = print_endline $ "ok"
