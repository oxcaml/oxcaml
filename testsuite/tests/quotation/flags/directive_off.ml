(* TEST
 flags = "-syntax-quotations";
*)

#syntax quotations off

(* The directive overrides the command-line default for this file. *)
let ( $ ) f x = f x
let () = print_endline $ "ok"
