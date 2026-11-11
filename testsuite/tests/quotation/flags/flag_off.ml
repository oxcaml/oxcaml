(* TEST
 flags = "-no-syntax-quotations";
*)

(* Without quotation syntax, [$] is an ordinary infix operator. *)
let ( $ ) f x = f x
let () = print_endline $ "ok"
