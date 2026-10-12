(* TEST
 flags = "-syntax-quotations";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* With quotation syntax, [$] is the splice token, so this is a syntax
   error. *)
let ( $ ) f x = f x
