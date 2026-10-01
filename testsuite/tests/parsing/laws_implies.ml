(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* [===>] is reserved for laws, whether or not the extension is enabled. *)

let ( ===> ) a b = not a || b
