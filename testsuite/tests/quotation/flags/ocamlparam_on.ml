(* TEST
 set OCAMLPARAM = "_,syntax-quotations=1";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let ( $ ) f x = f x
