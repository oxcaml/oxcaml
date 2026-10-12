(* TEST
 flags = "-no-syntax-quotations";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

#syntax quotations on

(* The directive overrides the command-line default for this file. *)
let ( $ ) f x = f x
