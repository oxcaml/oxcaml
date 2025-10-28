(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

#syntax quotations on

let variable_eval (type a) () = [%eval: a]
