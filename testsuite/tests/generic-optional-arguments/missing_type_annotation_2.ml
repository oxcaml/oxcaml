(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 (* This test should fail after type checking is implemented. *)
 (*= ocamlc_byte_exit_status = "2"; *)
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let chain_call Stdlib.Option.?(sep) arg =
  String.concat (match sep with None -> " " | Some s -> s) arg
