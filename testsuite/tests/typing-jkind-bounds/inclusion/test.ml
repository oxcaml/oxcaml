(* TEST
   readonly_files = "inclusion.mli inclusion.ml";
   setup-ocamlc.byte-build-env;
   module = "inclusion.mli";
   ocamlc.byte;
   module = "inclusion.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)
(* CR layouts v2.8: This should be accepted *)
