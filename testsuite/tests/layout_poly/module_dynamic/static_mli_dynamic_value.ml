(* TEST
 flags = "-extension layout_poly_alpha";
 readonly_files = "static_mli_dynamic_value.mli \
                   static_mli_dynamic_value.reference";
 setup-ocamlc.byte-build-env;
 module = "static_mli_dynamic_value.mli";
 ocamlc.byte;
 module = "static_mli_dynamic_value.ml";
 compiler_output = "static_mli_dynamic_value.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 compiler_reference = "static_mli_dynamic_value.reference";
 check-ocamlc.byte-output;
*)

let x = Fun.id 42
