(* TEST
 readonly_files = "static_param.mli dynamic_arg.mli \
                   static_param_dynamic_arg_test.reference";
 setup-ocamlc.byte-build-env;
 flags = "-extension layout_poly_alpha -as-parameter";
 module = "static_param.mli";
 ocamlc.byte;
 flags = "-extension layout_poly_alpha -as-argument-for Static_param";
 module = "dynamic_arg.mli";
 compiler_output = "dynamic_arg.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 compiler_reference = "static_param_dynamic_arg_test.reference";
 check-ocamlc.byte-output;
*)
