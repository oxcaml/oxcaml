(* TEST

 readonly_files = "\
   unrepresentable_param.mli \
   uses_unrepresentable_param.ml uses_unrepresentable_param.mli \
   uses_unrepresentable_param.reference \
   string_argument.ml string_argument.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "unrepresentable_param.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Unrepresentable_param -dlambda";
 module = "string_argument.mli string_argument.ml";
 ocamlopt.byte;

 flags = "-parameter Unrepresentable_param";
 module = "uses_unrepresentable_param.mli";
 ocamlopt.byte;

 module = "uses_unrepresentable_param.ml";
 compiler_output = "uses_unrepresentable_param.output";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;

 compiler_reference = "uses_unrepresentable_param.reference";
 check-ocamlopt.byte-output;
*)
