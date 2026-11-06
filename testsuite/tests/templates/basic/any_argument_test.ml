(* TEST

 readonly_files = "\
   unrepresentable_param.mli \
   any_argument.mli any_argument.reference \
   included_argument.ml \
   included_argument_intf.mli included_argument_intf.reference \
   included_any_argument_intf.mli included_any_argument_intf.reference \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "unrepresentable_param.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Unrepresentable_param";
 module = "any_argument.mli";
 compiler_output = "any_argument.output";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;

 compiler_reference = "any_argument.reference";
 check-ocamlopt.byte-output;

 module = "included_argument.ml";
 compiler_output = "included_argument.output";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;

 module = "included_argument_intf.mli";
 compiler_output = "included_argument_intf.output";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;

 compiler_reference = "included_argument_intf.reference";
 check-ocamlopt.byte-output;

 module = "included_any_argument_intf.mli";
 compiler_output = "included_any_argument_intf.output";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;

 compiler_reference = "included_any_argument_intf.reference";
 check-ocamlopt.byte-output;
*)
