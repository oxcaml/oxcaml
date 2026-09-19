(* TEST
 flags = "-extension layout_poly -nocwd -Ix .";
 readonly_files = "dynamic_lib.mli dynamic_lib.ml static_mli_include_dynamic.mli \
                   static_mli_include_dynamic.reference";
 setup-ocamlc.byte-build-env;
 module = "dynamic_lib.mli";
 ocamlc.byte;
 module = "dynamic_lib.ml";
 ocamlc.byte;
 module = "static_mli_include_dynamic.mli";
 ocamlc.byte;
 module = "static_mli_include_dynamic.ml";
 compiler_output = "static_mli_include_dynamic.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "static_mli_include_dynamic.reference";
 check-ocamlc.byte-output;
*)

include Dynamic_lib.Builtin

let h = Dynamic_lib.h
