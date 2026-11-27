(* TEST
 readonly_files = "sum_shape.mli";
 setup-ocamlc.byte-build-env;
 module = "sum_shape.mli";
 ocamlc.byte;
 flags = "-no-extension layouts";
 module = "layouts_disabled.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let pass (value : Sum_shape.refined) = value
