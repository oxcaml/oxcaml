(* TEST
 readonly_files = "immediate_tags_cmi_bad.mli immediate_tags_cmi_bad.ml";
 setup-ocamlc.byte-build-env;
 module = "immediate_tags_cmi_bad.mli";
 ocamlc.byte;
 module = "immediate_tags_cmi_bad.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
