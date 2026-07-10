(* TEST
 readonly_files = "immediate_tags_cmi_sparse_impl.mli immediate_tags_cmi_sparse_impl.ml immediate_tags_cmi_sparse_user.ml";
 setup-ocamlc.byte-build-env;
 module = "immediate_tags_cmi_sparse_impl.mli";
 ocamlc.byte;
 module = "immediate_tags_cmi_sparse_impl.ml";
 ocamlc.byte;
 module = "immediate_tags_cmi_sparse_user.ml";
 ocamlc.byte;
 module = "";
 program = "${test_build_directory}/immediate_tags_cmi_sparse.exe";
 all_modules = "immediate_tags_cmi_sparse_impl.cmo immediate_tags_cmi_sparse_user.cmo";
 ocamlc.byte;
 run;
 check-program-output;
*)
