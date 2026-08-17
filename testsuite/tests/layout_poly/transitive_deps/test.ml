(* TEST
 readonly_files = "a.mli a.ml b.mli b.ml";
 setup-ocamlopt.byte-build-env;
 (* [-nocwd] is needed because [-Ix .] doesn't override the implicit [-I .]. *)
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 module = "a.mli";
 ocamlopt.byte;
 module = "a.ml";
 ocamlopt.byte;
 module = "b.mli";
 ocamlopt.byte;
 module = "b.ml";
 ocamlopt.byte;
 module = "test.ml";
 ocamlopt.byte;
 unset module;
 program = "${test_build_directory}/test.exe";
 all_modules = "a.cmx b.cmx test.cmx";
 ocamlopt.byte;
 run;
*)
let _ = B.id 52
