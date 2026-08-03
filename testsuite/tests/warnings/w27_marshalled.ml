(* TEST
 readonly_files = "marshall_for_w27.ml w27_source.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/marshall_for_w27.exe";
 all_modules = "marshall_for_w27.ml";
 ocamlc.byte;
 run;
 module = "w27_source.marshalled.ml";
 compiler_reference = "${test_source_directory}/w27_marshalled.compilers.reference";
 flags = "-w +27";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This tests that warning 27 fires appropriately when dealing with marshalled
   ASTs (where all locations have loc_ghost = true). It does that by marshalling
   `w27_source.ml` to disk and then passing the marshalled ast to the
   compiler. *)
