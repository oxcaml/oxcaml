(* TEST
 flambda2;
 readonly_files =
   "recover_sources_from_usages_lib.mli recover_sources_from_usages_lib.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -flambda2-reaper -reaper-local-fields \
          -flambda2-result-types-all-functions -reaper-debug-flags=nostamps";
 module = "recover_sources_from_usages_lib.mli";
 ocamlopt.byte;
 module = "recover_sources_from_usages_lib.ml";
 ocamlopt.byte with dump-simplify, dump-reaper;
 check-fexpr-dump;
 flags = "-O3";
 ocamlopt_flags = "";
 fexpr_dump_file = "";
 module = "recover_sources_from_usages.ml";
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
 flags = "";
 module = "";
 program = "${test_build_directory}/recover_sources_from_usages.exe";
 all_modules =
   "recover_sources_from_usages_lib.cmx recover_sources_from_usages.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

let () =
  print_int (Recover_sources_from_usages_lib.o ());
  print_newline ()
