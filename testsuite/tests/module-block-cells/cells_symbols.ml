(* TEST
 readonly_files = "check_cells_global.sh";
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 output = "${test_build_directory}/cells_symbols.output";
 reference = "${test_source_directory}/cells_symbols.reference";
 script = "sh ${test_source_directory}/check_cells_global.sh";
 {
   flags = "-S";
   ocamlopt.byte;
   script;
   check-program-output;
 }{
   flags = "-opaque -S";
   ocamlopt.byte;
   script;
   check-program-output;
 }{
   flags = "-Oclassic -S";
   ocamlopt.byte;
   script;
   check-program-output;
 }
*)

(* The module-block cells must be global symbols (not made local by the cmx
   reachability filter), including under -opaque and -Oclassic, so that a
   consumer compiled without this unit's .cmx can still reference them. *)

let f x = x + 1
let n = Sys.opaque_identity 3
