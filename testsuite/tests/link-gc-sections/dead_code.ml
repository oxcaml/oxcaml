(* TEST
 readonly_files = "dead_code_lib.ml";
 flags = "-g -function-sections";
 native-compiler;
 function_sections;
 link_order_frametables;
 setup-ocamlopt.byte-build-env;
 module = "dead_code_lib.ml";
 ocamlopt.byte;
 module = "dead_code.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "dead_code_lib.cmx dead_code.cmx";
 reference = "${test_source_directory}/dead_code.reference";
 {
   program = "${test_build_directory}/dead_code_noexport.exe";
   flags = "-g -no-export-dynamic";
   ocamlopt.byte;
   run;
   check-program-output;
   script = "sh ${test_source_directory}/check_symbols.sh absent";
   script;
 }{
   program = "${test_build_directory}/dead_code_default.exe";
   flags = "-g";
   ocamlopt.byte;
   run;
   check-program-output;
   script = "sh ${test_source_directory}/check_symbols.sh present";
   script;
 }{
   program = "${test_build_directory}/dead_code_nogc.exe";
   flags = "-g -no-gc-sections";
   ocamlopt.byte;
   run;
   check-program-output;
   script = "sh ${test_source_directory}/check_symbols.sh present";
   script;
 }
*)

(* The functions under test live in Dead_code_lib so that they are exported
   from a separately compiled unit, i.e. reachable through the .cmx interface
   and hence only removable by the linker.  [Dead_code_lib.dead] is never
   referenced; the live path raises through several non-inlined frames and
   allocates across a major GC so that the surviving frame descriptors are
   exercised. *)

let () =
  Printexc.record_backtrace true;
  (try Dead_code_lib.live 3 with
   | Dead_code_lib.Deep n ->
     Printf.printf "caught Deep %d\n" n;
     print_string (Printexc.get_backtrace ()));
  Printf.printf "churn %d\n" (Dead_code_lib.churn 20)
