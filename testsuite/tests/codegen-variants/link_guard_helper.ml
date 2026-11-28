(* TEST
 readonly_files = "link_guard_unit.ml";
 guard = "sh ${test_source_directory}/link_guard.sh";
 {
   setup-ocamlopt.byte-build-env;
   module = "link_guard_unit.ml";
   ocamlopt.byte;
   script = "${guard} self-test link_guard_unit.${objext}";
   output = "self-test.output";
   script;
   check-program-output;
   module = "";
   flags = "-output-complete-obj";
   program = "complete.${objext}";
   ocamlopt.byte;
   script = "${guard} self-test complete.${objext}";
   script;
   check-program-output;
 }{
   codegen_variant = "+fp";
   variant_available;
   codegen_variant = "-fp";
   variant_available;
   compiler_directory_suffix = ".two";
   setup-ocamlopt.byte-build-env;
   module = "link_guard_unit.ml";
   ocamlopt.byte;
   script = "cp link_guard_unit.${objext} first.${objext}";
   script;
   codegen_variant = "+fp";
   ocamlopt.byte;
   script = "ar rc two.${libext} first.${objext} link_guard_unit.${objext}";
   script;
   script = "${guard} self-test-two-guards two.${libext}";
   output = "two-guards.output";
   script;
   reference = "${test_source_directory}/link_guard_helper.two.reference";
   check-program-output;
 }
*)

(* The tests of the link-time guard decide that a link was refused for the
   right reason from what the linker printed: it must name the guard that
   the object file refers to. Linkers and systems differ in how they print
   an undefined symbol, and the tests that need two codegen variants do not
   run on every system. The first block runs on every system, with an
   object file of the base variant: it checks the decision itself,
   against the diagnostics of the linkers of each supported system,
   including the leading underscore of the symbols of some of them,
   against symbols that only contain the name of the guard, and against
   two definitions of a symbol, which are two runtimes only for the symbol
   that every runtime defines. It does the same with an object file made
   with -output-complete-obj, which contains the runtime: the references of
   the startup code to the guards of the other variants, which need not be
   satisfied, are not guards that the file needs. The second block needs
   two variants: a file that refers to two guards has no right reason to be
   refused for. *)

let () = print_endline "not run"
