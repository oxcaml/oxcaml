(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 readonly_files = "link_guard_unit.ml";
 guard = "sh ${test_source_directory}/link_guard.sh";
 ocamlrunparam = "v=0";
 {
   codegen_variant = "+fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   flags = "-runtime-variant d";
   program = "${test_build_directory}/debug.exe";
   ocamlopt.byte;
   run;
   check-program-output;
   flags = "-runtime-variant _pic";
   program = "${test_build_directory}/pic.exe";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   codegen_variant = "+sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   flags = "-runtime-variant d";
   program = "${test_build_directory}/debug.exe";
   ocamlopt.byte;
   run;
   check-program-output;
   flags = "-runtime-variant _pic";
   program = "${test_build_directory}/pic.exe";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   codegen_variant = "+sc";
   variant_available;
   codegen_variant = "-sc";
   variant_available;
   codegen_variant = "";
   compiler_directory_suffix = ".suffix";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime-suffix ${ocamlsrcdir}/stdlib \
     ${ocamlrun} ${ocamlopt_byte}";
   output = "suffix.output";
   script;
   reference =
     "${test_source_directory}/link_guard_runtime_variant.suffix.reference";
   check-program-output;
 }
*)

(* Every runtime of a codegen variant defines the link-time guard of the
   variant: the debug runtime and the one compiled as position-independent
   code (-runtime-variant d and _pic) link and run under a variant, like
   the plain one.

   The value of -runtime-variant is part of the name of the archive that
   the linker is given, and the runtimes of the other variants are named
   after their axes: with the library tree of another variant on the search
   path, -runtime-variant _fp or _sc names that variant's runtime for code
   of the base variant. The last block tries this with the runtime of
   every other variant that has a name of its own: each link is refused,
   since nothing defines the guard of the code. *)

let () = print_endline "the program runs"
