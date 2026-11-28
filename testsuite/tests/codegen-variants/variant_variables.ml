(* TEST
 readonly_files = "variant_variables_.c variant_variables.sh";
 mkexe += " -DFROM_TEST";
 cflags += " -DFROM_TEST";
 output = "variant_variables.output";
 {
 (* A test that assigns stdlib keeps its value, also when that value is a
    library tree of the compiler under test and a variant is selected. *)
   codegen_variant = "fp";
   variant_available;
   stdlib = "${ocamlsrcdir}/stdlib";
   script = "sh ${test_source_directory}/variant_variables.sh own-stdlib ${ocamlsrcdir}/stdlib";
   script;
 }{
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "sc";
   variant_available;
   script = "sh variant_variables.sh native";
   script;
   reference = "${test_source_directory}/variant_variables.sc.reference";
   check-program-output;
 }{
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "fp";
   variant_available;
   script = "sh variant_variables.sh native";
   script;
   reference = "${test_source_directory}/variant_variables.fp.reference";
   check-program-output;
 }{
   compiler_directory_suffix = ".ccopt";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "sc";
   variant_available;
   all_modules = "variant_variables_.c variant_variables.ml";
   ocamlopt_flags = "-ccopt '${cflags}'";
   ocamlopt.byte;
   run;
   reference = "${test_source_directory}/variant_variables.ccopt.reference";
   check-program-output;
 }{
 (* The test takes the macro of the variant's axis back.  The variant's
    part of cflags comes after what the test adds, so the stub sees the
    macro all the same.  The flags of a compilation are read as a command
    line is: the two programs print the same, and the script then checks
    what they print. *)
   compiler_directory_suffix = ".order";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "sc";
   variant_available;
   cflags += " -USTACK_CHECKS_ENABLED";
   commandline = "-nostdlib -I ${stdlib} -ccopt '${cflags}' \
     variant_variables_.c variant_variables.ml -o ${program}";
   ocamlopt.byte;
   output = "${test_build_directory}/commandline.output";
   run;
   commandline = "";
   all_modules = "variant_variables_.c variant_variables.ml";
   ocamlopt_flags = "-ccopt '${cflags}'";
   ocamlopt.byte;
   output = "${test_build_directory}/flags.output";
   run;
   reference = "${test_build_directory}/commandline.output";
   check-program-output;
   output = "${test_build_directory}/order.output";
   script = "sh variant_variables.sh order";
   script;
 }{
   compiler_directory_suffix = ".bytecode";
   codegen_variant = "sc";
   variant_available;
   setup-ocamlc.byte-build-env;
   script = "sh variant_variables.sh bytecode";
   script;
 }
*)

(* The variables that a script builds its commands from describe the
   variant in effect when the script runs, wherever the test selected it:
   here after the build environment was set up, under whatever variant the
   run uses.  stdlib is the variant's library tree, and cflags and mkexe
   end with its C flags, after what the test itself added to them.  The
   flags of a native compilation get them in the same form.
   In a block set up for a bytecode compiler, cflags and mkexe are for the
   bytecode runtime and carry nothing of the variant, while stdlib still
   is the variant's tree, for the native compilers that such a block may
   start. *)

external variant_macros : unit -> string = "variant_macros"

let () = Printf.printf "cflags:%s\n" (variant_macros ())
