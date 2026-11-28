(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 readonly_files = "link_guard_main.c";
 guard = "sh ${test_source_directory}/link_guard.sh";
 undefined = "${test_source_directory}/link_guard.undefined.reference";
 two_runtimes = "${test_source_directory}/link_guard.two_runtimes.reference";
 c_link = "${mkexe} -I${runtime} link_guard_main.c camlcode.${objext}";
 ocaml_link = "${ocamlrun} ${ocamlopt_byte} link_guard_output_obj.ml";
 no-tsan;
 {
   codegen_variant = "+fp";
   variant_available;
   codegen_variant = "-fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} other.${libext}";
   script;
   codegen_variant = "+fp";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} own.${libext}";
   script;
   flags = "-output-obj";
   program = "camlcode.${objext}";
   ocamlopt.byte;
   script = "${c_link} own.${libext} ${nativecc_libs} -o own.exe";
   script;
   program = "./own.exe";
   output = "own.output";
   run;
   check-program-output;
   output = "refusal.output";
   reference = "${undefined}";
   script = "${guard} refuse camlcode.${objext} \
     ${c_link} other.${libext} ${nativecc_libs} -o other.exe";
   script;
   check-program-output;
   script = "${guard} refuse camlcode.${objext} \
     ${ocaml_link} -without-runtime other.${libext} -o other.exe";
   script;
   check-program-output;
   reference = "${two_runtimes}";
   script = "${guard} refuse camlcode.${objext} \
     ${ocaml_link} other.${libext} -o other.exe";
   script;
   check-program-output;
 }{
   codegen_variant = "+sc";
   variant_available;
   codegen_variant = "-sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} other.${libext}";
   script;
   codegen_variant = "+sc";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} own.${libext}";
   script;
   flags = "-output-obj";
   program = "camlcode.${objext}";
   ocamlopt.byte;
   script = "${c_link} own.${libext} ${nativecc_libs} -o own.exe";
   script;
   program = "./own.exe";
   output = "own.output";
   run;
   check-program-output;
   output = "refusal.output";
   reference = "${undefined}";
   script = "${guard} refuse camlcode.${objext} \
     ${c_link} other.${libext} ${nativecc_libs} -o other.exe";
   script;
   check-program-output;
   script = "${guard} refuse camlcode.${objext} \
     ${ocaml_link} -without-runtime other.${libext} -o other.exe";
   script;
   check-program-output;
   reference = "${two_runtimes}";
   script = "${guard} refuse camlcode.${objext} \
     ${ocaml_link} other.${libext} -o other.exe";
   script;
   check-program-output;
 }{
   codegen_variant = "-sc";
   variant_available;
   codegen_variant = "+sc";
   variant_available;
   compiler_directory_suffix = ".no-sc";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} other.${libext}";
   script;
   codegen_variant = "-sc";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} own.${libext}";
   script;
   flags = "-output-obj";
   program = "camlcode.${objext}";
   ocamlopt.byte;
   script = "${c_link} own.${libext} ${nativecc_libs} -o own.exe";
   script;
   program = "./own.exe";
   output = "own.output";
   run;
   check-program-output;
   output = "refusal.output";
   reference = "${undefined}";
   script = "${guard} refuse camlcode.${objext} \
     ${c_link} other.${libext} ${nativecc_libs} -o other.exe";
   script;
   check-program-output;
 }
*)

(* A runtime defines one symbol named after the frame-pointer, stack-check
   and AddressSanitizer settings that it was built for, and every object
   file that ocamlopt generates refers to the symbol of the settings that it
   was compiled for. So linking code with a runtime of another codegen
   variant fails in the linker, on an undefined reference to
   caml_runtime_codegen_variant_is_<the variant of the code>, where nothing
   else can tell: the compiler does not see a link that the user writes
   after -output-obj, and it passes an archive that it is given by name on
   to the linker.

   Each block compiles this file with -output-obj for one variant, links it
   with the runtime of that variant from a C main program (it runs), and
   then with the runtime of another variant (refused). The first two blocks
   also give that other runtime to ocamlopt by name: without the runtime of
   the variant (-without-runtime), the guard is undefined; with it, the
   guard is found in the second runtime, which then collides with the
   first on the symbol that every runtime defines. *)

let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)

let () = Printf.printf "sum: %d\n%!" (sum 100 0)
