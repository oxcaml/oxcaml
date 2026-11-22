(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 readonly_files = "link_guard_main.c link_guard_front.c link_guard_caller.c \
   link_guard_again.c link_guard_unit.ml";
 guard = "sh ${test_source_directory}/link_guard.sh";
 two_runtimes = "${test_source_directory}/link_guard.two_runtimes.reference";
 start = "${test_source_directory}/link_guard.start.reference";
 cc_link = "${mkexe} -I${runtime}";
 mixed = "link_guard_front.c camlcode.${objext} other.${libext} \
   link_guard_caller.c own.${libext} ${nativecc_libs} -o mixed.exe";
 unexported = "-Wl,--no-export-dynamic";
 linux;
 no-tsan;
 {
   codegen_variant = "+fp -asan";
   variant_available;
   codegen_variant = "-fp -asan";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} other.${libext}";
   script;
   script = "${guard} shared-runtime ${stdlib} other.so";
   script;
   module = "link_guard_unit.ml";
   flags = "-disable-poll-insertion";
   ocamlopt.byte;
   script = "cp link_guard_unit.${objext} other-unit.${objext}";
   script;
   codegen_variant = "+fp -asan";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   script = "cp other-unit.${objext} link_guard_unit.${objext}";
   script;
   module = "";
   script = "${guard} runtime ${stdlib} own.${libext}";
   script;
   script = "${guard} shared-runtime ${stdlib} own.so";
   script;
   flags = "-output-obj";
   program = "camlcode.${objext}";
   ocamlopt.byte;
   output = "refusal.output";
   reference = "${two_runtimes}";
   script = "${guard} refuse camlcode.${objext} ${cc_link} ${mixed}";
   script;
   check-program-output;
   script = "${guard} refuse camlcode.${objext} ${cc_link} \
     -Wl,--gc-sections ${mixed}";
   script;
   check-program-output;
   script = "${cc_link} link_guard_main.c camlcode.${objext} ./own.so \
     ${nativecc_libs} -o own-shared.exe";
   script;
   program = "./own-shared.exe";
   output = "own.output";
   reference = "${test_source_directory}/link_guard_two_runtimes.reference";
   run;
   check-program-output;
   script = "${cc_link} link_guard_again.c camlcode.${objext} ./own.so \
     ${nativecc_libs} -o again.exe";
   script;
   program = "./again.exe";
   arguments = "./other.so";
   output = "again.output";
   reference = "${test_source_directory}/link_guard.again.reference";
   run;
   check-program-output;
   arguments = "";
   script = "${cc_link} link_guard_main.c camlcode.${objext} ./other.so \
     ./own.so ${nativecc_libs} -o two-shared.exe";
   script;
   script = "${guard} refuse-start ./two-shared.exe";
   output = "start.output";
   script;
   reference = "${start}";
   check-program-output;
   flags = "-output-obj -disable-poll-insertion";
   all_modules = "link_guard_unit.cmx";
   program = "stale.${objext}";
   ocamlopt.byte;
   script = "${cc_link} link_guard_main.c stale.${objext} ./own.so \
     ./other.so ${nativecc_libs} -o stale-shared.exe";
   script;
   script = "${guard} refuse-start ./stale-shared.exe";
   output = "stale.output";
   script;
   reference = "${test_source_directory}/link_guard.loaded.reference";
   check-program-output;
   script = "${cc_link} ${unexported} link_guard_main.c stale.${objext} \
     ./own.so other.${libext} ${nativecc_libs} -o stale-archive.exe";
   script;
   script = "${guard} refuse-start ./stale-archive.exe";
   script;
   check-program-output;
   script = "${cc_link} ${unexported} link_guard_front.c camlcode.${objext} \
     other.${libext} link_guard_caller.c ./own.so ${nativecc_libs} \
     -o archive-shared.exe";
   script;
   script = "${guard} stopped ./archive-shared.exe";
   script;
   reference = "${test_source_directory}/link_guard.stopped.reference";
   check-program-output;
   script = "${cc} -nostdlib -r -Wl,-u,caml_detect_cpu_features \
     other.${libext} -o foreign.${objext}";
   script;
   script = "${cc_link} ${unexported} link_guard_main.c camlcode.${objext} \
     foreign.${objext} ./own.so ${nativecc_libs} -o foreign-shared.exe";
   script;
   script = "${guard} refuse-start ./foreign-shared.exe";
   script;
   reference = "${test_source_directory}/link_guard.loaded.reference";
   check-program-output;
 }{
   codegen_variant = "+sc -asan";
   variant_available;
   codegen_variant = "-sc -asan";
   variant_available;
   compiler_directory_suffix = ".sc";
   setup-ocamlopt.byte-build-env;
   script = "${guard} runtime ${stdlib} other.${libext}";
   script;
   script = "${guard} shared-runtime ${stdlib} other.so";
   script;
   module = "link_guard_unit.ml";
   flags = "-disable-poll-insertion";
   ocamlopt.byte;
   script = "cp link_guard_unit.${objext} other-unit.${objext}";
   script;
   codegen_variant = "+sc -asan";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   script = "cp other-unit.${objext} link_guard_unit.${objext}";
   script;
   module = "";
   script = "${guard} runtime ${stdlib} own.${libext}";
   script;
   script = "${guard} shared-runtime ${stdlib} own.so";
   script;
   flags = "-output-obj";
   program = "camlcode.${objext}";
   ocamlopt.byte;
   output = "refusal.output";
   reference = "${two_runtimes}";
   script = "${guard} refuse camlcode.${objext} ${cc_link} ${mixed}";
   script;
   check-program-output;
   script = "${guard} refuse camlcode.${objext} ${cc_link} \
     -Wl,--gc-sections ${mixed}";
   script;
   check-program-output;
   script = "${cc_link} link_guard_main.c camlcode.${objext} ./own.so \
     ${nativecc_libs} -o own-shared.exe";
   script;
   program = "./own-shared.exe";
   output = "own.output";
   reference = "${test_source_directory}/link_guard_two_runtimes.reference";
   run;
   check-program-output;
   script = "${cc_link} link_guard_again.c camlcode.${objext} ./own.so \
     ${nativecc_libs} -o again.exe";
   script;
   program = "./again.exe";
   arguments = "./other.so";
   output = "again.output";
   reference = "${test_source_directory}/link_guard.again.reference";
   run;
   check-program-output;
   arguments = "";
   script = "${cc_link} link_guard_main.c camlcode.${objext} ./other.so \
     ./own.so ${nativecc_libs} -o two-shared.exe";
   script;
   script = "${guard} refuse-start ./two-shared.exe";
   output = "start.output";
   script;
   reference = "${start}";
   check-program-output;
   flags = "-output-obj -disable-poll-insertion";
   all_modules = "link_guard_unit.cmx";
   program = "stale.${objext}";
   ocamlopt.byte;
   script = "${cc_link} link_guard_main.c stale.${objext} ./own.so \
     ./other.so ${nativecc_libs} -o stale-shared.exe";
   script;
   script = "${guard} refuse-start ./stale-shared.exe";
   output = "stale.output";
   script;
   reference = "${test_source_directory}/link_guard.loaded.reference";
   check-program-output;
   script = "${cc_link} ${unexported} link_guard_main.c stale.${objext} \
     ./own.so other.${libext} ${nativecc_libs} -o stale-archive.exe";
   script;
   script = "${guard} refuse-start ./stale-archive.exe";
   script;
   check-program-output;
   script = "${cc_link} ${unexported} link_guard_front.c camlcode.${objext} \
     other.${libext} link_guard_caller.c ./own.so ${nativecc_libs} \
     -o archive-shared.exe";
   script;
   script = "${guard} stopped ./archive-shared.exe";
   script;
   reference = "${test_source_directory}/link_guard.stopped.reference";
   check-program-output;
   script = "${cc} -nostdlib -r -Wl,-u,caml_detect_cpu_features \
     other.${libext} -o foreign.${objext}";
   script;
   script = "${cc_link} ${unexported} link_guard_main.c camlcode.${objext} \
     foreign.${objext} ./own.so ${nativecc_libs} -o foreign-shared.exe";
   script;
   script = "${guard} refuse-start ./foreign-shared.exe";
   script;
   reference = "${test_source_directory}/link_guard.loaded.reference";
   check-program-output;
 }
*)

(* A link that names the runtimes of two codegen variants.

   With static libraries, the linker takes each object file of the runtime
   from the first library that has what is undefined when it reads it. In
   the first link below it reads the runtime of the other variant when the
   OCaml code is there but nothing calls caml_startup yet, and the runtime
   of the code's variant after the C file that does: the stack switching
   code would come from the first and the startup code from the second.
   Every object file of a runtime refers to the guard of its own variant,
   which is defined in an object file of its own, next to a symbol that
   has the same name in every variant: the link takes that object file
   from both libraries and fails on two definitions of the symbol, also
   when the linker discards the sections that nothing refers to.

   With shared libraries nothing fails at link time, since each may define
   what the other defines: the program starts in the first, and the second
   only defines the guard. The runtime that starts the program checks that
   the guard that the startup object refers to is its own, and stops. The
   same program linked with the shared runtime of its variant alone runs.
   It also runs when its C code loads the other runtime after the first
   call of caml_startup and calls it again: the runtime is checked when it
   starts, once, and the lookups of that check leave no error with the
   dynamic linker for the C code to find.

   The next program is one whose startup object agrees with the first
   runtime, and one of whose units has the object file of the other
   variant: alone, the first runtime leaves the guard of that object file
   undefined, and the second defines it. The runtime that starts the
   program also checks that no guard of another variant is defined where
   it is loaded, and stops. (That unit is compiled without polls for both
   variants: an object file without polls for a unit that claims them is
   refused by the linker, for that.)

   The last three programs get what the shared runtime of their variant
   lacks from the static library of the other one, and do not export
   their symbols, unlike a program that ocamlopt links: the guard of the
   stale object file in one, some of the runtime's functions in the
   others, with the guard that their object files refer to. That guard is
   then in the executable, where the shared runtime only sees it because
   it refers to the guards of all the variants. All are stopped when they
   start; which of the two checks stops the second depends on where the
   linker takes caml_startup from. The third has one function of the other
   runtime, linked beforehand into an object file with what it needs of
   that library: its own runtime starts it with every linker, and must find
   the guard of the other.

   The variants have no AddressSanitizer: a C link that this test writes
   would need the sanitizer's own flags and runtime, which is not what it
   is about. *)

let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)

let () = Printf.printf "sum: %d\n%!" (sum 100 0)
