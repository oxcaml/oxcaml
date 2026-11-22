(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 readonly_files = "link_guard_unit.ml";
 guard = "sh ${test_source_directory}/link_guard.sh";
 generate = "${ocamlsrcdir}/generate_cached_generic_functions.exe";
 cached = "-use-cached-generic-functions -cached-generic-functions-path";
 gc = "-function-sections -ccopt -Wl,--gc-sections,--no-export-dynamic";
 undefined = "${test_source_directory}/link_guard.undefined.reference";
 two_runtimes = "${test_source_directory}/link_guard.two_runtimes.reference";
 linux;
 function_sections;
 no-tsan;
 codegen_variant = "-fp";
 variant_available;
 codegen_variant = "+fp";
 variant_available;
 setup-ocamlopt.byte-build-env;
 script = "${generate} cached";
 script;
 script = "${guard} runtime ${stdlib} other-runtime.${libext}";
 script;
 flags = "${gc} ${cached} cached.${libext}";
 ocamlopt.byte;
 run;
 check-program-output;
 module = "link_guard_unit.ml";
 flags = "-function-sections -disable-poll-insertion";
 ocamlopt.byte;
 script = "cp link_guard_unit.${objext} other-unit.${objext}";
 script;
 script = "${guard} sectioned other-unit.${objext}";
 output = "sectioned.output";
 script;
 reference =
   "${test_source_directory}/link_guard_gc_sections.sectioned.reference";
 check-program-output;
 codegen_variant = "-fp";
 ocamlopt.byte;
 script = "cp other-unit.${objext} link_guard_unit.${objext}";
 script;
 module = "";
 ocamlopt_byte_exit_status = "2";
 output = "refusal.output";
 reference = "${undefined}";
 flags = "${gc} ${cached} cached.${libext}";
 program = "other-archive.exe";
 compiler_output = "other-archive.raw";
 ocamlopt.byte;
 script = "${guard} refusal cached.${libext} other-archive.raw";
 script;
 check-program-output;
 flags = "${gc}";
 all_modules = "link_guard_unit.cmx";
 program = "other-object.exe";
 compiler_output = "other-object.raw";
 ocamlopt.byte;
 script = "${guard} refusal other-unit.${objext} other-object.raw";
 script;
 check-program-output;
 flags = "${gc} -without-runtime other-runtime.${libext}";
 all_modules = "link_guard_gc_sections.ml";
 program = "other-runtime.exe";
 compiler_output = "other-runtime.raw";
 ocamlopt.byte;
 script = "${guard} refusal link_guard_gc_sections.${objext} \
   other-runtime.raw";
 script;
 check-program-output;
 flags = "${gc} other-runtime.${libext}";
 program = "two-runtimes.exe";
 compiler_output = "two-runtimes.raw";
 ocamlopt.byte;
 script = "${guard} refusal link_guard_gc_sections.${objext} \
   two-runtimes.raw";
 script;
 reference = "${two_runtimes}";
 check-program-output;
 flags = "-shared ${gc} -ccopt -Wl,--strip-all";
 all_modules = "link_guard_unit.cmx";
 program = "mixed.cmxs";
 compiler_output = "mixed.raw";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "${guard} needs mixed.cmxs other-unit.${objext}";
 output = "plugin.output";
 script;
 reference = "${test_source_directory}/link_guard_gc_sections.plugin.reference";
 check-program-output;
*)

(* A linker that discards the sections that nothing refers to
   (--gc-sections, here with one section per function and without the
   export of every symbol that keeps most sections of an OCaml program)
   must not discard the reference to the link-time guard of an object file
   whose code it keeps. The reference is at the start of the data of the
   object file, and the startup code refers to the data of every object
   file whose code it registers: the units of the program, and the members
   of the archive of cached generic functions that the program uses. The
   object file of a unit that the links below take in place of another is
   itself compiled with one section per function.

   The program links with the archive of its variant and runs. For the
   other variant, every link that the other tests of the guard show to be
   refused is refused with such a linker too: with that archive, on the
   guard of its members; for a unit whose object file was compiled for the
   first variant, on the guard of that object file; with the runtime of the
   first variant in place of its own, on the guard of the program; and with
   both runtimes, on the symbol they share. A plugin that contains the object
   file of the first variant still needs that variant's guard from outside
   it when it is loaded; the plugin is stripped, so that the need is read
   where the dynamic linker reads it. (The unit is compiled without polls
   for both variants, so that the tie between polls and object files, of
   poll_object.ml, has nothing to say about the pairs.) *)

let apply3 f x y z = (Sys.opaque_identity f) x y z

let add3 x y z = x + y + z

let partial = Sys.opaque_identity (apply3 (fun a b c d -> a + b + c + d) 1 2 3)

let () = Printf.printf "%d %d\n%!" (apply3 add3 1 2 3) (partial 4)
