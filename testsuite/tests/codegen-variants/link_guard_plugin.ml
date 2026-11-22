(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 guard = "sh ${test_source_directory}/link_guard.sh";
 undefined = "${test_source_directory}/link_guard.undefined.reference";
 unit = "link_guard_unit";
 raw = "${ocamlrun} ${ocamlopt_byte} -shared";
 readonly_files = "${unit}.ml link_guard_printf_unit.ml";
 shared-libraries;
 native-dynlink;
 codegen_variant = "+fp -asan";
 variant_available;
 codegen_variant = "-fp -asan";
 variant_available;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "${unit}.ml";
 program = "same.cmxs";
 ocamlopt.byte;
 script = "${raw} ${unit}.${objext} -o raw_same.cmxs";
 script;
 codegen_variant = "+fp -asan";
 program = "other.cmxs";
 ocamlopt.byte;
 script = "cp ${unit}.${objext} other-unit.${objext}";
 script;
 script = "${raw} other-unit.${objext} -o raw_other.cmxs";
 script;
 flags = "-shared -internal-assembler";
 program = "other_internal.cmxs";
 ocamlopt.byte;
 flags = "-shared";
 all_modules = "link_guard_printf_unit.ml";
 program = "other_printf.cmxs";
 ocamlopt.byte;
 codegen_variant = "-fp -asan";
 flags = "-disable-poll-insertion";
 module = "${unit}.ml";
 ocamlopt.byte;
 script = "cp other-unit.${objext} ${unit}.${objext}";
 script;
 module = "";
 all_modules = "${unit}.cmx";
 flags = "-shared";
 program = "mixed.cmxs";
 ocamlopt.byte;
 flags = "";
 program = "mixed.exe";
 compiler_output = "compiler-output.raw";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 script = "${guard} refusal other-unit.${objext} compiler-output.raw";
 output = "refusal.output";
 script;
 reference = "${undefined}";
 check-program-output;
 ocamlopt_byte_exit_status = "0";
 flags = "-I +dynlink";
 libraries = "dynlink";
 all_modules = "link_guard_plugin.ml";
 program = "${test_build_directory}/host.exe";
 ocamlopt.byte;
 arguments = "same.cmxs raw_same.cmxs other.cmxs other_printf.cmxs";
 output = "host.output";
 run;
 reference = "${test_source_directory}/link_guard_plugin.reference";
 check-program-output;
 script = "${guard} exports other.cmxs other_internal.cmxs";
 output = "exports.output";
 script;
 reference = "${test_source_directory}/link_guard_plugin.exports.reference";
 check-program-output;
 output = "refusal.output";
 reference = "${undefined}";
 script = "${guard} refuse other-unit.${objext} ${program} \
   other_internal.cmxs mixed.cmxs";
 script;
 check-program-output;
 script = "${guard} refuse other-unit.${objext} ${program} raw_other.cmxs";
 script;
 check-program-output;
*)

(* The link-time guard of the codegen variant and plugins.

   The object files of a plugin refer to the guard of the frame-pointer,
   stack-check and AddressSanitizer settings that they were compiled for,
   like all generated code, and the startup code of the plugin defines the
   guard of the settings that the plugin was linked for, for the plugin
   only. So a plugin whose units agree with it can be opened by any
   program, and Dynlink, which has to open a plugin before it can read what
   it was compiled for, compares that with the program and explains a
   refusal: the plugin "other" below, compiled for another variant than
   the program.

   What Dynlink cannot see is an object file that does not match the unit
   information next to it. Here the object file of a unit compiled for the
   variant of the program is replaced by the one compiled for the other
   variant. The plugin "mixed", linked around it, cannot be opened: nothing
   defines the guard of that object file. Only a plugin defines a guard of
   its own: the program linked from the same files does not link. (That
   unit is compiled without polls, so that the other tie between unit
   information and object file, the one for polls of poll_object.ml, has
   nothing to say about the pair.)

   The plugin "other_printf" is "other" with a use of the standard library.
   It can be opened as well, for Dynlink to refuse it in the same way: the
   standard libraries of two variants define the same symbols.

   A refused plugin stays in the program's address space. It must not
   export its guard, which would define it for the plugins opened later:
   "mixed" is opened after "other_internal", which is "other" with its
   object files written by the compiler itself (-internal-assembler)
   instead of the assembler, and is still refused.

   A plugin made from an object file alone has no unit information for
   Dynlink to compare with the program, so it defines no guard of its own:
   it refers to the guard, like the object file in it, and can open only where
   that guard is visible. "raw_same" is loaded (and not run, for want of that
   information); "raw_other" is not opened here, where nothing defines it.

   The two variants differ by frame pointers and have no AddressSanitizer:
   instrumented code needs the AddressSanitizer runtime, so that a plugin
   with it cannot be opened by a program without it, whatever else they
   agree on. *)

let load plugin =
  match Dynlink.loadfile plugin with
  | () -> Printf.printf "%s: loaded\n%!" plugin
  | exception Dynlink.Error (Inconsistent_codegen_variant _) ->
    Printf.printf "%s: refused by Dynlink, as compiled for another variant\n%!"
      plugin
  | exception Dynlink.Error error ->
    Printf.printf "%s: %s\n%!" plugin (Dynlink.error_message error);
    exit 2

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    load Sys.argv.(i)
  done
