(* TEST
 codegen_variant = "fp";
 variant_available;
 codegen_variant = "base";
 variant_available;
 shared-libraries;
 native-dynlink;
 readonly_files = "dynlink_variants_plugin.ml";
 {
   compiler_directory_suffix = ".fp-plugin";
   reference = "${test_source_directory}/dynlink_variants.fp-plugin.reference";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "fp";
   flags = "-shared";
   program = "dynlink_variants_plugin.cmxs";
   all_modules = "dynlink_variants_plugin.ml";
   ocamlopt.byte;
   codegen_variant = "base";
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/dynlink_variants.exe";
   all_modules = "dynlink_variants.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".fp-host";
   reference = "${test_source_directory}/dynlink_variants.fp-host.reference";
   setup-ocamlopt.byte-build-env;
   codegen_variant = "base";
   flags = "-shared";
   program = "dynlink_variants_plugin.cmxs";
   all_modules = "dynlink_variants_plugin.ml";
   ocamlopt.byte;
   codegen_variant = "fp";
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/dynlink_variants.exe";
   all_modules = "dynlink_variants.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A plugin compiled for another codegen variant than the program is
   refused by Dynlink itself, with the fingerprint of each. For that the
   dynamic linker has to open the plugin first, which it only does when
   every symbol that the plugin refers to exists in the program: the
   plugin uses Printf, whose code has symbols named after the path of
   its source, so the standard library must have the same symbols in
   every variant's tree. *)

let () =
  match Dynlink.loadfile "dynlink_variants_plugin.cmxs" with
  | () -> print_endline "plugin loaded"
  | exception
      Dynlink.Error
        (Inconsistent_codegen_variant
           { unit_name; unit_fingerprint; host_fingerprint; _ }) ->
    Printf.printf "refused: %s was compiled for [%s], the program for [%s]\n"
      unit_name unit_fingerprint host_fingerprint
  | exception Dynlink.Error e ->
    Printf.printf "unexpected dynlink error: %s\n" (Dynlink.error_message e)
