(* TEST
 codegen_variant = "poll";
 variant_available;
 shared-libraries;
 native-dynlink;
 readonly_files = "poll_dynlink_plugin.ml";
 {
   compiler_directory_suffix = ".reject";
   reference = "${test_source_directory}/poll_dynlink.reject.reference";
   setup-ocamlopt.byte-build-env;
   flags = "-shared -disable-poll-insertion";
   program = "poll_dynlink_plugin.cmxs";
   all_modules = "poll_dynlink_plugin.ml";
   ocamlopt.byte;
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/poll_dynlink.exe";
   all_modules = "poll_dynlink.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".d1";
   reference = "${test_source_directory}/poll_dynlink.d1.reference";
   setup-ocamlopt.byte-build-env;
   flags = "-shared -disable-poll-insertion";
   program = "poll_dynlink_plugin.cmxs";
   all_modules = "poll_dynlink_plugin.ml";
   ocamlopt.byte;
   flags = "-I +dynlink";
   libraries = "dynlink";
   program = "${test_build_directory}/poll_dynlink.exe";
   all_modules = "poll_dynlink.ml";
   ocamlopt.byte;
   ocamlrunparam = "d=1";
   run;
   check-program-output;
 }
*)

(* A fully polled host runs with max_domains > 1 by default, so loading a
   plugin containing a unit compiled without poll insertion must be
   rejected (it would falsify the fully-polled property the domain cap
   was derived from).  The same host run with OCAMLRUNPARAM=d=1 (via the
   exported ocamlrunparam TSL variable) caps the runtime at one domain,
   which makes the non-polled plugin harmless and the load is
   permitted.  The rejection message
   embeds the plugin's absolute path, so the host prints a stable
   projection of the error instead of the full message. *)

let () =
  match Dynlink.loadfile "poll_dynlink_plugin.cmxs" with
  | () -> print_endline "plugin loaded"
  | exception Dynlink.Error (Library_not_fully_polled { unit_name; _ }) ->
    Printf.printf "load rejected (not fully polled): %s\n" unit_name
  | exception Dynlink.Error e ->
    Printf.printf "unexpected dynlink error: %s\n" (Dynlink.error_message e)
