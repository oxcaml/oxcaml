(* TEST
 readonly_files = "plugin.ml";
 include dynlink;
 libraries = "";
 shared-libraries;
 {
   setup-ocamlc.byte-build-env;
   flags = "-no-crc";
   module = "plugin.ml";
   ocamlc.byte;
   flags = "";
   module = "dynlink_gap.ml";
   ocamlc.byte;
   module = "";
   libraries = "dynlink";
   program = "${test_build_directory}/dynlink_gap.byte";
   all_modules = "dynlink_gap.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   native-dynlink;
   setup-ocamlopt.byte-build-env;
   flags = "-no-crc";
   module = "plugin.ml";
   ocamlopt.byte;
   module = "";
   flags = "-no-crc -shared";
   program = "plugin.cmxs";
   all_modules = "plugin.cmx";
   ocamlopt.byte;
   flags = "";
   module = "dynlink_gap.ml";
   ocamlopt.byte;
   module = "";
   libraries = "dynlink";
   program = "${test_build_directory}/dynlink_gap.exe";
   all_modules = "dynlink_gap.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A host program built with interface CRCs loads a plugin built with
   [-no-crc]. Nothing checks the plugin against the host: its import table
   has no CRCs, and [Dynlink] has no way of knowing that it was built without
   them. *)

(* CR-soon zqian: [Dynlink] should refuse a [-no-crc] plugin unless the host
   was itself built with [-no-crc] (or opts in explicitly). The native
   toplevel's [#load] of a prebuilt [.cmxs] has the same gap. Once fixed, this
   test should expect an error instead of the plugin's output. *)

let () =
  let plugin = if Dynlink.is_native then "plugin.cmxs" else "plugin.cmo" in
  match Dynlink.loadfile plugin with
  | () -> print_endline "host: Dynlink accepted a -no-crc plugin"
  | exception Dynlink.Error e -> print_endline (Dynlink.error_message e)
