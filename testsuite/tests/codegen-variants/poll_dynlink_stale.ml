(* TEST
 codegen_variant = "poll";
 variant_available;
 codegen_variant = "base";
 variant_available;
 shared-libraries;
 native-dynlink;
 readonly_files = "poll_dynlink_stale_parent.ml poll_dynlink_stale_child.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "poll";
 module = "poll_dynlink_stale_parent.ml";
 ocamlopt.byte;
 module = "poll_dynlink_stale_child.ml";
 ocamlopt.byte;
 module = "";
 flags = "-shared";
 program = "poll_dynlink_stale_child.cmxs";
 all_modules = "poll_dynlink_stale_child.cmx";
 ocamlopt.byte;
 flags = "-linkall";
 module = "poll_dynlink_stale_parent.ml";
 ocamlopt.byte;
 module = "";
 flags = "-shared";
 program = "poll_dynlink_stale_parent.cmxs";
 all_modules = "poll_dynlink_stale_parent.cmx";
 ocamlopt.byte;
 codegen_variant = "base";
 flags = "-I +dynlink";
 libraries = "dynlink";
 program = "${test_build_directory}/poll_dynlink_stale.exe";
 all_modules = "poll_dynlink_stale.ml";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* A plugin that was compiled against an implementation other than the
   one loaded is refused as that, an implementation mismatch on the unit
   in question, whatever the poll-insertion settings of the plugin and of
   the program are: here both plugins have polls, the program has none,
   and the first plugin was compiled again (with -linkall, which changes
   its implementation CRC) after the second was compiled against it. *)

let load file =
  match Dynlink.loadfile file with
  | () -> Printf.printf "%s: loaded\n" file
  | exception Dynlink.Error e ->
    Printf.printf "%s: %s\n" file (Dynlink.error_message e)

let () =
  load "poll_dynlink_stale_parent.cmxs";
  load "poll_dynlink_stale_child.cmxs"
