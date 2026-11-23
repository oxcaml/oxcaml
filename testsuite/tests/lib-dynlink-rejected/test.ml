(* TEST
 include dynlink;
 readonly_files = "api.ml plugin.ml consumer.ml";
 subdirectories = "rejected";
 libraries = "";
 native-dynlink;
 setup-ocamlopt.byte-build-env;
 module = "rejected/api.ml";
 ocamlopt.byte;
 program = "rejected/plugin.cmxs";
 flags = "-I rejected -shared";
 module = "";
 all_modules = "rejected/plugin.ml";
 ocamlopt.byte;
 flags = "";
 module = "api.ml";
 ocamlopt.byte;
 program = "plugin.cmxs";
 flags = "-shared";
 module = "";
 all_modules = "plugin.ml";
 ocamlopt.byte;
 program = "consumer.cmxs";
 all_modules = "consumer.ml";
 ocamlopt.byte;
 flags = "";
 module = "test.ml";
 ocamlopt.byte;
 program = "${test_build_directory}/test.exe";
 libraries = "dynlink";
 module = "";
 all_modules = "api.cmx test.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

let load file =
  match Dynlink.loadfile file with
  | () -> Printf.printf "%s: loaded\n%!" file
  | exception Dynlink.Error error ->
    Printf.printf "%s: %s\n%!" file (Dynlink.error_message error)

let () =
  ignore (Sys.opaque_identity Api.version);
  load "rejected/plugin.cmxs";
  load "plugin.cmxs";
  load "consumer.cmxs"
