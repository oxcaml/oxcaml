(* TEST
 readonly_files = "dep.ml";
 subdirectories = "other";
 setup-ocaml-build-env;
 module = "dep.ml";
 ocamlc.byte;
 cwd = "other";
 cd;
 module = "dep.ml";
 ocamlc.byte;
 module = "bad_user.ml";
 ocamlc.byte;
 module = "";
 cwd = "..";
 cd;
 binary_modules = "dep";
 ocaml_script_as_argument = "true";
 ocaml_exit_status = "125";
 ocaml;
 check-ocaml-output;
*)

(* As in [interactive.ml], for a script. [custom.ml] runs this script too. *)
#load "other/bad_user.cmo";;
