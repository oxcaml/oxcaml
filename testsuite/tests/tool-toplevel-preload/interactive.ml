(* TEST
 readonly_files = "dep.ml good_user.ml";
 subdirectories = "other";
 setup-ocaml-build-env;
 module = "dep.ml";
 ocamlc.byte;
 module = "good_user.ml";
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
 ocaml;
 check-ocaml-output;
*)

(* [dep.cmo] was loaded from the command line. A unit compiled against
   another interface of [Dep] must be refused, and one compiled against this
   interface must then still be accepted. *)
#load "other/bad_user.cmo";;
#load "good_user.cmo";;

(* So must a phrase typed against the other interface. *)
#directory "other";;
ignore (Dep.v : string);;
