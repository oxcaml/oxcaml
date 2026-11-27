(* TEST
 readonly_files = "dep.ml uses_stub.ml";
 subdirectories = "lib other";
 shared-libraries;
 setup-ocaml-build-env;
 module = "dep.ml";
 ocamlc.byte;
 module = "uses_stub.ml";
 ocamlc.byte;
 cwd = "lib";
 cd;
 module = "dep.ml";
 ocamlc.byte;
 module = "good.ml";
 ocamlc.byte;
 module = "bad.ml";
 ocamlc.byte;
 module = "stubs.c";
 ocamlc.byte;
 module = "";
 program = "refused";
 modules = "stubs.${objext}";
 ocamlmklib;
 modules = "good.cmo bad.cmo";
 ocamlmklib;
 modules = "";
 cwd = "../other";
 cd;
 module = "good.ml";
 ocamlc.byte;
 module = "";
 cwd = "..";
 cd;
 ocaml;
 check-ocaml-output;
*)

#load "dep.cmo";;

(* The library is refused: its unit [Bad] was compiled against another
   interface of [Dep]. Nothing of it is loaded: its unit [Good], which comes
   first, is not run... *)
#directory "lib";;
#load "refused.cma";;

(* ... its shared library is not opened, so its primitive is missing... *)
#load "uses_stub.cmo";;

(* ... and the interface of [Good] is not recorded, so another one loads. *)
#load "other/good.cmo";;
