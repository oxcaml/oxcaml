(* TEST
 readonly_files = "dep.ml script.ml";
 subdirectories = "other";
 setup-ocamlc.byte-build-env;
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
 include ocamlcommon;
 include ocamlbytecomp;
 include ocamltoplevel;
 flags = "-linkall";
 program = "./custom.exe";
 all_modules = "dep.cmo custom.ml";
 ocamlc.byte;
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 arguments = "script.ml";
 exit_status = "125";
 run;
 check-program-output;
*)

(* A toplevel with [dep.cmo] linked in, as [ocamlmktop] makes them. It runs
   [script.ml]. *)
let () = exit (Topmain.main ())
