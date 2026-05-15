(* TEST
 readonly_files = "dep.mli dep.ml compile.mli compile.ml";
 directories += "${ocamlsrcdir}/otherlibs/eval";
 flags = "-extension runtime_metaprogramming";
 {
   setup-ocamlopt.opt-build-env;
   module = "dep.mli";
   ocamlopt.opt;
   module = "dep.ml";
   ocamlopt.opt;
   module = "compile.mli";
   ocamlopt.opt;
   module = "compile.ml";
   ocamlopt.opt;
   module = "";
   program = "lib.cmxa";
   flags = "-a";
   all_modules = "dep.cmx compile.cmx";
   ocamlopt.opt;
   program = "${test_build_directory}/main.exe";
   flags = "-extension runtime_metaprogramming -uses-metaprogramming";
   all_modules = "eval.cmxa lib.cmxa main.ml";
   ocamlopt.opt;
   run;
   check-program-output;
 }
*)

(* Regression test: a module referenced only from inside a runtime-metaprogramming
   quote in another unit must not be dropped from a packed [.cmxa] at link time.

   [Dep] is referenced only inside [Compile]'s [<[ ... ]>] quote, never directly.
   Both units are archived into [lib.cmxa]; only [Compile] is reachable from the
   executable's call graph.

   If [Dep] doesn't get linked then the Eval call wil crash with an error like
   "Symbol camlDep__foo_0_1_code refered to by the PLT is unknown". *)
let () = Printf.printf "%d\n" (Compile.get_result ())
