(* TEST
 readonly_files = "a.mli a.ml b.mli b.ml";
 directories += "${ocamlsrcdir}/otherlibs/eval";
 flags = "-extension runtime_metaprogramming";
 {
   setup-ocamlopt.opt-build-env;
   module = "a.mli";
   ocamlopt.opt;
   module = "a.ml";
   ocamlopt.opt;
   module = "b.mli";
   ocamlopt.opt;
   module = "b.ml";
   ocamlopt.opt;
   module = "";
   program = "lib.cmxa";
   flags = "-a";
   all_modules = "a.cmx b.cmx";
   ocamlopt.opt;
   program = "${test_build_directory}/main.exe";
   flags = "-extension runtime_metaprogramming -uses-metaprogramming";
   all_modules = "eval.cmxa lib.cmxa available.ml";
   ocamlopt.opt;
   run;
   check-program-output;
 }
*)

#syntax quotations on

let () =
  let e = Obj.magic_many <[ B.x / A.x ]> in
  print_string (Quote.string_of_expr e);
  print_newline ();
  print_int (Eval.eval e);
  print_newline ()
