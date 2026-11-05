(* TEST
 readonly_files = "\
   dep.mli dep.ml unrepresentable_unit.mli unrepresentable_unit.ml \
 ";
 directories += "${ocamlsrcdir}/otherlibs/eval";
 flags = "-extension runtime_metaprogramming -no-alias-deps";
 {
   setup-ocamlopt.opt-build-env;
   module = "dep.mli";
   ocamlopt.opt;
   module = "dep.ml";
   ocamlopt.opt;
   module = "unrepresentable_unit.mli";
   ocamlopt.opt;
   module = "unrepresentable_unit.ml";
   ocamlopt.opt;
   module = "";
   program = "lib.cmxa";
   flags = "-a";
   all_modules = "dep.cmx unrepresentable_unit.cmx";
   ocamlopt.opt;
   program = "${test_build_directory}/unrepresentable_quote.exe";
   flags = "\
     -extension runtime_metaprogramming -syntax-quotations -no-alias-deps \
     -uses-metaprogramming \
   ";
   all_modules = "eval.cmxa lib.cmxa unrepresentable_quote.ml";
   compiler_output = "unrepresentable_quote.output";
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

(* When a quote refers to another compilation unit, we need to mark it as
   required for the linker. This test refers to a compilation unit containing
   an unrepresentable item, which tests that finding the compilation unit
   does not force a runtime representation unnecessarily. *)
let f =
  <[ fun (_ : Unrepresentable_unit.M.t option) ->
      (* This alias points to yet another compilation unit; we should mark that
         as required, too. *)
      Unrepresentable_unit.Alias.foo () ]>

let () = Printf.printf "%d\n" ((Eval.eval f) None)
