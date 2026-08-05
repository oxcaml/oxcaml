(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "helper.ml";
   setup-ocamlopt.opt-build-env;
   { all_modules = "helper.ml";
     ocamlopt_flags = " -Oclassic -no-flambda2-expert-fallback-inlining-heuristic";
     fexpr_reference_suffix = ".helper.reference";
     ocamlopt.opt with dump-raw;
     check-fexpr-dump; }
   { all_modules = "rec_info_alias.ml";
     ocamlopt.opt with dump-simplify;
     check-fexpr-dump; }
 *)

(* This test ensures that we simplify and remove [let depth x = y] bindings
   that appear in the output of the [helper] module. *)

let h y = Helper.p y
