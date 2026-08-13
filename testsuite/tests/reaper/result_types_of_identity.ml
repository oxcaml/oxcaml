(* TEST
 flambda;
 readonly_files = "hidden_identity.ml";
 setup-ocamlopt.byte-build-env;
 {
   ocamlopt_flags = "-flambda2-reaper -flambda2-result-types-all-functions";
   all_modules = "hidden_identity.ml";
   ocamlopt.byte;
 }{
   ocamlopt_flags = "hidden_identity.cmx -dfexpr-annot";
   all_modules = "result_types_of_identity.ml";
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 }
 {
   ocamlopt_flags = "-flambda2-reaper -no-flambda2-result-types";
   all_modules = "hidden_identity.ml";
   ocamlopt.byte;
 }{
   ocamlopt_flags = "hidden_identity.cmx -dfexpr-annot";
   all_modules = "result_types_of_identity.ml";
   ocamlopt.byte with dump-simplify;
   fexpr_reference_suffix = "no-result-types.reference";
   check-fexpr-dump;
 }
*)

let always_0 () =
  (* The result types should be enough to get rid of the assertion. *)
  assert ((Hidden_identity.hidden_identity [@inlined never]) 0 = 0)
