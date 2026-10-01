(* TEST
 readonly_files = "other.mli scopes.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "other.mli scopes.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o scopes_laws.ml";
 module = "scopes.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/scopes_laws.ml";
 reference = "${test_source_directory}/scopes_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o scopes_laws.mli";
 module = "scopes.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/scopes_laws.mli";
 reference = "${test_source_directory}/scopes_laws.mli.reference";
 check-program-output;
 check-ocamlopt.opt-output;
 flags = "-extension laws";
 module = "scopes_laws.mli scopes_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of an interface with laws in submodules, functors
   (several parameters, nested, in submodules, with parameters whose
   signatures have laws or shadow enclosing parameters), module types
   from other units, and a module alias (see scopes.mli). The law of the
   generative functor is not generated, with a warning. *)
