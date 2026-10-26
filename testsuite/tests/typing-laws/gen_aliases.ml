(* TEST
 readonly_files = "laws_gen_Stdlib.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "laws_gen_Stdlib.ml gen_aliases.ml";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o gen_aliases_laws.ml";
 module = "gen_aliases.cmx";
 ocamlopt.opt;
 output = "${test_build_directory}/gen_aliases_laws.ml";
 reference = "${test_source_directory}/gen_aliases_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "gen_aliases_laws.ml";
 ocamlopt.opt;
*)

(* The laws file refers to units through aliases with fresh names: here,
   the alias of [Stdlib] cannot be [Laws_gen_Stdlib], which is a unit. *)

law? fresh : Laws_gen_Stdlib.x = 1
