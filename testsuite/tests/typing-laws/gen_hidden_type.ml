(* TEST
 readonly_files = "gen_hidden_type.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_hidden_type.mli";
 ocamlopt.opt;
 module = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The types of the parameters of a law must be exported as well. *)

type t = int
let f x = x
law? about_t (x : t) : f x = x
