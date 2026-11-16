(* TEST
 readonly_files = "hidden_type.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "hidden_type.mli";
 ocamlopt.opt;
 module = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The laws file is compiled against the interface of the unit, so a law
   that refers to a type the interface does not export is rejected. *)

type t = int
let f x = x
law_? about_t (x : t) : f x = x
