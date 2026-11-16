(* TEST
 readonly_files = "hidden_value.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "hidden_value.mli";
 ocamlopt.opt;
 module = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The laws file is compiled against the interface of the unit, so a law
   that refers to a value the interface does not export is rejected. *)

let f x = x
let g x = x + 1
law_? about_g (x : int) : g x = x + 1
