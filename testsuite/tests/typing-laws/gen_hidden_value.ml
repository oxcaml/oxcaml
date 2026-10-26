(* TEST
 readonly_files = "gen_hidden_value.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_hidden_value.mli";
 ocamlopt.opt;
 module = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The laws file is compiled against the interface of the unit, so a law
   that refers to a value the interface does not export is rejected, even
   when the interface does not state the law. *)

let f x = x
let g x = x + 1
law? about_g (x : int) : g x = x + 1
