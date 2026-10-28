(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws -for-pack Packed";
 module = "gen_pack.ml";
 ocamlopt.opt;
 unset module;
 program = "packed.cmx";
 flags = "-pack";
 all_modules = "gen_pack.cmx";
 ocamlopt.opt;
 flags = "-generate-laws-implementation";
 module = "packed.cmx";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* A pack is not compiled from a unit and records no laws file, so asking
   for one is an error. *)

law? trivial : 1 = 1
