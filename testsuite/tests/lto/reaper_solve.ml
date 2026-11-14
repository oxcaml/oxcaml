(* TEST
 modules = "reaper_solve_dependency.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -lto-support";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_solve_dependency.cmx reaper_solve.cmx";
 last_flags = "-o reaper_solve.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_solve.ltosol";
 file-exists;

 flags = "-reaper-solve reaper_solve.cmx reaper_solve.cmx";
 last_flags = "-o reaper_solve_duplicate.ltosol";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 ocamlopt_opt_exit_status = "0";

 flags = "-reaper-solve reaper_solve.cmx";
 last_flags = "-o reaper_solve_partial.ltosol";
 ocamlopt.opt;

 file = "reaper_solve_partial.ltosol";
 file-exists;

 check-ocamlopt.opt-output;
*)

(* A solve over both units and a partial solve over the caller alone each
   produce a solution file. *)

let () = assert (Reaper_solve_dependency.used 41 = 42)
