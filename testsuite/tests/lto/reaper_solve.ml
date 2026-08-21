(* TEST
 modules = "reaper_solve_dependency.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 file = "reaper_solve_dependency.cmr";
 file-exists;

 file = "reaper_solve.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-solve reaper_solve_dependency.cmr reaper_solve.cmr";
 last_flags = "-o reaper_solve.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_solve.ltosol";
 file-exists;

 check-ocamlopt.opt-output;
*)

(* CR mvellacott: currently reaper_lto_solve just prints the unit names it was
   given, when the solve stage is implemented this test will need updating. *)

let () = assert (Reaper_solve_dependency.used 41 = 42)
