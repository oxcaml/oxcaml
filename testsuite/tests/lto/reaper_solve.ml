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

 flags = "-reaper-rebuild reaper_solve_dependency.cmr reaper_solve.ltosol";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_solve_dependency.reaped.cmx";
 file-exists;

 flags = "-reaper-rebuild reaper_solve.cmr reaper_solve.ltosol";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_solve.reaped.cmx";
 file-exists;

 flags = "-flambda2-reaper";
 last_flags = "";
 compile_only = "true";
 all_modules = "reaper_solve.ml";
 ocamlopt.opt;

 script = "cmp reaper_solve.reaped.o reaper_solve.o";
 script;

 check-ocamlopt.opt-output;
*)

let () = assert (Reaper_solve_dependency.used 41 = 42)
