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

 flags = "";
 last_flags = "";
 compile_only = "false";
 program = "${test_build_directory}/reaper_solve.exe";
 all_modules = "reaper_solve_dependency.reaped.cmx reaper_solve.reaped.cmx";
 ocamlopt.opt;

 run;

 check-ocamlopt.opt-output;
*)

(* Checks the solve and rebuild pipeline on a two-module program: the linked
   reaped program runs correctly. (The rebuilt objects are not compared against
   per-unit Reaper output: the whole-program solve intentionally treats unit
   boundaries more precisely, so the outputs legitimately differ. Rebuild
   determinism is checked by multi_module_rebuild.) *)

let () = assert (Reaper_solve_dependency.used 41 = 42)
