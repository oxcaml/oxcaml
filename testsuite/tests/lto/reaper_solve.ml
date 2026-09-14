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

 flags = "-reaper-solve reaper_solve.cmr";
 last_flags = "-o reaper_solve_partial.ltosol";
 ocamlopt.opt;

 file = "reaper_solve_partial.ltosol";
 file-exists;

 flags = "-reaper-rebuild reaper_solve.cmr reaper_solve_partial.ltosol";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_solve.reaped.cmx";
 file-exists;

 flags = "";
 all_modules = "reaper_solve_dependency.cmx reaper_solve.reaped.cmx";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
 run;
 check-program-output;
*)

(* The partial solve excludes the dependency. Rebuilding the caller in a fresh
   process must preserve the dependency's metadata from the saved CMR, so its
   non-inlined call still works when linked with the original dependency. *)

let () = assert (Reaper_solve_dependency.used 41 = 42)
