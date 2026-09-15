(* TEST
 modules = "reaper_rebuild_slices_dep.ml reaper_rebuild_slices_other.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -lto-support -flambda2-result-types-all-functions";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_slices_dep.cmx reaper_rebuild_slices_other.cmx reaper_rebuild_slices.cmx";
 last_flags = "-o reaper_rebuild_slices.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_rebuild_slices.ltosol";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_slices.cmx reaper_rebuild_slices.ltosol -dcmm";
 last_flags = "";
 compiler_output2 = "reaper_rebuild_slices.cmm";
 ocamlopt.opt;

 file = "reaper_rebuild_slices.reaped.cmx";
 file-exists;

 script = "awk 'BEGIN { while ((getline line) > 0) text = text line; exit !(text ~ /G:.camlReaper_rebuild_slices_dep__fn[^ ]*_code.[[:space:]]+83[[:space:]]+int[)]/) }' reaper_rebuild_slices.cmm";
 script;

 compiler_output2 = "ocamlopt.opt.output";

 flags = "-reaper-rebuild reaper_rebuild_slices_dep.cmx reaper_rebuild_slices.ltosol";
 ocamlopt.opt;

 file = "reaper_rebuild_slices_dep.reaped.cmx";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_slices_other.cmx reaper_rebuild_slices.ltosol";
 ocamlopt.opt;

 file = "reaper_rebuild_slices_other.reaped.cmx";
 file-exists;

 flags = "";
 all_modules = "reaper_rebuild_slices_dep.reaped.cmx reaper_rebuild_slices_other.reaped.cmx reaper_rebuild_slices.reaped.cmx";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
 run;
 check-program-output;
*)

(* Each unit is rebuilt from its own part of the solution, which must contain
   what the unit needs to know about the other units.

   The caller is rebuilt before the dependency, so its direct call must use the
   solved metadata of the dependency's code, from the caller's part of the
   solution. Result types expose the code id of the returned closure, whose
   captured value becomes unused during the solve. The Cmm check requires the
   direct call to pass only the tagged integer 41, without a closure argument.
   Linking and running checks that this calling convention agrees with the
   rebuilt dependency. *)

let () = assert (Reaper_rebuild_slices_dep.used 41 = 42)
