(* TEST
 modules = "reaper_rebuild_sections_dep.ml reaper_rebuild_sections_other.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto -flambda2-result-types-all-functions";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_sections_dep.cmr reaper_rebuild_sections_other.cmr reaper_rebuild_sections.cmr";
 last_flags = "-o reaper_rebuild_sections.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_rebuild_sections.ltosol";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_sections_other.cmr reaper_rebuild_sections.ltosol -reaper-debug-flags sections";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_rebuild_sections_other.reaped.cmx";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_sections.cmr reaper_rebuild_sections.ltosol -dcmm";
 compiler_output2 = "reaper_rebuild_sections.cmm";
 ocamlopt.opt;

 file = "reaper_rebuild_sections.reaped.cmx";
 file-exists;

 script = "awk 'BEGIN { while ((getline line) > 0) text = text line; exit !(text ~ /G:.camlReaper_rebuild_sections_dep__fn[^ ]*_code.[[:space:]]+83[[:space:]]+int[)]/) }' reaper_rebuild_sections.cmm";
 script;

 compiler_output2 = "ocamlopt.opt.output";

 flags = "-reaper-rebuild reaper_rebuild_sections_dep.cmr reaper_rebuild_sections.ltosol -reaper-debug-flags sections";
 ocamlopt.opt;

 file = "reaper_rebuild_sections_dep.reaped.cmx";
 file-exists;

 flags = "";
 all_modules = "reaper_rebuild_sections_dep.reaped.cmx reaper_rebuild_sections.reaped.cmx";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
 run;
 check-program-output;
*)

(* The solution is sharded per compilation unit; each rebuild must read only
   the sections for the units it needs. The reference file checks, via the
   debug output, that rebuilding the independent unit does not read this unit's
   or the dependency's sections, and that rebuilding the dependency does not
   read the independent unit's section.

   The caller is rebuilt before the dependency, so its direct call must use
   the solved foreign code metadata without a dependency .reaped.cmx file.
   Result types expose the code id of the returned closure, whose captured
   value becomes unused during the solve. The Cmm check requires the direct
   call to pass only the tagged integer 41, without a closure argument.
   Linking and running checks that this calling convention agrees with the
   rebuilt dependency. *)

let () = assert (Reaper_rebuild_sections_dep.used 41 = 42)
