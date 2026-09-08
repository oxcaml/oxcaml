(* TEST
 modules = "reaper_rebuild_export_dep.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_export_dep.cmr reaper_rebuild_export.cmr";
 last_flags = "-o reaper_rebuild_export.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild reaper_rebuild_export_dep.cmr reaper_rebuild_export.ltosol -reaper-debug-flags export";
 last_flags = "";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* The solve records which of a unit's symbols are referenced by other
   participants; the rebuild keeps exactly those (plus the module symbol)
   global and demotes the rest to local symbols. Only the dependency is
   rebuilt; the reference file checks its exported set, which contains [used]
   but not [internal]. *)

let () = assert (Reaper_rebuild_export_dep.used 20 = 42)
