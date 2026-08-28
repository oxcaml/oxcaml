(* TEST
 readonly_files = "unused_export_dep.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "unused_export_dep.ml";
 ocamlopt.opt;

 all_modules = "unused_export.ml";
 ocamlopt.opt;

 script = "sh ${test_source_directory}/check-has.sh MARKER_OF_DEAD_EXPORT unused_export_dep.o";
 script;

 compile_only = "false";
 flags = "-reaper-solve unused_export_dep.cmr unused_export.cmr";
 last_flags = "-o unused_export.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild unused_export_dep.cmr unused_export.ltosol";
 last_flags = "-o unused_export_dep.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild unused_export.cmr unused_export.ltosol";
 last_flags = "-o unused_export.reaped.cmx";
 ocamlopt.opt;

 script = "sh ${test_source_directory}/check-no.sh MARKER_OF_DEAD_EXPORT unused_export_dep.reaped.o";
 script;

 script = "sh ${test_source_directory}/check-has.sh MARKER_OF_USED_EXPORT unused_export_dep.reaped.o";
 script;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/unused_export.exe";
 all_modules = "unused_export_dep.reaped.cmx unused_export.reaped.cmx";
 ocamlopt.opt;

 script = "sh ${test_source_directory}/check-no.sh MARKER_OF_DEAD_EXPORT unused_export.exe";
 script;

 run;
 check-program-output;
*)

(* Checks that the whole-program Reaper removes definitions that are exposed by
   a module but unused anywhere in the program. [unused_export_dep.ml] exposes
   [used] (called below) and [unused_export] (never used): after the solve and
   rebuild, the dead function's marker string must be gone from the rebuilt
   object file and from the linked executable, while the used function's marker
   must remain. *)

let () = print_endline (Unused_export_dep.used (Sys.opaque_identity "call"))
