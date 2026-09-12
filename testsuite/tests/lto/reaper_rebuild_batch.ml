(* TEST
 modules = "reaper_rebuild_batch_dependency.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 file = "reaper_rebuild_batch_dependency.cmr";
 file-exists;

 file = "reaper_rebuild_batch.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_batch_dependency.cmr reaper_rebuild_batch.cmr";
 last_flags = "-o reaper_rebuild_batch.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "reaper_rebuild_batch.ltosol";
 file-exists;

 flags = "-reaper-rebuild reaper_rebuild_batch_dependency.cmr reaper_rebuild_batch.cmr reaper_rebuild_batch.ltosol";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_rebuild_batch_dependency.reaped.cmx";
 file-exists;

 file = "reaper_rebuild_batch_dependency.reaped.o";
 file-exists;

 file = "reaper_rebuild_batch.reaped.cmx";
 file-exists;

 file = "reaper_rebuild_batch.reaped.o";
 file-exists;

 check-ocamlopt.opt-output;
*)

(* Rebuilding two independent units with a single batched -reaper-rebuild
   invocation must behave like separate invocations: the .ltosol file is loaded
   once and every member gets its own .reaped.cmx/.reaped.o pair. *)

let[@inline never] used x = x + 1

let unused x = x * 2

let () = assert (used 41 = 42)
