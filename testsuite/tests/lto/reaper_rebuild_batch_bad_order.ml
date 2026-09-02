(* TEST
 modules = "reaper_rebuild_batch_dependency.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_batch_dependency.cmr reaper_rebuild_batch_bad_order.cmr";
 last_flags = "-o reaper_rebuild_batch_bad_order.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild reaper_rebuild_batch_bad_order.cmr reaper_rebuild_batch_dependency.cmr reaper_rebuild_batch_bad_order.ltosol";
 last_flags = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
*)

(* A batched -reaper-rebuild invocation whose .cmr files are not in dependency
   order must be rejected. *)

let () = assert (Reaper_rebuild_batch_dependency.used 41 = 42)
