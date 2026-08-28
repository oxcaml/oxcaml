(* TEST
 readonly_files = "sc_big_variant.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "rebuild_stale_cmr.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve rebuild_stale_cmr.cmr";
 last_flags = "-o rebuild_stale_cmr.ltosol";
 all_modules = "";
 ocamlopt.opt;

 script = "cp --remove-destination sc_big_variant.ml rebuild_stale_cmr.ml";
 script;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "rebuild_stale_cmr.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-rebuild rebuild_stale_cmr.cmr rebuild_stale_cmr.ltosol";
 last_flags = "-o rebuild_stale_cmr.reaped.cmx";
 all_modules = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* A unit recompiled after the whole-program solve has a .cmr whose identifier
   stamp counters can exceed the .ltosol's; rebuilding it against the stale
   solution must fail rather than risk stamp collisions. This file is
   replaced by sc_big_variant.ml (which mints many more stamps) between the
   solve and the rebuild. The copy uses --remove-destination because ocamltest
   materialises source files as symlinks: copying through the link would
   clobber the original test file. *)

let () = ignore (Sys.opaque_identity 0 : int)
