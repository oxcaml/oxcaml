(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve no_lto_info.cmx";
 last_flags = "-o no_lto_info.ltosol";
 all_modules = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;

 flags = "-reaper-rebuild no_lto_info.cmx no_lto_info.ltosol";
 last_flags = "";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* A .cmx file compiled without -lto-support has no LTO sections, so the solve
   and the rebuild must reject it. *)

let[@inline never] f x = x + 1

let () = ignore (Sys.opaque_identity (f 3) : int)
