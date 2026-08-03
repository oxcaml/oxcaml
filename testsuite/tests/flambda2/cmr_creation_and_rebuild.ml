(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 ocamlrunparam = "b=0";

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 file = "cmr_creation_and_rebuild.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-rebuild cmr_creation_and_rebuild.cmr";
 all_modules = "";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* CR mvellacott: This tests placeholder functionality: that a CMR file is
   written and read back without error. It will be replaced as the resume path
   is implemented. *)

let () = ()
