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

(* CR mvellacott: This tests placeholder functionality: that a hello world
   message is successfully roundtripped through a CMR file. It will be
   removed. *)

let () = ()
