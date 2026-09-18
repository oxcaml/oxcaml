(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto -opaque";
 compile_only = "true";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* The LTO sections are imported with the table of the export information,
   which -opaque omits, so the combination is rejected. *)

let[@inline never] f x = x + 1

let () = ignore (Sys.opaque_identity (f 3) : int)
