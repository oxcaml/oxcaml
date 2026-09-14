(* TEST
 flambda2;
 readonly_files = "pack_rejected_member.ml";
 setup-ocamlopt.opt-build-env;

 flags = "-for-pack Pack_rejected_pack -flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "pack_rejected_member.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-pack";
 program = "pack_rejected_pack.cmx";
 all_modules = "pack_rejected_member.cmx";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;

 flags = "-pack -support-lto";
 ocamlopt.opt;

 check-ocamlopt.opt-output;
*)

(* Packing drops the LTO sections of the members, so -pack rejects members
   compiled with -support-lto, and -support-lto itself. This file is only here
   to carry the test description. *)
