(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws -for-pack Packed";
 module = "gen_pack.ml";
 ocamlopt.opt;
 unset module;
 program = "packed.cmx";
 flags = "-pack";
 all_modules = "gen_pack.cmx";
 ocamlopt.opt;
 exit_status = "2";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file packed.cmx";
 script;
 check-program-output;
*)

(* A pack is not compiled from a unit and records no laws file, so asking
   for one is an error. *)

law_? trivial : 1 = 1
