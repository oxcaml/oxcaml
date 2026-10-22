(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 ocamlopt.opt;

 program = "export_info_never_returning.cmx";
 output = "export_info_never_returning.objinfo";
 ocamlobjinfo;
 check-program-output;
*)

(* The initialiser never returns normally, but the .cmx should still carry
   Flambda export information. *)

let () = raise Exit
