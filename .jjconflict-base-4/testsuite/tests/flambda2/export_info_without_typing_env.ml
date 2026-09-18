(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 ocamlopt.opt;

 program = "export_info_without_typing_env.cmx";
 output = "export_info_without_typing_env.objinfo";
 ocamlobjinfo;

 output = "scripts.output";
 script = "grep -q 'Typing env: none' export_info_without_typing_env.objinfo";
 script;
*)

(* The initialiser never returns normally, so the unit has no final typing
   environment. Its .cmx file still carries Flambda export information. *)

let () = raise Exit
