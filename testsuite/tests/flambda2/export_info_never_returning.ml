(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 ocamlopt.opt;

 program = "export_info_never_returning.cmx";
 output = "export_info_never_returning.objinfo";
 ocamlobjinfo;
 (* The "Codegen fingerprint" line depends on the configure defaults;
    strip it so the reference (and the export-info regression coverage)
    holds on all configurations.  It is checked by
    tool-ocamlobjinfo/codegen_fingerprint.ml. *)
 stdout = "strip-codegen-fingerprint.stdout";
 stderr = "strip-codegen-fingerprint.stderr";
 script = "sh ${test_source_directory}/../tool-ocamlobjinfo/strip-codegen-fingerprint.sh ${output}";
 script;
 check-program-output;
*)

(* The initialiser never returns normally, but the .cmx should still carry
   Flambda export information. *)

let () = raise Exit
