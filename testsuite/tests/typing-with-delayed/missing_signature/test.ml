(* TEST
 readonly_files = "base.mli param.mli use.ml";
 include ocamlcommon;
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 module = "base.mli";
 ocamlc.byte;
 module = "param.mli";
 flags = "-as-parameter";
 ocamlc.byte;
 script = "mkdir hidden";
 script;
 script = "mv base.cmi hidden/base.cmi";
 script;
 module = "use.ml";
 flags = "-parameter Param -H hidden";
 ocamlc.byte;
 flags = "-parameter Param";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 expect;
*)

let () =
  let cmi = Cmi_format.read_cmi "ocamlc.byte/param.cmi" in
  let signature, _ = cmi.Cmi_format.cmi_sign in
  match signature with
  | [Types.Sig_module (_, _, md, _, _)] ->
      Format.printf "%a@." Printtyp.modtype md.Types.md_type
  | _ -> failwith "Unexpected parameter signature"
;;
[%%expect {|
sig ... end
|}]
