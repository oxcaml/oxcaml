(* TEST
 readonly_files = "dep.ml check_no_dep.sh";
 compile_only = "true";
 flags = "-no-crc Dep";
 setup-ocamlopt.byte-build-env;
 all_modules = "dep.ml no_crc_native.ml";
 ocamlopt.byte;
 {
   program = "no_crc_native.cmx";
   output = "no_crc_native.objinfo";
   ocamlobjinfo;
 }{
   script = "sh ${test_source_directory}/check_no_dep.sh no_crc_native.objinfo";
   script;
 }
*)

(* Native counterpart of [no_crc.ml]: [-no-crc Dep] omits Dep's row from both
   import tables of the produced .cmx (interfaces and implementations). The
   .cmx also carries a Flambda 2 export dump, so rather than compare the whole
   ocamlobjinfo output we grep just the import tables (see check_no_dep.sh). *)

let () = ignore Dep.x
