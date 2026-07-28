(* TEST
 readonly_files = "dep.ml";
 compile_only = "true";
 flags = "-no-crc Dep";
 setup-ocamlc.byte-build-env;
 all_modules = "dep.ml no_crc.ml";
 ocamlc.byte;
 program = "no_crc.cmi";
 ocamlobjinfo;
 program = "no_crc.cmo";
 ocamlobjinfo;
 check-program-output;
*)

(* Check that [-no-crc Dep] omits Dep's row from the interface import tables of
   the produced .cmi and .cmo. Dep still appears under "Required globals": the
   flag only drops the recorded CRC, not the dependency itself. Other imports'
   CRCs are shown as all-zeros by ocamlobjinfo's own -null-crc. *)

let () = ignore Dep.x
