(* TEST
 readonly_files = "unloaded.mli unloaded.ml defs.mli defs.ml";
 flags = "-extension layouts_beta";
 setup-ocamlc.byte-build-env;
 module = "unloaded.mli";
 ocamlc.byte;
 module = "unloaded.ml";
 ocamlc.byte;
 module = "defs.mli";
 ocamlc.byte;
 module = "defs.ml";
 ocamlc.byte;
 module = "test_unboxed_version_ok.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Same as [test_unboxed_version.ml] but without [-short-paths], which was
   needed to trigger the bug there. *)
let _f : Defs.z = ""
