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
 flags = "-extension layouts_beta -short-paths";
 module = "test_unboxed_version.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Regression test for a bogus error report: the genuine type error below is
   replaced by a whole-file "Unloaded.Record.t has no unboxed version" error. *)
let _f : Defs.z = ""
