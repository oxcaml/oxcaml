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

(* Companion to [test_unboxed_version.ml]: identical except that the final
   unit is compiled without [-short-paths], so the genuine type error below is
   reported rather than the bogus "has no unboxed version" error. *)
let _f : Defs.z = ""
