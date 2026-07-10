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

(* Test that the genuine type error below is reported under [-short-paths].
   It used to be replaced by a whole-file "Unloaded.Record.t has no unboxed
   version" error: printing the error forced the components of [Make(Arg)]
   (reachable via [type w] in defs.mli), whose well-formedness check failed
   spuriously because [unloaded.cmi] is neither loaded (this unit never
   mentions [Unloaded]) nor loadable from the printing context. *)
let _f : Defs.z = ""
