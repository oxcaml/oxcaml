(* TEST
 readonly_files = "unique_export_fail.ml unique_export_fail.mli";
 setup-ocamlc.byte-build-env;
 module = "unique_export_fail.mli";
 ocamlc.byte;
 module = "unique_export_fail.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A top-level binding exported by the interface is accessible to other
   compilation units, so it cannot be consumed uniquely. *)

let unique_id (x @ unique) = ignore x

let exported = "foo"

let () = unique_id exported

let use () = ()
