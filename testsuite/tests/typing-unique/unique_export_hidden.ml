(* TEST
 readonly_files = "unique_export_hidden.ml unique_export_hidden.mli";
 setup-ocamlc.byte-build-env;
 module = "unique_export_hidden.mli";
 ocamlc.byte;
 module = "unique_export_hidden.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A top-level binding hidden by the interface is not accessible to other
   compilation units, so it can be consumed uniquely within this unit. *)

let unique_id (x @ unique) = ignore x

let hidden = "foo"

let () = unique_id hidden

let use () = ()
