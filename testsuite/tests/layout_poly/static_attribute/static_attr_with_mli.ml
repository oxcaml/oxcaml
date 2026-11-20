(* TEST
 readonly_files = "static_attr_with_mli.mli";
 setup-ocamlc.byte-build-env;
 module = "static_attr_with_mli.mli";
 ocamlc.byte;
 module = "static_attr_with_mli.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This unit has an [.mli], and the [.mli] decides its staticity. *)

[@@@static]

let x = 1
