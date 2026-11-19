(* TEST
 readonly_files = "mli_with_static_default.mli";
 flags = "-extension layout_poly_alpha";
 setup-ocamlopt.byte-build-env;
 module = "mli_with_static_default.mli";
 ocamlopt.byte;
 module = "missing_static_data.ml";
 flags += " -nocwd -Ix . -w -58";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Even a runtime-only use must report missing static data at the reference. *)
let runtime_only =
  Mli_with_static_default.foo + 1
