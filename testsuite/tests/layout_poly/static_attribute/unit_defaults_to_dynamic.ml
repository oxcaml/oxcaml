(* TEST
 readonly_files = "plain_unit.ml";
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 setup-ocamlopt.byte-build-env;
 module = "plain_unit.ml";
 ocamlopt.byte;
 module = "unit_defaults_to_dynamic.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let _ = Plain_unit.id 1
