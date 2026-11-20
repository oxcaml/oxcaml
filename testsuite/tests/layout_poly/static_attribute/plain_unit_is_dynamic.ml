(* TEST
 readonly_files = "plain_unit.ml";
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 setup-ocamlopt.byte-build-env;
 module = "plain_unit.ml";
 ocamlopt.byte;
 module = "plain_unit_is_dynamic.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Plain_unit] is [Static_unit] without the [[@@@static]]. *)

let _ = Plain_unit.id 1
