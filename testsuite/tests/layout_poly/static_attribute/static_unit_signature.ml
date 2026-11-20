(* TEST
 readonly_files = "static_unit.ml";
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 setup-ocamlopt.byte-build-env;
 module = "static_unit.ml";
 ocamlopt.byte;
 module = "static_unit_signature.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Static_unit] has no [.mli] and starts with [[@@@static]]. The error prints
   the interface inferred for it. *)
module _ : sig
  val absent : unit
end =
  Static_unit
