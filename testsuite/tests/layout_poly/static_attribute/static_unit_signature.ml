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

(* This test just prints the inferred signature of an implementation via an
   error message, by purposefully omitting [absent] from the implementation *)
module _ : sig
  val absent : unit
end =
  Static_unit
