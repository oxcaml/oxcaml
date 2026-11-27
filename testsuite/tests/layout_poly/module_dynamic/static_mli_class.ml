(* TEST
 flags = "-extension layout_poly_alpha";
 readonly_files = "static_mli_class.mli";
 setup-ocamlc.byte-build-env;
 module = "static_mli_class.mli";
 ocamlc.byte;
 module = "static_mli_class.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

class point = object
  val mutable x = 0
end
