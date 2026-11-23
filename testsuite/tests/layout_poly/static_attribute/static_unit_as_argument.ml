(* TEST
 readonly_files = "int_id.mli static_unit.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-extension layout_poly_alpha -as-parameter";
 module = "int_id.mli";
 ocamlopt.byte;
 flags = "-extension layout_poly_alpha -as-argument-for Int_id";
 module = "static_unit.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Int_id.id : int -> int] is an instance of the layout-polymorphic
   [Static_unit.id], so [Static_unit] must be static to be an argument for
   [Int_id]. *)
