(* TEST
 readonly_files = "static_unit.ml";
 flags = "-extension layout_poly_alpha";
 setup-ocamlopt.byte-build-env;
 module = "static_unit.ml";
 ocamlopt.byte;
 flags += " -cmi-file static_unit.cmi";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Static_unit] is compiled a second time against the [.cmi] of the first
   compilation, like how a build system compiles a unit without an [.mli]. *)
