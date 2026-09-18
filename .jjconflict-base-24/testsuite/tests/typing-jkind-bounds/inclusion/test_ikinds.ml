(* TEST
   readonly_files = "inclusion.mli inclusion.ml";
   setup-ocamlc.byte-build-env;
   module = "inclusion.mli";
   ocamlc.byte;
   module = "inclusion.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)
