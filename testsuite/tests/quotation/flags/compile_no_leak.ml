(* TEST
 readonly_files = "mod_directive_on.ml mod_uses_dollar.ml";
 setup-ocamlc.byte-build-env;
 commandline = "-c -no-syntax-quotations mod_directive_on.ml mod_uses_dollar.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The directive in the first file must not leak into the second one, which
   is compiled by the same compiler process. *)
