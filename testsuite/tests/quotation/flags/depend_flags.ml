(* TEST
 readonly_files = "mod_directive_on.ml mod_uses_dollar.ml";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -modules -no-syntax-quotations mod_directive_on.ml mod_uses_dollar.ml";
 ocamlc.byte;
 commandline = "-depend -modules -syntax-quotations mod_uses_dollar.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 set OCAMLPARAM = "_,syntax-quotations=0";
 commandline = "-depend -modules mod_uses_dollar.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* ocamldep honours the flags and OCAMLPARAM, and a directive in one file
   does not leak into the next. *)
