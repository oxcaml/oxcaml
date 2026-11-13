(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_extension_enabled.ml";
 ocamlopt.opt;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file gen_extension_enabled.cmx";
 script;
 check-program-output;
*)

(* With the laws extension, a unit records laws data. *)

law_? trivial : 1 = 1
