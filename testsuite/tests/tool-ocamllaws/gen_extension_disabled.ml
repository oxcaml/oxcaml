(* TEST
 setup-ocamlopt.opt-build-env;
 module = "gen_extension_disabled.ml";
 ocamlopt.opt;
 exit_status = "2";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file gen_extension_disabled.cmx";
 script;
 check-program-output;
*)

(* Without the laws extension, a unit records no laws data. *)

let x = 1
