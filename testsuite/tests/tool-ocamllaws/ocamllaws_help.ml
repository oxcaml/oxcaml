(* TEST
 setup-ocamlopt.opt-build-env;
 script = "sh -c '${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws -help && echo && \
   ${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract -help'";
 script;
 check-program-output;
*)

(* The help of [ocamllaws] and of its commands. *)
