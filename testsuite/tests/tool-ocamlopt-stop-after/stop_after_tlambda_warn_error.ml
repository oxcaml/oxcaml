(* TEST
 setup-ocamlopt.byte-build-env;
 flags = "-stop-after tlambda -warn-error +53";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Attributes are consumed by the translation to tlambda, so stopping there is
   late enough for warning 53 to be issued, as with [-stop-after lambda]. *)

let[@inlined] f x = x
