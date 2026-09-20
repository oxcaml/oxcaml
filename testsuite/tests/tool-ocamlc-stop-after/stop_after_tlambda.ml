(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-dtlambda -stop-after tlambda -nopervasives -warn-error +53";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Stopping after tlambda dumps the tlambda and still issues warning 53 (the
   attribute checks happen during translation), but goes no further. *)

external p : int -> unit = ""
let () = p 1
let[@inlined] f x = x
