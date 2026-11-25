(* TEST
 setup-ocamlopt.byte-build-env;
 flags = "-dtlambda -dlambda -stop-after transl -nopervasives ";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Only the -dtlambda output should be printed, since compilation stops
   before slambda evaluation. *)

external p : int -> unit = ""
let () = p 1
