(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-dtlambda -dlambda -stop-after transl -nopervasives ";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Only the -dtlambda output should be printed, since compilation stops
   before slambda evaluation. *)

external p : int -> unit = ""
let () = p 1
