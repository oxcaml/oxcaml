(* TEST
 hasunix;
 {
   include unix;
   toplevel;
 }{
   directories = "${ocamlsrcdir}/otherlibs/unix";
   toplevel.opt;
 }
*)

(* No backtrace, whatever OCAMLRUNPARAM says. *)
Printexc.record_backtrace false;;

(* The toplevel survives an interrupt of a phrase, and forgets the phrase. *)
let interrupted = Unix.kill (Unix.getpid ()) Sys.sigint;;

print_endline "still running";;
interrupted;;
