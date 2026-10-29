(* TEST
 flags = "-w -a";
 split [
 | setup-ocamlc.byte-build-env;
   ocamlc.byte;
 | setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
 ]
 exit_status = "2";
 run;
 hasunix;
 {
   not-target-windows;
   reference = "${test_source_directory}/syserror.unix.reference";
   check-program-output;
 }{
   target-windows;
   reference = "${test_source_directory}/syserror.win32.reference";
   check-program-output;
 }
*)

let _ = Printexc.record_backtrace false

let channel = open_out "titi:/toto"
