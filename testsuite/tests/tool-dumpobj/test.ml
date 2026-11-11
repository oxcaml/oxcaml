(* TEST
 flags = "-nopervasives";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/dumpobj -nobanners ${program}";
 script;
 check-program-output;
*)
let x = 42L
