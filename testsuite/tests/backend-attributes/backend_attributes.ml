(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 flags = "-c -dcfg";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let[@regalloc irc] with_irc x = x

let[@regalloc irc][@regalloc_param "IRC_SPILLING_HEURISTICS=flat"] with_irc_and_param x = x
