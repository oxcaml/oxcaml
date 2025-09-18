(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let[@regalloc irc][@regalloc ls] duplicate x = x

let[@regalloc unknown] payload x = x

let f x =
  let[@regalloc irc] y = x + x in
  y

let[@cold][@inline never] unused x = x