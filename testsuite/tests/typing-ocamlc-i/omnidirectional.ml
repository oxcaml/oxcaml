(* TEST
 flags = "-i -extension omnidirectionality";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let r = ref None
let g (x @ portable) = r := Some x
let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
