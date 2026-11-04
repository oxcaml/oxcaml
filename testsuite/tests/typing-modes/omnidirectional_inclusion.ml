(* TEST
 readonly_files = "omnidirectional_inclusion.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension omnidirectionality";
 module = "omnidirectional_inclusion.mli";
 ocamlc.byte;
 module = "omnidirectional_inclusion.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let r = ref None
let g x = r := Some x

module type S = module type of struct
  let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
