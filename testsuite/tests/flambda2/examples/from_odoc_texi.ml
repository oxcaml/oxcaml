(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

let ( ! ) { contents } = contents

let esc_8bits = ref false

external opaque : 'a -> 'a = "%opaque"

let[@inline never] foo x = opaque x

let y = if !esc_8bits then [foo "a", "b"] else []
