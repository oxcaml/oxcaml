(* TEST
 stack-allocation;
 flambda2;
 setup-ocamlopt.byte-build-env;
 unset OCAMLPARAM;
 ocamlopt.byte with dump-raw;
 check-fexpr-dump;
*)

let f : float @ local -> unit = fun (_x[@unboxable]) -> ()
let[@unboxable] g : float @ local -> float = fun (_x[@unboxable]) -> 0.
