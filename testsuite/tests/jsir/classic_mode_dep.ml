(* Paired with classic_mode.ml: a unit asking for classic mode with an
   attribute. *)

[@@@ocaml.flambda_oclassic]

exception Error of int

let counter = ref 0

let pair = Sys.opaque_identity 1, 2

let[@inline never] incr_counter () = incr counter

let[@inline never] raise_error n = raise (Error n)

let[@inline never] sum_pair () = fst pair + snd pair

let[@inline never] add x y = x + y
