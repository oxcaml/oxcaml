(* TEST_BELOW *)

[@@@ocaml.flambda_oclassic]

let[@inline never] f x = x

let[@inline always] g x = (f[@inlined always]) x

let h x = g x

(* CR mvellacott: Re-enable when we bring back classic mode. *)
(* TEST
 skip;
  flags = "-w +A-70";
  flambda2;
  native;
*)
