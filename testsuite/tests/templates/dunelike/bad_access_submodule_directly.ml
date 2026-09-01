(* Parameters: P, Q *)

(* This is compiled as if it's in the [fancy] library, so it's compiled with
   [-open-cmi fancy/fancy__.cmi] *)

(* Be naughty and try and directly access another submodule in [Fancy] *)
type t = Fancy__Flourish.t
