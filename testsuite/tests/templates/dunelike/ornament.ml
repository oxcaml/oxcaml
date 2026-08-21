(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
[-open-cmi fancy/fancy__.cmi] *)

type t = string

let create t = t
let to_string t = "Ornament(" ^ t ^ ")"
