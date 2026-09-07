(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
   [-open-cmi fancy/fancy__.cmi] *)

type t

val create : P.t -> t
val to_string : t -> string
