(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
[-open-cmi fancy/fancy__.cmi] *)

type t

val create : string -> t
val to_string : t -> string
