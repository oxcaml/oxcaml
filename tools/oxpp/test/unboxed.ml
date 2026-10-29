(* Unboxed types and literals are erased to their boxed counterparts. *)

type t = float#

let x = #3.0

let y = -#3l
