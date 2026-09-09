(* Parameters: P, depends on Basic *)

type t = Basic.t

let create b = b

let to_string b = "Derived(" ^ Basic.to_string b ^ ")"
