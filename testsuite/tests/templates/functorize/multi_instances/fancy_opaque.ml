(* Parameters: P, depends on Basic_opaque *)

type t = Basic_opaque.t

let create () = Basic_opaque.create (P.create ())

let to_string t = Basic_opaque.to_string t
