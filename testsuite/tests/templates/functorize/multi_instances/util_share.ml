(* Parameters: P, depends on Basic_share *)

type t = Basic_share.t

let create () = Basic_share.create (P.create ())

let to_string t = Basic_share.to_string t
