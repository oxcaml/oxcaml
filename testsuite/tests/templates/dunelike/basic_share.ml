(* Parameters: P *)

type t = P.t

let count = ref 0

let create t = incr count; t

let to_string t = P.to_string t

let get_count () = !count
