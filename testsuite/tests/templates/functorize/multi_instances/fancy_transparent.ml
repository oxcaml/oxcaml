(* Parameters: P *)
(* Transparent version of util: type t = P.t is visible (no .mli) *)

type t = P.t

let create () = P.create ()
let to_string t = P.to_string t
