(* Parameters: P *)
(* Transparent version of basic: type t = P.t is visible (no .mli) *)

type t = P.t

let create (t : t) = t
let to_string t = P.to_string t
