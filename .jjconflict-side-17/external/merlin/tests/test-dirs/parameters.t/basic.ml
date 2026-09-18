type t = P.t

let create () = P.create ()
let wrap t = t
let p t = t
let to_string t = "Basic(" ^ P.to_string t ^ ")"
