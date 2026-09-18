type t = Basic.t

let create t = t
let wrap t = Basic.wrap t
let to_string t = "Fancy(" ^ Basic.to_string t ^ ")"
