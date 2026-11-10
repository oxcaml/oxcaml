type t = int

let create i = if i mod 2 = 0 then Some i else None
let get i = i
