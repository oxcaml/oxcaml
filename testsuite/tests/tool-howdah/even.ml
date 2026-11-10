type t = int

let create i = if i mod 2 = 0 then Some i else None
let get i = i

law? get_is_even (x : t) : get x mod 2 = 0
