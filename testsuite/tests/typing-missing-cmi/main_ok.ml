let f (x : C.t1) = (x : C.t2)

let g (x : C.v) = match x with A _ -> true | Z -> false

let use_any_variant x = C.use_any_variant x
let use_any_record x = C.use_any_record x
