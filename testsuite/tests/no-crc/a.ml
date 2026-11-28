let f (x : B.u) = match x with B.A -> "A" | B.B s -> s

let () = print_endline (f C.a)
