let[@inline] f x = x, x + 1

let[@inline] g (a, b) = a * b+2

let test xs =
  xs |> List.rev_map f |> List.rev_map g |> List.rev

