let[@inline always] add_one (x : int) = x + 1

let[@inline never] f y =
  let z = add_one y in
  z * 2

let () =
  let r = f 41 in
  Printf.printf "%d\n" r
