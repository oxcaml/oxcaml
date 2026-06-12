let[@inline always] add_two_things (a : int) (b : int) =
  let sum = a + b in
  sum * 3

let[@inline never] caller (x : int) (y : int) =
  let result = add_two_things x y in
  result

let () =
  let r = caller 10 20 in
  Printf.printf "%d\n" r
