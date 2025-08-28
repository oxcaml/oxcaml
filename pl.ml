[@@@ocaml.flambda_o3]

let[@inline never] f x = x + 1

let f x y =
  let x' = f (x + 1) in
  let p' = x, x in
  let p = p', y in
  let z = fst (fst p) + y in
  let z' = x' * snd p in
  let d = z / z' in
  d + 42
