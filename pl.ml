[@@@ocaml.flambda_o3]

let f x y =
  let p' = x, x in
  let p = p', y in
  let z = fst (fst p) + y in
  let z' = x * snd p in
  let d = z / z' in
  d + 42
