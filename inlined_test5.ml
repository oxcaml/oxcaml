let[@inline always] inner a =
  let s = a + 1 in
  let t = s * 2 in
  let u = t - a in
  u + a

let[@inline never] caller x =
  (* Pass x*3 (a computation) as argument, to prevent direct substitution *)
  let result = inner (x * 3) in
  result + 1

let () =
  let r = caller 41 in
  (Printf.printf [@inlined never]) "%d\n" r
