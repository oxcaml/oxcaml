let[@inline always] inner a =
  let s = a + 1 in
  let t = s * 2 in
  let u = t - a in
  u + a

let[@inline never] caller x =
  let result = inner x in
  result + 1

let () =
  let r = caller 41 in
  (Printf.printf [@inlined never]) "%d\n" r
