let[@inline always] inner (a : int) =
  let temp = a * 2 in
  let temp2 = temp + 1 in
  temp2

let[@inline never] caller (x : int) =
  let result = inner x in
  result + result

let () =
  let r = caller 41 in
  (Printf.printf [@inlined never]) "%d\n" r
