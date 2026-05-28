let[@inline never] make_int () = (Random.int [@inlined never]) 100
let[@inline never] take_int x = x + 0

let[@inline always] inner pppp =
  let s = (take_int [@inlined never]) pppp in
  s + 1

let[@inline never] caller () =
  let result = inner (make_int ()) in
  result + 1

let () =
  let r = caller () in
  (Printf.printf [@inlined never]) "%d\n" r
