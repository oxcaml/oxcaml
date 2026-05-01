let[@inline never] make_int () = (Random.int [@inlined never]) 100

let[@inline always] inner pppp =
  let s = pppp + 1 in
  s * 2

let[@inline never] caller () =
  let result = inner (make_int ()) in
  result + 1

let () =
  let r = caller () in
  (Printf.printf [@inlined never]) "%d\n" r
