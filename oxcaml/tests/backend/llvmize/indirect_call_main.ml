let () =
  let f x = x + 1 in
  let res = Indirect_call.apply f 314 in
  Format.printf "%d\n" res
