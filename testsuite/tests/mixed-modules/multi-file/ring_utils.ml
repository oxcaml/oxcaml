let pow a b =
  let mutable r = Ring.one in
  for i = 1 to b do
    r <- Ring.mul r a
  done;
  r
