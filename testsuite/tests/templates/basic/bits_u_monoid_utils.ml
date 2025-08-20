let pow b e =
  let mutable r = Bits_u_monoid.empty in
  for i = 1 to e do
    r <- Bits_u_monoid.append r b
  done;
  r
