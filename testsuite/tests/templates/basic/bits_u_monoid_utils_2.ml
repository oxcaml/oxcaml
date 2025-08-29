let one = Bits_u_monoid.empty

let pow b e =
  let mutable r = one in
  for i = 1 to e do
    r <- Bits_u_monoid.append r b
  done;
  r

let square b = pow b 2
