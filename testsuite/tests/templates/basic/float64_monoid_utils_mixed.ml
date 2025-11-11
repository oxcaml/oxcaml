let one = Float64_monoid.empty

let pow b e =
  let mutable r = one in
  for i = 1 to e do
    r <- Float64_monoid.append r b
  done;
  r

let square b = pow b 2
