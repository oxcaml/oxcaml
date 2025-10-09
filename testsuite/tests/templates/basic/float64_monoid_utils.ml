let pow b e =
  let mutable r = Float64_monoid.empty in
  for i = 1 to e do
    r <- Float64_monoid.append r b
  done;
  r
