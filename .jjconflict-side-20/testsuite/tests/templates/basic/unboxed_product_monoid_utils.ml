let pow b e =
  let mutable r = Unboxed_product_monoid.empty in
  for i = 1 to e do
    r <- Unboxed_product_monoid.append r b
  done;
  r
;;
