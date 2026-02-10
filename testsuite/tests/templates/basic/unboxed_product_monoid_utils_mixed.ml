let one = Unboxed_product_monoid.empty

let pow b e =
  let mutable r = one in
  for i = 1 to e do
    r <- Unboxed_product_monoid.append r b
  done;
  r
;;

let square b = pow b 2
