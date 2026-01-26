let to_float = Multi_ret.Float_u.to_float

let () =
  let #(a, b, c, d) = Multi_ret.permute #1.0 #2.0 #3.0 #4.0 in
  Format.printf "%f %f %f %f\n"
    (to_float a)
    (to_float b)
    (to_float c)
    (to_float d)
