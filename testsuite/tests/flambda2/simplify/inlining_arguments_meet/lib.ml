let f_with_code_size_between_1_and_10 x = x * x * x

let g x = f_with_code_size_between_1_and_10 x

let[@inline] two_arg x =
  let g_with_code_size_between_11_and_20 y = x * y * y * y * y in
  let () = () in
  g_with_code_size_between_11_and_20
