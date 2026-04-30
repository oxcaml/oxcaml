module R = Bundle.Func(P_int)()

let () =
  let b = R.Basic.create 42 in
  let u = R.Util.create () in
  print_endline (R.Basic.to_string b);
  print_endline (R.Util.to_string u)
