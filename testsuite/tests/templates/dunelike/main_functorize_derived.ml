module R = Bundle_derived.Func(P_int)()

let () =
  let b = R.Basic.create 42 in
  let d = R.Derived.create b in
  print_endline (R.Derived.to_string d)
