module R = Bundle.Func(P_int)(Q_impl)()

let () =
  let f = R.Fancy.create 42
    (R.Fancy__Flourish.create 42)
    (R.Fancy__Ornament.create "hello")
  in
  print_endline (R.Fancy.to_string f)
