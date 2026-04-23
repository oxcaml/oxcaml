module R1 = Bundle.Func(P_int)()
module R2 = Bundle.Func(P_int)()

let () =
  let b1 = R1.Basic.create 42 in
  let u1 = R1.Util.create () in
  print_endline (R1.Basic.to_string b1);
  print_endline (R1.Util.to_string u1)

let () =
  let b2 = R2.Basic.create 42 in
  let u2 = R2.Util.create () in
  print_endline (R2.Basic.to_string b2);
  print_endline (R2.Util.to_string u2)
