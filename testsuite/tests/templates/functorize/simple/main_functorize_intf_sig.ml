(* Demonstrate that [Bundle.Make(P_int)()] satisfies [Bundle.Intf(P_int).S]:
   ascribe the result with that module type and verify the bundle's modules
   are accessible. *)
module type S = Bundle.Intf(P_int).S
module R : S = Bundle.Make(P_int)()

let () =
  let b = R.Basic.create 42 in
  let u = R.Util.create () in
  print_endline (R.Basic.to_string b);
  print_endline (R.Util.to_string u)
