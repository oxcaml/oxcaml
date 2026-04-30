(* The bundle_swap CMI was generated with Util listed before Basic.
   The bundle_swap CMO was compiled with Basic listed before Util.
   The coercion must swap fields so that downstream code sees the
   correct module at each field position. *)
module R = Bundle_swap.Func(P_int)()

let () =
  let b = R.Basic.create 42 in
  let u = R.Util.create () in
  print_endline (R.Basic.to_string b);
  print_endline (R.Util.to_string u)
