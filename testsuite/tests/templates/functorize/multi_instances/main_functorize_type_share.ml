module R = Bundle_share.Make(P_int)()

let () =
  (* Type sharing: Util_share.t = Basic_share.t (declared in util_share.mli).
     This means a Util_share.t value can be used directly as a Basic_share.t. *)
  let u : R.Util_share.t = R.Util_share.create () in
  let b : R.Basic_share.t = u in
  print_endline (R.Basic_share.to_string b)
