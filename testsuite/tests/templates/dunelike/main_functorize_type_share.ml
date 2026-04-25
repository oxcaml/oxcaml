module R = Bundle_share.Func(P_int)()

let () =
  (* Type sharing: Util_share.t = Basic_share.t (declared in util_share.mli).
     This means a Util_share.t value can be used directly as a Basic_share.t. *)
  let u : R.Util_share.t = R.Util_share.create () in
  let b : R.Basic_share.t = u in
  print_endline (R.Basic_share.to_string b)

module R1 = Bundle_share.Func(P_int)()
module R2 = Bundle_share.Func(P_int)()

let () =
  (* Each application gets a fresh counter, independent from the others.
     (R already incremented its own counter once above via Util_share.create.) *)
  assert (R1.Basic_share.get_count () = 0);
  assert (R2.Basic_share.get_count () = 0);
  ignore (R1.Basic_share.create 1);
  ignore (R1.Basic_share.create 2);
  assert (R1.Basic_share.get_count () = 2);
  assert (R2.Basic_share.get_count () = 0);
  ignore (R2.Basic_share.create 10);
  assert (R2.Basic_share.get_count () = 1);
  assert (R1.Basic_share.get_count () = 2);
  print_endline "OK"
