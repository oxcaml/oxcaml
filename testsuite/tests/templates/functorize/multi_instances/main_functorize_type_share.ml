module R = Bundle_opaque.Make(P_int)()

let () =
  (* Type sharing: Fancy_opaque.t = Basic_opaque.t (declared in fancy_opaque.mli).
     This means a Fancy_opaque.t value can be used directly as a Basic_opaque.t. *)
  let u : R.Fancy_opaque.t = R.Fancy_opaque.create () in
  let b : R.Basic_opaque.t = u in
  print_endline (R.Basic_opaque.to_string b)
