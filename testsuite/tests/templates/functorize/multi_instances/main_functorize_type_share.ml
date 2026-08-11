module R = Bundle_opaque.Make(P_int)()

let () =
  (* Type sharing: Util_opaque.t = Basic_opaque.t (declared in util_opaque.mli).
     This means a Util_opaque.t value can be used directly as a Basic_opaque.t. *)
  let u : R.Util_opaque.t = R.Util_opaque.create () in
  let b : R.Basic_opaque.t = u in
  print_endline (R.Basic_opaque.to_string b)
