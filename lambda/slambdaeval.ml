let do_eval slam =
  match (slam : Lambda.slambda) with
  | SLhalves { sval_comptime = _; sval_runtime = lam } -> lam
  | _ -> Misc.fatal_error "slambda eval not yet implemented"

let eval slam = Profile.record "static_eval" do_eval slam
