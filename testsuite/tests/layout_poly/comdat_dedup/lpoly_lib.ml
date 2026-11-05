let[@inline never] poly_ lpoly_pair x y = #(y, x)

let[@inline never] poly_ lpoly_call_between x y =
  let keep = Sys.opaque_identity (ref 0) in
  Gc.minor ();
  incr keep;
  #(y, x)
