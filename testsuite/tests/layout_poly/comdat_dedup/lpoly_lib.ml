let[@inline never] poly_ lpoly_pair x y = #(y, x)

let[@inline never] poly_ lpoly_call_between x y =
  let keep = Sys.opaque_identity (ref 0) in
  Gc.minor ();
  incr keep;
  #(y, x)

let[@inline never] poly_ lpoly_select n m x y =
  let k =
    match n with
    | 0 -> m + 7
    | 1 -> m * 3
    | 2 -> m lsl 4
    | 3 -> m lxor 99
    | 4 -> m - 12
    | 5 -> m * m
    | _ -> m
  in
  #(k, y, x)
