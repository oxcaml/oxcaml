let[@local never] [@inline never] tail_call_me n m x =
  Format.printf "tail_call_me: %d %d %f\n" n m x

let[@local never] [@inline never] do_nothing () = ()
