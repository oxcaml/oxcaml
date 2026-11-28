(* The counter of the call of a probe handler, which To_cmm replaces by the
   probe: it is dropped (see [Fdo_dropped_counters]). *)

let[@inline never] side x = x * 3

let[@inline never] probed x =
  [%probe "fdo_test" (ignore (side x))];
  x + 1
