(* Does not instantiate the template itself: [User1.pair] gets inlined here, so
   this unit calls [User1]'s instance directly. That call must go to the
   cohort's shared symbol, not to [User1]'s private one. *)

external box : float# -> float = "%box_float"

let call_user1 () =
  let #(a, b) = User1.pair () in
  box a +. box b
