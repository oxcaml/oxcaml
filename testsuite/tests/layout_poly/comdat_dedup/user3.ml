(* Does not instantiate the template itself: [User1.pair] gets inlined here, so
   this unit calls [User1]'s instance directly. That call must go to the
   cohort's shared symbol, not to [User1]'s private one. The same goes for
   [User1.sole], except that no other unit has a copy of that instance to mask
   a wrong symbol. *)

external box : float# -> float = "%box_float"

external box_int64 : int64_u -> int64 = "%box_int64"

let call_user1 () =
  let #(a, b) = User1.pair () in
  box a +. box b

let call_sole () =
  let #(a, b) = User1.sole () in
  Int64.to_float (box_int64 a) +. box b
