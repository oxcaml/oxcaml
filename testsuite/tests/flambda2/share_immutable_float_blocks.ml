(* TEST
 flambda2;
 native;
*)

(* Equal constant float records should be shared, like other structured
   constants. *)

type r =
  { x : float;
    y : float
  }

let r1 = { x = 1.0; y = 2.0 }

let r2 = { x = 1.0; y = 2.0 }

let r3 = { x = 1.0; y = 3.0 }

let () =
  assert (Sys.opaque_identity r1 == Sys.opaque_identity r2);
  assert (Sys.opaque_identity r1 != Sys.opaque_identity r3)
