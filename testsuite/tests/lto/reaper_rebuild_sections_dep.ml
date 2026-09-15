let[@inline never] ignore_capture x _captured = x + 1

let[@inline never] make captured =
  let[@inline never] used x = ignore_capture x captured in
  used

(* The solve removes the use of [captured], so [used] no longer needs its
   closure argument. The caller must use solved, not original, metadata. *)
let used = make (Sys.opaque_identity 100)

let unused x = x * 2
