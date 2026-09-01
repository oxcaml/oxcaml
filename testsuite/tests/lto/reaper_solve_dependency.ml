(* Disable inlining so that rebuild of the main unit needs this unit's metadata. *)
let[@inline never] used x = x + 1

let unused x = x * 2
