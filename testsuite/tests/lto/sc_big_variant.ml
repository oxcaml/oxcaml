(* Copied over rebuild_stale_cmr.ml after the solve to model a recompile that
   mints many more identifier stamps than the solved version did. *)

let[@inline never] f0 x =
  let g y = x + y in
  g

let[@inline never] f1 x =
  let g y = x - y in
  g

let[@inline never] f2 x =
  let g y = x * y in
  g

let[@inline never] f3 x =
  let g y = x + (2 * y) in
  g

let[@inline never] f4 x =
  let g y = x - (2 * y) in
  g

let[@inline never] f5 x =
  let g y = (x * 2) + y in
  g

let[@inline never] f6 x =
  let g y = (x * 2) - y in
  g

let[@inline never] f7 x =
  let g y = (x + y) * 2 in
  g

let () =
  let apply f = f (Sys.opaque_identity 1) (Sys.opaque_identity 2) in
  let total =
    apply f0 + apply f1 + apply f2 + apply f3 + apply f4 + apply f5 + apply f6
    + apply f7
  in
  ignore (Sys.opaque_identity total : int)
