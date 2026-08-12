(* part of avoid_loading_cmx.ml *)
let[@inline never] mk x =
  let[@inline never] g y = x + y in
  g
