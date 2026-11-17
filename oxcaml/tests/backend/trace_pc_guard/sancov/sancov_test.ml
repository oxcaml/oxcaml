(* Each function is on a single line, so that its line number does not depend on
   which of its probes [sancov] reports. *)

let[@inline never] hit_fn x = if x > 0 then x + 1 else x - 1

let[@inline never] missed_fn x = if x > 0 then x * 2 else x * 3

let () =
  let x = Sys.opaque_identity 1 in
  ignore (Sys.opaque_identity (hit_fn x) : int);
  if Sys.opaque_identity false
  then ignore (Sys.opaque_identity (missed_fn x) : int)
