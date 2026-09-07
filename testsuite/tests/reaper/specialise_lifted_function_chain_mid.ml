(* [f] is unknown here, so [my_map] must keep what is needed to specialise the
   lifted [loop] in its caller. *)

let[@inline] my_map (f @ local) (l @ local) = exclave_
  Specialise_lifted_function_lib.map_stack f l
