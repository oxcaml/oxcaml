type state = { mutable used : unit }

let state = { used = () }

(* Shadowed: the first [v] writes a legacy toplevel, so only pairing with the last
   definition can suggest [@@ stateless]. *)
let v x =
  state.used <- ();
  x
;;

let _ = v
let v x = x
