(* Host-side API for the plugin: a function that raises, and a callback slot
   for the plugin to fill. *)

exception Boom of int

(* Only conditionally raising keeps callers from treating the call as never
   returning (and hence from turning it into a tail call). *)
let[@inline never] boom n = if n > 0 then raise (Boom (n * 2)) else n

let callback = ref (fun () -> print_endline "plugin did not register")

let register f = callback := f
