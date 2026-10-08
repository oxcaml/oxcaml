(* Filled in from Msupport. *)
let msupport_raise_error : (exn -> unit) ref = ref (fun exn -> raise exn)

let raise_error exn = !msupport_raise_error exn
