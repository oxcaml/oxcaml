(* Filled in from Msupport. *)
(* Eta-expanded: OxCaml's [raise] has modes that [ref] cannot weaken. *)
let msupport_raise_error : (exn -> unit) ref = ref (fun exn -> raise exn)

let raise_error exn = !msupport_raise_error exn
