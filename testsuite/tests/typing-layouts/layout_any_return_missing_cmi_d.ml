(* Compiled without layout_any_return_missing_cmi_m.cmi: the return type of [f] has
   layout [any] here (its cmi is missing), so [f] is classified as a
   forwarder and compiled with the unknown-result convention. *)
let[@inline never] f () = Layout_any_return_missing_cmi_b.mk ()
