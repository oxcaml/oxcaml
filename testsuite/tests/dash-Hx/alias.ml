(* [B_alias.M] is an alias for [C], so instantiating [B_alias.M.id] looks up
   [c.cmi] directly. [C] can only be static if its cmx is guaranteed. *)
let _ = B_alias.M.id 42
