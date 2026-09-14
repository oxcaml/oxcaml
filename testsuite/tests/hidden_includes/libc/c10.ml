(* Compiled with [-open-cmi libd/aliases.cmi] and [-H liba]: [AA] resolves
   to [A] even though [liba] is hidden, but [A] itself stays unnameable. *)
let y = A.x
