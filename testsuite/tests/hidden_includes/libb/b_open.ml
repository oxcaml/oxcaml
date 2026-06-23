(* Compiled with [-open-cmi liba/a.cmi]: bare [t] and [x] should resolve
   to [A.t] / [A.x] without [A] being on the include path. *)
let y : t = x
