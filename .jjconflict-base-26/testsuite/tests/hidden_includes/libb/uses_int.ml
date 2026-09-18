(* Compiled with [-open-cmi libb/with_sub.cmi -open-cmi liba/a.cmi]: bare
   [x] resolves to [liba/a.cmi]'s [int] [x], not to the [A] sub-module that
   [with_sub.cmi] brought into scope. *)
let y : int = x
