(* Compiled with [-open A]: bare [t] and [x] should resolve to [A.t]/[A.x]
   even when [A] is on a hidden include path. *)
let y : t = x
