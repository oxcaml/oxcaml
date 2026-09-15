(* Compiled with [-open-cmi libd/aliases_a.cmi -open With_sub]: the
   trailing [-open] shadows the [A] rebinding, so [A] is the [float]
   sub-module of [With_sub]. *)
let y : float = A.x
