(* Compiled with [-open With_sub -open-cmi libd/aliases_a.cmi]: the
   trailing [-open-cmi] rebinding shadows the [A] sub-module of
   [With_sub], so [A.x] is [liba]'s [int] [x]. *)
let y : int = A.x
