(* Compiled with [-I liba -open A -open-cmi libb/with_sub.cmi]: the
   trailing [-open-cmi] should shadow the earlier [-open A], so [x]
   resolves to [With_sub]'s top-level [string] [x] rather than to
   [liba/a.cmi]'s [int] [x]. *)
let y : string = x
