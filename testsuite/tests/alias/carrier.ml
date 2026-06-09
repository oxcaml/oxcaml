(* Compiled with [-alias Foo Bar]: its .cmi records a hidden [module Foo = Bar]
   even though [Foo] is unused here. A unit that [include]s [Carrier] then
   inherits that hidden alias. *)
let c = ()
