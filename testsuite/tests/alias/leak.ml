(* Defines its own [Foo] and [include]s [Carrier], whose .cmi carries a hidden
   [module Foo = Bar]. The included hidden alias must coexist with this unit's
   own [Foo] -- it must not clash ("Multiple definition of Foo") nor break the
   signature inclusion check. *)
module Foo = Bar
include Carrier
let _ = c
