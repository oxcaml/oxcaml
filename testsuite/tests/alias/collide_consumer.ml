(* Without [-alias]: [Collide.Foo] must resolve to the unit's own generative
   functor -- the visible component wins over the hidden [module Foo = Bar]
   alias recorded in [collide.cmi] -- so it can be applied. Were it to resolve
   to [Bar] (a plain module), the application below would be rejected. *)
module I = Collide.Foo ()

let _ = I.here
