(* [Collide.Foo] is the unit's own generative functor, so it can be applied.
   Were it [Lib__bar] (a plain module), the application below would be
   rejected. *)
module I = Collide.Foo ()

let _ = I.here
