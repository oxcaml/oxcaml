(* The unit defines its own [Foo] -- a generative functor, unlike the plain
   module [Lib__bar] that [-bind Foo Lib__bar] binds [Foo] to. The unit's
   [Foo] shadows the bound name: [Foo ()] below instantiates the functor. *)
module Foo () = struct
  let here = ()
end

module Inst = Foo ()

let _ = Inst.here
