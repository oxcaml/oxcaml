(* The unit defines its own [Foo] -- a generative functor, unlike the plain
   module [Lib__bar] that [-expose Lib__bar Foo] exposes. The unit's [Foo]
   shadows the exposed name: [Foo ()] below instantiates the functor. *)
module Foo () = struct
  let here = ()
end

module Inst = Foo ()

let _ = Inst.here
