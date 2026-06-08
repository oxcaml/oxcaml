(* The unit defines its own [Foo] -- a generative functor, unlike the plain
   module [Bar] that [-alias Foo Bar] points at. The real [Foo] must shadow the
   injected alias both in the environment ([Foo ()] below instantiates the
   functor) and in the unit's signature: previously the two same-named
   components both reached the inclusion check, which failed with "modules do
   not match" (a generative functor is not included in the alias target). *)
module Foo () = struct
  let here = ()
end

module Inst = Foo ()

let _ = Inst.here
