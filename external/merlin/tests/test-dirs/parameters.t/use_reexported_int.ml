module Reexport_int = Reexport(P)(P_int) [@jane.non_erasable.instances]

let p1 : int = Reexport_int.As_alias.create ()
let p2 : int = Reexport_int.Included.create ()
