(* Re-export the nested synthetic slots while the callbacks are still
   unknown. *)
module Lib = Specialise_lifted_nested_captures_lib
let[@inline] values f g n = Lib.values f g n
let[@inline] closures f g n = Lib.closures f g n
let[@inline] nested f g n = Lib.nested f g n
let[@inline] backedge f n = Lib.backedge f n
