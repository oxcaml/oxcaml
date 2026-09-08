(* [used] is called by the main unit, so its symbols must stay global in the
   rebuilt object file. [internal] is only called from within this unit, so its
   symbols can be demoted to local. *)
let[@inline never] internal x = x * 2

let[@inline never] used x = internal x + 2
