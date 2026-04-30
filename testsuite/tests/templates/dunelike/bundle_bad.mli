(* Incompatible declared interface: Func is a plain structure rather than
   a functor.  Using this with -cmi-file should fail the inclusion check. *)
module Func : sig val x : int end
