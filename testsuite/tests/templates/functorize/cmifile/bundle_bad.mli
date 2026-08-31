(* Incompatible declared interface: Make is a plain structure rather than
   a functor.  Using this with -cmi-file should fail the inclusion check. *)
module Make : sig val x : int end
