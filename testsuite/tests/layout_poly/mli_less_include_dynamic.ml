(* This unit has no [.mli], so it is saved as [static]. The items it takes from
   the dynamic [Dynamic_lib] must therefore be marked [@@ dynamic] in its
   inferred interface, while its own definitions stay static. *)

include Dynamic_lib.Builtin

let g = Dynamic_lib.h

let poly_ own x = x
