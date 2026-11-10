(* TEST
   flambda2;
   flags += "-Oclassic -flambda2-reaper -X reaper-oclassic=1 -reaper-debug-flags=nostamps";
   native with dump-reaper;
   check-fexpr-dump;
 *)

(* We must make sure to track fields of static blocks which are
   an [Or_variable]. Currently, only classic mode generates those. *)

type t = { a : float; b : float }

let v = Sys.opaque_identity 3.0

let r = { a = v +. 1.0; b = 2.0 }

let () = assert (r.a = 4.0)