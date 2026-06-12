(* Rebind the library module [S] at [@ static].  [S]'s cmi records [Static], so
   this succeeds only when [S] is loaded from a directory where a cmx is
   guaranteed to be available (i.e. via [-Ix]).  With plain [-I], [S] is forced
   to [dynamic] and the rebind fails. *)
module (M @ static) = S
