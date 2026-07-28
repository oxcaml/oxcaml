(* Rebind the whole static module [S] at [@ static].  This succeeds only when
   [S] is loaded via [-Ix] (a cmx is guaranteed); with plain [-I], [S] is forced
   dynamic and the rebind fails. *)
module (M @ static) = S
